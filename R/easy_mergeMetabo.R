#' Merge Metabolomics Intensity and Identification Data (Single Mode)
#'
#' Merges metabolomic intensity data with identification data for a given polarity mode (positive or negative).
#' It filters identifications by a score threshold and combines them with the corresponding raw and normalized intensities.
#'
#' @param dat_intensity Data frame of intensity measurements (either positive or negative mode).
#'   This should include compound annotation columns followed by raw intensity columns and normalized intensity columns.
#'   The transition between raw and normalized sections is identified by column names "Raw.abundance" and "Normalised.abundance".
#' @param dat_identification Data frame of compound identification results for the same mode. Must contain columns like `Compound`, `Score`, `Fragmentation_Score`, `Description`, etc.
#' @param mode Character string, either `"pos"` or `"neg"`, indicating the polarity mode of the data.
#' @param score_value Numeric. The minimum MS1 Score cutoff. Only compounds with Score >= this value will be retained.
#' @param sampleInfo Data frame of sample information used to map original sample names to unique sample identifiers.
#'   It should have columns `pos.name` (for positive mode sample names), `neg.name` (for negative mode sample names), and `unique.name` (unified sample names).
#'
#' @return A data frame with merged information for the given mode. Each row corresponds to a unique compound passing the score filter, including:
#' \itemize{
#'   \item Annotation and identification details (from the first part of \code{dat_intensity} and \code{dat_identification}).
#'   \item Raw intensity values for each sample (prefixed with "Raw_").
#'   \item Normalized intensity values for each sample (prefixed with "Norm_").
#'   \item A column "Polarity" indicating the mode ("pos" or "neg").
#' }
#'
#' @details The function cleans up column names in the intensity and identification data frames, ensures numeric columns are properly typed, and then:
#'   \enumerate{
#'     \item Selects the sample name mapping relevant to the mode (using `pos.name` or `neg.name` from `sampleInfo` to map to `unique.name`).
#'     \item Splits the intensity data into annotation, raw intensities, and normalized intensities sections.
#'     \item Renames the raw and normalized intensity columns to "Raw_[UniqueName]" and "Norm_[UniqueName]" using the `unique.name` from sampleInfo.
#'     \item Filters the identification data to keep only entries with Score above the cutoff and selects the top hit for each compound (and each description) based on the Score.
#'     \item Merges the filtered identification data with the intensity data by the Compound identifier.
#'     \item Sorts the merged entries by Fragmentation_Score (descending) and tags each with the polarity.
#'   }
#' The result is a combined table of identification and intensity for the given mode.
#'
#' @import dplyr
#' @examples
#' \dontrun{
#' pos_combined <- easy_mergeMetabo(pos_intensity_df, pos_id_df, mode = "pos", score_value = 0.5, sampleInfo_df)
#' }
#' @export
#'
easy_mergeMetabo <- function(dat_intensity, dat_identification, mode, score_value, sampleInfo) {
  # Choose the appropriate sample name columns based on mode
  selectedCols <- if (mode == "pos") c("pos.name", "unique.name") else c("neg.name", "unique.name")
  sample_names <- sampleInfo |> dplyr::select(dplyr::all_of(selectedCols))
  colnames(sample_names) <- c("old.name", "Unique.name")

  # Identify where raw and normalized intensity sections start
  non_empty_indices <- which(colnames(dat_intensity) %in% c("Normalised.abundance", "Raw.abundance"))
  # Separate annotation vs intensity data
  annotationInfo <- dat_intensity[, 1:(non_empty_indices[1] - 1)]
  # Clean up annotation column names
  oldnames <- annotationInfo[2, ]
  new_colnames <- make.names(oldnames, unique = TRUE)
  new_colnames <- gsub("\\.+", "_", new_colnames)
  new_colnames <- gsub("_$", "", new_colnames)
  colnames(annotationInfo) <- new_colnames
  # Remove the first two rows (which might be header info) from annotation to get actual data rows
  annotationInfo <- annotationInfo[3:nrow(annotationInfo), ]

  # Extract raw intensity data and rename columns
  dat_intensity_raw=dat_intensity[,non_empty_indices[1]:(non_empty_indices[2]-1), drop = FALSE]
  #dat_intensity_raw <- dat_intensity[, non_empty_indices[1]:(non_empty_indices[2] - 1)]
  colnames(dat_intensity_raw) <- dat_intensity_raw[2, ]   # second row contains sample names
 # dat_intensity_raw <- dat_intensity_raw[3:nrow(dat_intensity_raw), ]
  dat_intensity_raw=dat_intensity_raw[3:nrow(dat_intensity_raw),, drop = FALSE]
  dat_intensity_raw <- dat_intensity_raw |> dplyr::select(dplyr::all_of(sample_names$old.name))
  colnames(dat_intensity_raw) <- paste0("Norm_", sample_names$Unique.name)
  # Convert all raw intensities to numeric
  dat_intensity_raw[] <- lapply(dat_intensity_raw, as.numeric)

  # Extract normalized intensity data and rename columns
  # (Assumes the normalized intensity section has same number of columns as raw intensity section)
  #dat_intensity_norm <- dat_intensity[, non_empty_indices[2]:(non_empty_indices[2] + length(non_empty_indices[1]:(non_empty_indices[2] - 1)) - 1)]
  dat_intensity_norm=dat_intensity[,non_empty_indices[2]:(non_empty_indices[2]+(length(non_empty_indices[1]:(non_empty_indices[2]-1))-1)), drop = FALSE]
  colnames(dat_intensity_norm) <- dat_intensity_norm[2, ]
#  dat_intensity_norm <- dat_intensity_norm[3:nrow(dat_intensity_norm), ]
  dat_intensity_norm=dat_intensity_norm[3:nrow(dat_intensity_norm),, drop = FALSE]
  dat_intensity_norm <- dat_intensity_norm |> dplyr::select(dplyr::all_of(sample_names$old.name))
  colnames(dat_intensity_norm) <- paste0("Raw_", sample_names$Unique.name)
  # Convert all normalized intensities to numeric
  dat_intensity_norm[] <- lapply(dat_intensity_norm, as.numeric)

  # Combine annotation with raw and normalized intensity data
  dat_all <- cbind(annotationInfo, dat_intensity_raw, dat_intensity_norm)
  # Drop any columns that are completely empty (all NA or blank)
  dat_all <- dat_all |> dplyr::select(dplyr::where(~ any(!is.na(.) & . != "")))

  # Clean up identification data column names and ensure Score fields are numeric
  colnames(dat_identification) <- gsub("_$", "", gsub("\\.+", "_", make.names(colnames(dat_identification), unique = TRUE)))
  dat_identification$Score <- as.numeric(dat_identification$Score)
  dat_identification$Fragmentation_Score <- as.numeric(dat_identification$Fragmentation_Score)

  # Filter and select top identification results based on Score (and Description)
  FinalCompounds <- dat_identification |>
    dplyr::arrange(-Score) |>
    dplyr::filter(Score >= score_value) |>
    dplyr::distinct(Compound, .keep_all = TRUE) |>
    dplyr::distinct(Description, .keep_all = TRUE)

  # Identify common columns (other than Compound) to avoid duplicating them in the merge
  common_names <- setdiff(intersect(colnames(FinalCompounds), colnames(dat_all)), "Compound")

  # Merge identification with intensity data on Compound, remove duplicate common columns
  mergedData <- dplyr::left_join(
    FinalCompounds,
    dat_all |> dplyr::select(-dplyr::all_of(common_names)),
    by = "Compound"
  ) |>
    dplyr::arrange(dplyr::desc(Fragmentation_Score)) |>
    dplyr::mutate(Polarity = mode) |>
    dplyr::select(Compound, Polarity, dplyr::everything())

  return(mergedData)
}
