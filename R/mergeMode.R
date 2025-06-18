#' Combine Positive and Negative Mode Results
#'
#' Merges the results from positive and negative modes into a single data frame, selecting the best scoring entry for each compound across both modes.
#'
#' @param posData Data frame of merged results from positive mode (output of \code{\link{easy_mergeMetabo}} with \code{mode="pos"}). Should contain a compound identifier column (e.g. `Compound_ID` or `Compound`) and score columns.
#' @param negData Data frame of merged results from negative mode (output of \code{easy_mergeMetabo} with \code{mode="neg"}). Must have the same format and identifiers as posData.
#' @return A combined data frame containing data from both modes. If a compound is found in both modes, only the entry with the highest Score (and highest Fragmentation_Score tie-breaker) is retained. Each row includes an identifier (e.g. Compound or Compound_ID), the polarity of origin, and all other associated data columns.
#'
#' @details This function simply binds the two data frames and then groups by the compound identifier (here assumed to be a column named `Compound_ID`). Within each group, it keeps the record with the highest `Score`. If there are ties in Score, it further filters to the highest `Fragmentation_Score`. The result is ungrouped and returned as a standard data frame.
#' @import dplyr
#' @seealso \code{\link{easy_mergeMetabo}}
#' @examples
#' \dontrun{
#' final_df <- mergeMode(pos_combined, neg_combined)
#' }
#' @export
#'
mergeMode <- function(posData, negData) {
  dplyr::bind_rows(posData, negData) |>
    dplyr::group_by(Compound_ID) |>
    dplyr::arrange(dplyr::desc(Score)) |>
    dplyr::filter(Score == max(Score, na.rm = TRUE)) |>
    dplyr::filter(Fragmentation_Score == max(Fragmentation_Score, na.rm = TRUE)) |>
    dplyr::ungroup()
}
