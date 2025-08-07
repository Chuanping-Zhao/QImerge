devtools::document()
devtools::check()
devtools::load_all()

remove.packages("QImerge")

if(!require("devtools")){install.packages("devtools")}
devtools::install_github("Chuanping-Zhao/QImerge")
QImerge::run_metabo_app()
