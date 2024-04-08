#' Quietly Load a Vector of Packages
#'
#' This function takes a vector of package names and loads each package quietly, suppressing warnings and startup messages.
#' It's useful when you want to load multiple packages without cluttering the console with messages.
#' @param pvec A character vector containing the names of the packages to load.
#' @export
pkg <- function(pvec=c('codetools','forcats','foreign','ggplot2','ggtext','grid','gridtext','gtable','highr','isoband','jpeg','jsonlite','KernSmooth','knitr','labeling','lattice','magrittr','markdown','MASS','Matrix','selectr','stringi','purrr','rmarkdown','stringr','dplyr','dtplyr','haven','hms','vtable','clipr','lubridate','tidyverse','parsedate','data.table','httr','rvest','XML','xml2','htmltools','devtools','roxygen2','quanteda','quanteda.textmodels','quanteda.textstats','quanteda.textplots','readtext','tuneR','cowplot','credentials','minqa','stringi','RCurl','googledrive','fixest','dreamerr','tokenizers','xtable','webshot2','processx','glmnet','Rcpp','xopen','clipr','reticulate','plm','lmtest','speech','audio',"arcgeocoder",'shrimpr')) {
  for (p in pvec) {
    suppressWarnings(suppressPackageStartupMessages(eval(call("library", p))))
  }
}
