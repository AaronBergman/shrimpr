#' Create and Open a New R Markdown File
#'
#' This function creates a new R Markdown (Rmd) file with an optional prefix and opens it for editing.
#' @param start An optional prefix for the filename. If not provided, the filename will just end with ".Rmd".
#' @return Invisible NULL. The function is called for its side effect of creating and opening an R Markdown file.
#' @export
newRmd <- function(start = '') {
  filename <- paste0(start, '.Rmd')
  file.create(filename)
  file.edit(filename)
}
