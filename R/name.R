#' Get Name of the Thing
#'
#' This function returns the name of the variable (or expression) passed to it as a string.
#' @param thing A variable or expression whose name you want to retrieve.
#' @return A character string representing the name of the variable or expression passed to it.
#' @export
name <- function(thing) {
  as.character(substitute(thing))
}
