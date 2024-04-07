#' Evaluate String as R Code
#'
#' This function takes a string containing R code and evaluates it.
#' @param str A string containing R code to be evaluated.
#' @return The result of evaluating the R code contained in the string.
#' @export
#' @examples
#' do("2 + 2")
#' do("mean(c(1, 2, 3, 4, 5))")
do <- function(str) {
  eval(parse(text=str))
}
