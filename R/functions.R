#' colclean
#'
#' This function makes variable names nicer, in one shot
#' @param tib dataframe
#' @return a dataframe with cleaned up variable names
#' @export
#' @examples

colclean=function(tib){
  sub=tib
  sub=rename_all(sub,~str_replace_all(., "[[:punct:] ]+", "_"))
  sub=rename_all(sub,~str_replace_all(., "_{2,}", "_"))
  sub=rename_all(sub,tolower)
  sub
}
