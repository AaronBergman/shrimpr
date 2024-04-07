#' Open New Google Sheets and Optionally Copy Data to Clipboard
#'
#' This function opens a new Google Sheets document in the default web browser. 
#' If data is provided (and is not NA), it attempts to copy this data to the clipboard.
#' @param x Optional data to be copied to the clipboard.
#' @importFrom clipr write_clip
#' @importFrom utils browseURL
#' @export
toSheets <- function(x) {
  utils::browseURL("https://docs.google.com/spreadsheets/u/0/create")
  if (!is.na(x)) {
    clipr::write_clip(x)
  }
}
