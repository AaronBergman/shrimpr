#' Read Data from CSV and Return as Tibble
#'
#' This function reads a CSV file into R and returns it as a tibble. 
#' It expects the filename without the .csv extension.
#' for reading the file, suppressing any messages generated during the read process.
#'
#' @param str The name of the file (without the .csv extension) to be read.
#' @return A tibble containing the data from the CSV file.
#' @export
read <- function(str){
  helper <- function(str){
    suppressMessages(eval(call("fread", paste0(str, '.csv', sep = ''))))
  }
  as_tibble(helper(str))
}

#' Write Data Frame or Text to File
#'
#' This function writes a data frame to a CSV file or any other object to a text file. 
#' The name of the output file is automatically generated based on the name of the object in R. 
#' If the object is a data frame, it is saved as a CSV file. Otherwise, it is saved as a text file.
#'
#' @param df The data frame or object to be written to a file. Can be NULL.
#' @return None.
#' @export
write <- function(df = NULL){
  name_of_df <- substitute(df)
  local_copy <- get(as.character(name_of_df), envir = globalenv())
  if(is.data.frame(df)) {
    write_csv(local_copy, paste0(name_of_df, '.csv'))
  } else {
    write_file(local_copy, paste0(name_of_df, '.txt'))
  }
}
