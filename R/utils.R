#' Hydrolyze DataFrame Columns to Global Environment
#'
#' This function takes a dataframe and assigns each column as a vector to the global environment.
#' @param df A dataframe to hydrolyze.
#' @export
hydrolyze <- function(df) {
  for (c in colnames(df)) {
    assign(c, as.vector(as.matrix(df[, c])), envir = globalenv())
  }
}

#' Generate Regression Strings from DataFrame
#'
#' This function generates a set of regression formula strings from a given dataframe. 
#' It's configured to handle dataframes with multiple Y (dependent variables) and X (independent variables).
#' @param df Dataframe containing the variables.
#' @param y_max_ind The index of the last Y variable in the dataframe.
#' @param usual Boolean to toggle the return format.
#' @return A character vector of regression formula strings.
#' @export
#' @importFrom tidyr unite
#' @importFrom stringr str_ends
regstr <- function(df, y_max_ind = 1, usual = TRUE) {
  x_ind <- (y_max_ind + 1):ncol(df)
  base2 <- c("")
  y_vec <- colnames(df[, 1:y_max_ind])
  x_n <- df[, x_ind]
  for (y in y_vec) {
    base <- c("")
    for (i in 1:length(x_n)) {
      v <- combn(colnames(x_n), m = i) %>% t() %>% as_tibble() %>% unite('x', 1:i, sep = " + ") %>% .$x
      base <- c(base, v)
    }
    reg_set <- paste0(y, " ~ ", base)
    base2 <- c(base2, reg_set)
  }
  basic <- base2[base2 != "" & !str_ends(base2, "~ ")]
  if (usual) { paste0("reg_df(lm(", basic, "))") } else { basic }
}

#' Generate Basic Regression Formulas
#'
#' This function generates basic regression formulas from a given dataframe. 
#' It focuses on creating one predictor at a time regression models.
#' @param df The dataframe from which to generate regressions.
#' @param ycount The number of Y (dependent) variables in the dataframe.
#' @return A character vector of regression calls.
#' @export
#' @importFrom tidyr unite
vanillaRegs <- function(df, ycount = 1) {
  x_ind <- (ycount + 1):ncol(df)
  base2 <- c("")
  y_vec <- colnames(df[, 1:ycount])
  x_n <- df[, x_ind]
  for (y in y_vec) {
    base <- c("")
    for (i in 1:length(x_n)) {
      v <- combn(colnames(x_n), m = i) %>% t() %>% as_tibble() %>% unite('x', 1:i, sep = " + ") %>% .$x
      base <- c(base, v)
    }
    reg_set <- paste0(y, " ~ ", base)
    base2 <- c(base2, reg_set)
  }
  basic <- base2[base2 != "" & !str_ends(base2, "~ ")]
  paste0("reg_df(lm(", basic, "))")
}

#' Combine Regression Results
#'
#' This function takes a vector of regression models and combines their results.
#' @param regs_vec A character vector of regression calls.
#' @return A combined dataframe of regression results.
#' @export
#' @importFrom dplyr bind_rows
mashAvocado <- function(regs_vec) {
  base <- do(regs_vec[1])
  for (r in regs_vec[2:length(regs_vec)]) {
    base <- bind_rows(base, do(r))
  }
  base
}
