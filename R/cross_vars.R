#' Combine two variables for analysis by groups
#'
#' @param df A \code{data.frame}
#' @param var1 A \code{character} indicating the name of the first variable to combine
#' @param var2 A \code{character} indicating the name of the second variable to combine
#'
#' @return
#' @export
#'
#' @examples
cross_vars <- function(df, var1, var2) {

  var_pat1 <- paste("\\b", var1, "\\b", sep = "")
  var_pat2 <- paste("\\b", var2, "\\b", sep = "")

  var1_nm <- df %>% select(grep(var_pat1, names(df))) %>% names()
  var2_nm <- df %>% select(grep(var_pat2, names(df))) %>% names()
  # df$combined <- paste(df$var1, df$var2, sep = " | ")
  # new_name <- paste(var1_nm, var2_nm, sep = " | ")
  # names(df)[names(df) == "combined"] <- new_name

  return(c(var1_nm, var2_nm))
}
