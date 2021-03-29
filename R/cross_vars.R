cross_vars <- function(df, var1, var2) {
  var1_nm <- names(df$var1)
  var2_nm <- names(df$var2)
  df$combined <- paste(df$var1, df$var2, sep = " | ")
  names(df$combined) <- paste(var1_nm, var2_nm, sep = " | ")
}
