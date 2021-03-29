df_to_factor <- function(df) {
  fct_df <- df
  cols <- names(fct_df)
  fct_df[cols] <- lapply(fct_df[cols], factor)
  df_factored <- fct_df[cols]
  df_factored <- as.data.frame(df_factored)
  return(df_factored)
}
