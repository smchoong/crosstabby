get_fct_info <- function(df_factored, row.vars) {
  names<-row.vars
  levels<-df_factored %>%
    select(all_of(row.vars)) %>%
    hardhat::get_levels() %>% as.vector()
  fct_info <- list(names, levels)
  return(fct_info)
}
