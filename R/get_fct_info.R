#' Get names and levels of all variables in a factored data.frame
#'
#' @param df_factored A \code{data.frame} where all variables have been transformed as factors.
#' @param row.vars A \code{character} vector
#'
#' @return A \code{list} of the names and levels of variables in row.vars
#'
#'
#' @import hardhat
#' @export
#'
#' @examples
get_fct_info <- function(df_factored, row.vars) {
  names<-row.vars
  levels<-df_factored %>%
    select(all_of(row.vars)) %>%
    hardhat::get_levels() %>% as.vector()
  fct_info <- list(names, levels)
  return(fct_info)
}
