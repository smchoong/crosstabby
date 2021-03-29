#' Get number of levels of factor variables in data.frame
#'
#' @param df A \code{data.frame}
#' @param input1 A \code{character} vector
#' @param input2 An optional \code{character} vector
#'
#' @return
#' @export
#'
#' @examples
get_input_lengths <- function(df, input1, input2 = NULL) {

  fct_df_rw <- df_to_factor(df) %>% select(all_of(input1))
  fct_df_cl <- df_to_factor(df) %>% select(all_of(input2))
  rw_ln <- lapply((sapply(fct_df_rw, levels)), length)
  cl_ln <- lapply((sapply(fct_df_cl, levels)), length)

  problem_vars <- c(rw_ln > 10 & cl_ln > 10)

  problem_vars_rw <- rw_ln > 10

  problem_vars_cl <- cl_ln > 10

  return(c(problem_vars, problem_vars_rw, problem_vars_cl))

}
