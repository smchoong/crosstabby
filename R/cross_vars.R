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
#' # Add new combined variable to data.frame
#' library(dplyr)
#' nationscape_2020_excerpt <- nationscape_2020_excerpt %>% mutate(cross_vars(., "gender", "vote_2016"))
cross_vars <- function(df, var1, var2) {

  new_name <- paste(var1, var2, sep = " x ")
  new_df <- df %>% unite(new_var, c(all_of(var1), all_of(var2)), sep = " x ")
  new_var <- new_df %>% select(new_var)
  names(new_var)[1] <- new_name
  out <- cbind(df, new_var)

  return(out)

}
