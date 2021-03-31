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
  df$R_NUM = c(1:nrow(df))
  new_df <- df %>% drop_na(c(all_of(var1), all_of(var2))) %>%
    unite(new_var, c(all_of(var1), all_of(var2)), sep = " x ")
  new_var <- new_df %>% select(R_NUM, new_var)
  names(new_var)[2] <- new_name
  out <- merge(df, new_var, by="R_NUM", all = T) %>% select(-R_NUM)

  return(out)

}
