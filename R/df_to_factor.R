#' Transform all variables of data.frame to factors
#'
#' @param df A \code{data.frame}
#'
#' @return A \code{data.frame} with factors as variables
#' @export
#'
#' @examples
df_to_factor <- function(df) {
  df_factored <- df %>% mutate(across(!where(is.factor), ~factor(.x)))
  return(df_factored)
}
