#' Combine list of data.frames vertically as one data.frame
#'
#' @param df_list A \code{list} of data.frames
#' @param row.vars A \code{character} vector
#'
#' @return A \code{data.frame}
#'
#' @importFrom data.table rbindlist
#' @export
#'
#' @examples
stacktab <- function(df, row.vars) {
  x <- data.table::rbindlist(df, fill = T)
  var_names <- x %>%
    select(all_of(row.vars)) %>% names()
  out <- x %>%
    gather(all_of(var_names), key = "Question", value = "Response") %>%
    drop_na(Response)
}
