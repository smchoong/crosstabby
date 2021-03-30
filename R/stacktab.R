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
stacktab <- function(df_list, row.vars) {
  x <- data.table::rbindlist(df_list, fill = T)
  x <- x %>%
    select(all_of(row.vars)) %>%
    gather(., key = "Question", value = "Response") %>%
    drop_na(Response)
}
