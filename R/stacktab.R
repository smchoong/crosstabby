#' Combine list of data.frames vertically as one data.frame
#'
#' @param x A \code{list} of data.frames
#'
#' @return A \code{data.frame}
#'
#' @importFrom data.table rbindlist
#' @export
#'
#' @examples
stacktab <- function(x) {
  x <- data.table::rbindlist(x, fill = T)
  out <- x %>%
    gather(grep("q", names(x), value = T), key = "Question", value = "Response") %>%
    drop_na(Response)

}
