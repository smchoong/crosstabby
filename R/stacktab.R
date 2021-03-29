stacktab <- function(x) {
  x <- data.table::rbindlist(x, fill = T)
  out <- x %>%
    gather(grep("q", names(x), value = T), key = "Question", value = "Response") %>%
    drop_na(Response)

}
