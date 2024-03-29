
context("data.frame object returned")

test_that("when length(row.var)==1 output is a data.frame", {
  df <- data.frame(
    x = c(1, 1, 1, 1),
    y = c(1, 1, 2, 2),
    z = c(1, 2, 1, 2)
  )
  expect_s3_class(big_tabby(df,"x"), "data.frame")
})

test_that("when length(row.var)>1 output is a data.frame", {
  df <- data.frame(
    x = c(1, 1, 1, 1),
    y = c(1, 1, 2, 2),
    z = c(1, 2, 1, 2)
  )
  var<-c("x","y")
  expect_s3_class(big_tabby(df,var), "data.frame")
})

test_that("when length(row.var)==1 and length(col.var)==1 output is a data.frame", {
  df <- data.frame(
    x = c(1, 1, 1, 1),
    y = c(1, 1, 2, 2),
    z = c(1, 2, 1, 2)
  )
  var<-c("x","y")
  by<-"z"
  expect_s3_class(big_tabby(df,var,by), "data.frame")
})
