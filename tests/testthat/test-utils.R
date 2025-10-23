test_that("bind_rows_safe() works", {
  df1 <- tibble::tibble(
    a = c(1, 2, 3),
    b = c(NA, NA, NA),
    c = c(TRUE, FALSE, TRUE)
  )
  df2 <- tibble::tibble(a = c("A"), b = "2025-01-01", c = c(NA))

  comb <- bind_rows_safe(df1, df2)
  expect_s3_class(comb, "data.frame")
  expect_type(comb$a, "character")
  expect_s3_class(comb$b, "Date")
  expect_type(comb$c, "logical")
})
