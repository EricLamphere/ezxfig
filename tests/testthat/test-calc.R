
test_that("no prefix, multiple formulas", {
  a <- dplyr::tibble(
    x = c(1,2,3),
    y = c(2,2,2)
  ) %>%
    calc(
      labels = c("x_times_y", "x_plus_y"),
      formulas = c("x * y", "x + y")
    )

  expect_equal(
    a$x_times_y,
    c(2,4,6)
  )

  expect_equal(
    a$x_plus_y,
    c(3,4,5)
  )
})


test_that("prefix included, multiple formulas", {
  a <- dplyr::tibble(
    x = c(1,2,3),
    y = c(2,2,2)
  ) %>%
    calc(
      labels = c("x_times_y", "x_plus_y"),
      formulas = c("x * y", "x + y"),
      prefix = "yay_"
    )

  expect_equal(
    colnames(a),
    c("x", "y", "yay_x_times_y", "yay_x_plus_y")
  )

  expect_equal(
    a$yay_x_times_y,
    c(2,4,6)
  )

  expect_equal(
    a$yay_x_plus_y,
    c(3,4,5)
  )

})
