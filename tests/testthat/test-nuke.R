test_that("works with NAs in multiple columns (default)", {
  a <- dplyr::tibble(
    x = c(1,2,NA,3),
    y = c(1,2,3,NA)
  ) %>%
    nuke()

  expect_equal(
    a$x,
    c(1,2,0,3)
  )

  expect_equal(
    a$y,
    c(1,2,3,0)
  )
})


test_that("works with non-NA values (exact = TRUE)", {
  a <- dplyr::tibble(
    x = c(1,2,"nuke_me",3),
    y = c(1,2,3,"nuke_me")
  ) %>%
    nuke("nuke_me", "YAY")

  expect_equal(
    a$x,
    c("1","2","YAY","3")
  )
})


test_that("works with non-NA values (exact = FALSE)", {
  a <- dplyr::tibble(
    x = c(1,2,"nuke_me",3),
    y = c(1,2,3,"nuke_me")
  ) %>%
    nuke("nuk", "YAY", exact = FALSE)

  expect_equal(
    a$x,
    c("1","2","YAY","3")
  )
})


test_that("which_cols param works properly", {
  a <- dplyr::tibble(
    x = c(1,2,"nuke_me",3),
    y = c(1,2,3,"nuke_me")
  ) %>%
    nuke("nuk", "YAY", exact = FALSE, which_cols = "x")

  expect_equal(
    a$x,
    c("1","2","YAY","3")
  )

  expect_equal(
    a$y,
    c("1","2","3","nuke_me")
  )
})



test_that("where param works properly", {
  a <- dplyr::tibble(
    x = c(1,2,3),
    y = c(1,4,9),
    z = c("nope", "nope", "nuke_me")
  ) %>%
    nuke("nuke_me", "YAY", where = "x > 1 & y > 4")

  expect_equal(
    a$z,
    c("nope", "nope", "YAY")
  )
})

