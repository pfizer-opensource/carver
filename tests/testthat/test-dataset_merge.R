df1 <- iris |>
  dplyr::select(Species) |>
  dplyr::distinct()

df2 <- iris |>
  dplyr::select(Species, dplyr::ends_with("Width"))

test_that("dataset_merge works", {
  expected <- dplyr::left_join(df1, df2, by = "Species")
  actual <- dataset_merge(df1, df2, byvars = "Species")
  expect_identical(expected, actual)
})

test_that("dataset_merge works with subsets and for > 2 datasets to merge", {
  df12 <- dplyr::filter(df1, Species == "setosa")
  df21 <- dplyr::filter(df2, Sepal.Width > 3)
  df13 <- dplyr::filter(df1, Species != "versicolor")
  expected1 <- dplyr::left_join(df1, df21, by = "Species")
  actual1 <-
    dataset_merge(df1, df2, byvars = "Species", subset = list(NA_character_, "Sepal.Width > 3"))
  expected2 <- dplyr::left_join(df12, df21, by = "Species")
  actual2 <-
    dataset_merge(df1, df2, byvars = "Species", subset = list("Species == 'setosa'", "Sepal.Width > 3")) # nolint
  expected3 <- purrr::reduce(list(df13, df12, df2), dplyr::left_join, "Species")
  actual3 <-
    dataset_merge(
      df1, df1, df2,
      byvars = "Species",
      subset = list("Species != 'versicolor'", "Species == 'setosa'", NA_character_)
    )
  expect_identical(expected1, actual1)
  expect_identical(expected2, actual2)
  expect_identical(expected3, actual3)
})

test_that("dataset_merge returns expected errors", {
  expect_error(
    dataset_merge(
      df1,
      df2,
      byvars = "xxx"
    ),
    "`byvars` not present"
  )
  expect_error(
    dataset_merge(
      df1,
      df2,
      byvars = "Species",
      subset = list(NA_character_, NA_character_)
    ),
    "All subsets cannot be `NA`, use `subset = NULL` instead"
  )
})
