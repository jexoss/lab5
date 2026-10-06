test_that("get_questions returns the requested number of rows",{
  skip_on_cran()
  Sys.sleep(6)
  qs <- get_questions(amount = 3, difficulty = "easy")
  expect_equal(nrow(qs), 3)
})

test_that("get_questions returns the right columns",{
  skip_on_cran()
  Sys.sleep(6)
  qs <- get_questions(amount = 2)
  expect_true(all(c("category", "type", "difficulty", "question",
                    "correct_answer", "incorrect_answers") %in% names(qs)))
})

test_that("get_questions respects the requested difficulty", {
  skip_on_cran()
  Sys.sleep(6)
  qs <- get_questions(amount = 3, difficulty = "hard")
  expect_true(all(qs$difficulty == "hard"))
})

test_that("get_questions rejects invalid amount",{
  expect_error(get_questions(amount = -1))
  expect_error(get_questions(amount = "five"))
})

test_that("get_questions splits large request into several calls",{
  skip_on_cran()
  skip_on_ci()
  Sys.sleep(6)
  qs <- get_questions(amount = 55, difficulty = "easy")
  expect_equal(nrow(qs), 55)
})

test_that("plot_questions returns a ggplot object", {
  skip_on_cran()
  Sys.sleep(6)
  qs <- get_questions(amount = 5)
  p <- plot_questions(qs)
  expect_s3_class(p, "ggplot")
})

test_that("plot_questions rejects invalid input", {
  expect_error(plot_questions(data.frame(x = 1)))
  expect_error(plot_questions("not a data.frame"))
})
