#' Fetch trivia questions from the Open Trivia Database
#'
#' Downloads trivia questions from the Open Trivia Database API
#' (\url{https://opentdb.com}). The function requests a session token so
#' the same question is never returned twice within one call, decodes the
#' base64-encoded text the API returns, and automatically splits requests
#' for more than 50 questions into several calls, waiting 5 seconds between
#' them to respect the API's rate limit of one request per 5 seconds.
#'
#' @param amount Number of questions to fetch. Must be 1 or more; the API
#'   allows at most 50 questions per call, so larger requests are split
#'   into several calls automatically.
#' @param difficulty Optional difficulty: one of \code{"easy"},
#'   \code{"medium"} or \code{"hard"}. If \code{NULL}, questions of any
#'   difficulty are returned.
#' @param category Optional numeric category id. See
#'   \url{https://opentdb.com/api_category.php} for valid ids. If
#'   \code{NULL}, questions from any category are returned.
#' @param type Optional question type: \code{"multiple"} for multiple
#'   choice, or \code{"boolean"} for true/false. If \code{NULL}, both
#'   types are returned.
#'
#' @return A data.frame with one row per question and the columns
#'   \code{category}, \code{type}, \code{difficulty}, \code{question},
#'   \code{correct_answer} and \code{incorrect_answers}. The last column
#'   is a list-column, since each question has a different number of
#'   incorrect answers.
#'
#' @references
#' \url{https://opentdb.com/api_config.php}
#'
#' @importFrom httr2 request req_url_query req_perform resp_body_json
#' @importFrom base64enc base64decode
#'
#' @examples
#' \dontrun{
#' qs <- get_questions(amount = 10, difficulty = "easy")
#' head(qs)
#' }
#'
#' @export
get_questions <- function(amount, difficulty = NULL, category = NULL,
                          type = NULL) {
  stopifnot(is.numeric(amount), length(amount) == 1, amount >= 1)

  token <- get_token()
  all_results <- list()
  remaining <- amount

  while (remaining > 0) {
    batch_size <- min(remaining, 50)

    all_results[[length(all_results) + 1]] <- get_questions_once(
      amount = batch_size,
      difficulty = difficulty,
      category = category,
      type = type,
      token = token
    )

    remaining <- remaining - batch_size

    if (remaining > 0) {
      Sys.sleep(5)
    }
  }

  do.call(rbind, all_results)
}


#' Plot the category and difficulty distribution of trivia questions
#'
#' Takes the data.frame returned by \code{\link{get_questions}} and plots
#' how many questions were returned per category, with each bar split by
#' difficulty.
#'
#' @param questions A data.frame as returned by \code{\link{get_questions}}.
#'
#' @return A ggplot object, invisibly. Called primarily for its side effect
#' of producing a plot.
#'
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_minimal position_stack guide_legend
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' qs <- get_questions(amount = 20)
#' plot_questions(qs)
#' }
#'
#' @export
plot_questions <- function(questions) {
  stopifnot(is.data.frame(questions),
            all(c("category", "difficulty") %in% names(questions)))

  questions$difficulty <- factor(questions$difficulty, levels = c("easy",
                                                                 "medium",
                                                                 "hard"))

  p<- ggplot2::ggplot(questions, ggplot2::aes(x = .data$category,
                                              fill = .data$difficulty)) +
    ggplot2::geom_bar(position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_fill_manual(values = c(easy = "#4CAF50",
                                          medium = "#2196F3", hard = "#F44336"),
                               guide = ggplot2::guide_legend(reverse = TRUE)
                               ) +
    ggplot2::labs(title = "Questions per category and difficulty",
                  x = "Category", y = "Number of questions",
                  fill = "Difficulty") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  print(p)
  invisible(p)
}
