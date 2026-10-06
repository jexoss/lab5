# Request a new session token fomr the Open Trivia Database.
# Session tokens make sure the API never returns the same question twice
# during a session. See https://opentdb.com/api_config.php

get_token <- function(){
  resp <- httr2::request("https://opentdb.com/api_token.php") |>
    httr2::req_url_query(command = "request") |>
    httr2::req_perform()

  httr2::resp_body_json(resp)$token
}

# Decode a single base64-encoded string returned by the API.
decode_b64 <- function(x){
  rawToChar(base64enc::base64decode(x))
}

# Fetch a single batch of up to 50 questions as a data.frame.
# This is the low-level call against api-php. get_questions() wraps this
# to handle requests for more than 50 questions and the rate limit.
get_questions_once <- function(amount, difficulty = NULL, category = NULL,
                              type = NULL, token = NULL){
  stopifnot(is.numeric(amount), length(amount) == 1, amount >= 1, amount <= 50)

  resp <- httr2::request("https://opentdb.com/api.php") |>
    httr2::req_url_query(
      amount = amount,
      difficulty = difficulty,
      category = category,
      type = type,
      encode = "base64",
      token = token
    ) |>
    httr2::req_perform()

  body <- httr2::resp_body_json(resp)

  if (body$response_code != 0) {
    stop("Open Trivia DB returned response code ", body$response_code)
  }

  results <- body$results

  data.frame(
    category = vapply(results, function(q) decode_b64(q$category), character(1)),
    type = vapply(results, function(q) decode_b64(q$type), character(1)),
    difficulty = vapply(results, function(q) decode_b64(q$difficulty), character(1)),
    question = vapply(results, function(q) decode_b64(q$question), character(1)),
    correct_answer = vapply(results, function(q) decode_b64(q$correct_answer), character(1)),
    incorrect_answers = I(lapply(results, function(q) {
      vapply(q$incorrect_answers, decode_b64, character(1))
    })),
    stringsAsFactors = FALSE
  )
}
