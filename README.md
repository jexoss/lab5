
# lab5

<!-- badges: start -->
[![R-CMD-check](https://github.com/jexoss/lab5/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jexoss/lab5/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

lab5 provides functions to query the Open Trivia Database API, handling
session tokens, decoding and the API's rate limit, and returns the questions
as a data frame. It also includes a function to visualize the category and difficulty distribution of the returned questions.

A companion Shiny app for interactively exploring questions is available at [lab5-shiny](https://github.com/jexoss/lab5-shiny).

## Installation

You can install the development version of lab5 like so:

``` r
# install.packages("devtools")
devtools::install_github("jexoss/lab5", build_vignettes = TRUE)
```

## Example

This is a basic example which shows you how to fetch trivia questions:

``` r
library(lab5)
qs <- get_questions(amount = 10, difficulty = "easy")
head(qs)

plot_questions(qs)
```

See `vignette("lab5")` for a full walkthrough.
