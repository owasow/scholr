#' Utility Functions
#'
#' Miscellaneous helper functions for formatting values in tables.
#'
#' @name utils
NULL

#' Add Comma Separators to Numbers
#'
#' Formats numbers with comma separators for thousands.
#'
#' @param x Numeric vector
#' @param ... Additional arguments passed to format()
#'
#' @return Character vector with formatted numbers
#' @export
#' @examples
#' add_comma(1234567)
#' add_comma(c(1000, 2000000, 500))
add_comma <- function(x, ...) {
    format(x, ..., big.mark = ",", scientific = FALSE, trim = TRUE)
}

#' Convert Number to Word
#'
#' Converts small integers (1-10) to their word equivalents.
#'
#' @param x Integer vector
#' @return Character vector with numbers as words (for 1-10) or as-is
#' @export
#' @examples
#' number_to_word(3)
#' number_to_word(c(1, 5, 15))
number_to_word <- function(x) {
    words <- c("one", "two", "three", "four", "five",
               "six", "seven", "eight", "nine", "ten")
    ifelse(x > 10 | x < 1, as.character(x), words[x])
}

#' Round to One Decimal Place
#'
#' @param x Numeric vector
#' @return Numeric vector rounded to 1 decimal place
#' @export
#' @examples
#' round1(3.456)
round1 <- function(x) {
    round(as.numeric(x), 1)
}

#' Round to Two Decimal Places
#'
#' @param x Numeric vector
#' @return Numeric vector rounded to 2 decimal places
#' @export
#' @examples
#' round2(3.456)
round2 <- function(x) {
    round(as.numeric(x), 2)
}

#' Replace NA with Dash
#'
#' Converts NA values to "-" for table display.
#'
#' @param x Vector to process
#' @return Character vector with NAs replaced by "-"
#' @export
#' @examples
#' na_to_dash(c(1, NA, 3))
na_to_dash <- function(x) {
    x <- as.character(x)
    ifelse(is.na(x) | x == "NA", "-", x)
}

#' Replace NA with Blank
#'
#' Converts NA values to empty string for table display.
#'
#' @param x Vector to process
#' @return Character vector with NAs replaced by ""
#' @export
#' @examples
#' na_to_blank(c(1, NA, 3))
na_to_blank <- function(x) {
    x <- as.character(x)
    ifelse(is.na(x) | x == "NA", "", x)
}

#' Not In Operator
#'
#' Negation of the \%in\% operator. Returns TRUE for elements not in the set.
#'
#' @param x Vector of values to check
#' @param table Vector of values to check against
#' @return Logical vector
#' @export
#' @examples
#' c(1, 2, 3) %nin% c(2, 4)
#' # Returns: TRUE FALSE TRUE
`%nin%` <- function(x, table) {
    !x %in% table
}

#' Format P-value for LaTeX (Categorical)
#'
#' Formats p-values into categorical bins for LaTeX output.
#' Unlike \code{p()} which returns exact values, this returns
#' category labels like "$p < 0.001$".
#'
#' @param x Numeric p-value(s)
#' @return Character string with LaTeX-formatted p-value category
#' @export
#' @examples
#' pval(0.0001)
#' # Returns: "$p < 0.001$"
#' pval(0.03)
#' # Returns: "$p < 0.05$"
pval <- function(x) {
    if (is.data.frame(x) || is.matrix(x)) x <- x[[1]]
    x <- as.numeric(x)
    dplyr::case_when(
        x < 0.001 ~ "$p < 0.001$",
        x < 0.01  ~ "$p < 0.01$",
        x < 0.05  ~ "$p < 0.05$",
        TRUE      ~ "$p > 0.05$"
    )
}

#' Format Exponentiated Coefficient as Percent Change
#'
#' For logistic/Poisson regression, converts a coefficient to percent
#' change in odds/rate: (exp(b) - 1) * 100.
#'
#' @param model A fitted model object
#' @param coef Coefficient index (default: 2, first predictor after intercept)
#'   or character name of coefficient
#' @param digits Number of decimal places (default: 1)
#' @return Formatted percent change string
#' @export
#' @examples
#' m <- glm(am ~ wt, data = mtcars, family = binomial)
#' format_exp(m, "wt")
#' # Interpretation: A 1-unit increase in wt changes odds by X%
format_exp <- function(model, coef = 2, digits = 1) {
    b <- stats::coef(model)[coef]
    formatC((exp(b) - 1) * 100, format = "f", digits = digits)
}

#' Calculate Pseudo R-squared for Model List
#'
#' Extracts McFadden's pseudo R-squared from a list of GLM models
#' using \code{pscl::pR2()}.
#'
#' @param model_list A list of fitted GLM models
#' @return Character vector of formatted pseudo R-squared values
#' @export
#' @examples
#' \dontrun{
#' m1 <- glm(am ~ wt, data = mtcars, family = binomial)
#' m2 <- glm(am ~ wt + hp, data = mtcars, family = binomial)
#' pseudo(list(m1, m2))
#' }
pseudo <- function(model_list) {
    if (!requireNamespace("pscl", quietly = TRUE)) {
        stop("Package 'pscl' is required for pseudo(). Install with install.packages('pscl')")
    }
    purrr::map_dbl(model_list, function(x) as.numeric(pscl::pR2(x)[[6]])) |>
        round(digits = 2) |>
        format(nsmall = 2)
}
