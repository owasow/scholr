#' Inline Reporting Helpers
#'
#' Functions to streamline inline R code for reporting regression results
#' in R Markdown documents. These helpers extract and format coefficients,
#' p-values, standard errors, and odds ratios from model objects.
#'
#' @name inline-helpers
#' @examples
#' \dontrun{
#' # In R Markdown prose, instead of:
#' # `r round(coef(model)["var"], 2)`
#' # `r round(summary(model)$coef["var", 4], 3)`
#' #
#' # Use the cleaner:
#' # `r b(model, "var")`
#' # `r p(model, "var")`
#' # `r bp(model, "var")`
#' }
NULL

#' Extract Coefficient from Model
#'
#' Extracts and rounds a coefficient from a fitted model object.
#'
#' @param model A fitted model object (e.g., from lm, glm, lmer)
#' @param var Character string naming the coefficient to extract
#' @param digits Number of decimal places (default: 2)
#'
#' @return Rounded coefficient value
#' @export
#'
#' @examples
#' m <- glm(am ~ wt + hp, data = mtcars, family = binomial)
#' b(m, "wt")
#' b(m, "hp", digits = 3)
b <- function(model, var, digits = 2) {
    round(stats::coef(model)[var], digits)
}

#' Extract P-value from Model
#'
#' Extracts and formats a p-value from a fitted model object.
#' Returns "< .001" for very small p-values, otherwise "= X.XXX".
#'
#' @param model A fitted model object (e.g., from lm, glm, lmer)
#' @param var Character string naming the coefficient
#' @param digits Number of decimal places (default: 3)
#'
#' @return Formatted p-value string (e.g., "= 0.032" or "< .001")
#' @export
#'
#' @examples
#' m <- glm(am ~ wt + hp, data = mtcars, family = binomial)
#' p(m, "wt")
#' # In R Markdown: "The effect was significant (p `r p(m, 'wt')`)."
p <- function(model, var, digits = 3) {
    # Try different coefficient table structures
    s <- summary(model)

    # For glm/lm models
    if (!is.null(s$coefficients)) {
        coef_table <- s$coefficients
        # Handle different column names
        p_col <- grep("Pr\\(|p-value|p.value|Pr\\(>", colnames(coef_table), value = TRUE)
        if (length(p_col) > 0) {
            pval <- coef_table[var, p_col[1]]
        } else if (ncol(coef_table) >= 4) {
            pval <- coef_table[var, 4]
        } else {
            stop("Cannot find p-value column in model summary")
        }
    } else {
        stop("Model type not supported")
    }

    if (pval < 0.001) {
        "< .001"
    } else {
        paste0("= ", round(pval, digits))
    }
}

#' Extract Standard Error from Model
#'
#' Extracts and rounds a standard error from a fitted model object.
#'
#' @param model A fitted model object (e.g., from lm, glm, lmer)
#' @param var Character string naming the coefficient
#' @param digits Number of decimal places (default: 2)
#'
#' @return Rounded standard error value
#' @export
#'
#' @examples
#' m <- glm(am ~ wt + hp, data = mtcars, family = binomial)
#' se(m, "wt")
se <- function(model, var, digits = 2) {
    s <- summary(model)
    round(s$coefficients[var, "Std. Error"], digits)
}

#' Extract Odds Ratio from Model
#'
#' Extracts a coefficient and exponentiates it to get the odds ratio.
#' Useful for logistic regression models.
#'
#' @param model A fitted model object (typically from glm with family = binomial)
#' @param var Character string naming the coefficient
#' @param digits Number of decimal places (default: 2)
#'
#' @return Rounded odds ratio (exponentiated coefficient)
#' @export
#'
#' @examples
#' m <- glm(am ~ wt + hp, data = mtcars, family = binomial)
#' or(m, "wt")
#' # Interpretation: A one-unit increase in wt multiplies odds by or(m, "wt")
or <- function(model, var, digits = 2) {
    round(exp(stats::coef(model)[var]), digits)
}

#' Format Coefficient and P-value Together
#'
#' Extracts both coefficient and p-value and formats them as a single string.
#' Useful for inline reporting in R Markdown.
#'
#' @param model A fitted model object
#' @param var Character string naming the coefficient
#' @param b_digits Digits for coefficient (default: 2)
#' @param p_digits Digits for p-value (default: 3)
#'
#' @return Character string in format "b = X.XX, p = Y.YYY" or "b = X.XX, p < .001"
#' @export
#'
#' @examples
#' m <- glm(am ~ wt + hp, data = mtcars, family = binomial)
#' bp(m, "wt")
#' # Returns: "b = -8.08, p = 0.014"
#'
#' # In R Markdown prose:
#' # "Weight significantly predicted transmission (`r bp(m, 'wt')`)."
#' # Renders as: "Weight significantly predicted transmission (b = -8.08, p = 0.014)."
bp <- function(model, var, b_digits = 2, p_digits = 3) {
    paste0("b = ", b(model, var, b_digits), ", p ", p(model, var, p_digits))
}

#' Format Odds Ratio and P-value Together
#'
#' Extracts both odds ratio and p-value and formats them as a single string.
#' Useful for inline reporting of logistic regression results.
#'
#' @param model A fitted model object (typically logistic regression)
#' @param var Character string naming the coefficient
#' @param or_digits Digits for odds ratio (default: 2)
#' @param p_digits Digits for p-value (default: 3)
#'
#' @return Character string in format "OR = X.XX, p = Y.YYY"
#' @export
#'
#' @examples
#' m <- glm(am ~ wt + hp, data = mtcars, family = binomial)
#' orp(m, "wt")
#' # Returns: "OR = 0, p = 0.014"
#'
#' # In R Markdown prose:
#' # "Weight was associated with lower odds of manual transmission (`r orp(m, 'wt')`)."
orp <- function(model, var, or_digits = 2, p_digits = 3) {
    paste0("OR = ", or(model, var, or_digits), ", p ", p(model, var, p_digits))
}

#' Extract Z-value or T-value from Model
#'
#' Extracts and rounds the test statistic (z or t value) from a fitted model.
#'
#' @param model A fitted model object
#' @param var Character string naming the coefficient
#' @param digits Number of decimal places (default: 2)
#'
#' @return Rounded test statistic
#' @export
#'
#' @examples
#' m <- glm(am ~ wt + hp, data = mtcars, family = binomial)
#' z(m, "wt")
z <- function(model, var, digits = 2) {
    s <- summary(model)
    coef_table <- s$coefficients
    # Find z or t value column
    stat_col <- grep("z value|t value|z|t", colnames(coef_table), value = TRUE)
    if (length(stat_col) > 0) {
        round(coef_table[var, stat_col[1]], digits)
    } else if (ncol(coef_table) >= 3) {
        round(coef_table[var, 3], digits)
    } else {
        NA
    }
}

#' Extract 95% Confidence Interval
#'
#' Computes and formats a 95% confidence interval for a coefficient.
#'
#' @param model A fitted model object
#' @param var Character string naming the coefficient
#' @param digits Number of decimal places (default: 2)
#' @param exp Logical; if TRUE, exponentiate for odds ratio CI (default: FALSE)
#'
#' @return Character string in format "[lower, upper]"
#' @export
#'
#' @examples
#' m <- glm(am ~ wt + hp, data = mtcars, family = binomial)
#' ci95(m, "wt")
#' ci95(m, "wt", exp = TRUE)  # For odds ratio CI
ci95 <- function(model, var, digits = 2, exp = FALSE) {
    ci <- stats::confint(model, var, level = 0.95)
    if (exp) ci <- base::exp(ci)
    paste0("[", round(ci[1], digits), ", ", round(ci[2], digits), "]")
}
