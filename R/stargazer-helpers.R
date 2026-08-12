#' Stargazer Helper Functions
#'
#' Wrapper functions for stargazer with sensible defaults and automatic
#' output format detection.
#'
#' @name stargazer-helpers
NULL

#' Default Star Cutoffs
#'
#' Default significance cutoffs for stargazer tables.
#' By default, only uses p < 0.05 (single star).
#'
#' @export
star_cut_vector <- c(0.05, NA, NA)

#' Three-Star Cutoffs
#'
#' Traditional three-star significance cutoffs.
#'
#' @export
star_cut_three <- c(0.05, 0.01, 0.001)

#' Standard Compact Stargazer Table
#'
#' General-purpose wrapper that auto-detects the output format, suppresses the
#' standalone LaTeX header, aligns columns, displays three digits, uses
#' \code{scriptsize} for LaTeX, and reports a single significance threshold at
#' p < 0.05. Use this for the standard compact scholr regression table.
#'
#' For ordinary tables, pass one or more models through \code{...}. The named
#' \code{model} argument enables optional odds-ratio transformation for GLMs;
#' new code focused on odds ratios should generally use \code{star_or()}.
#'
#' @param ... Arguments passed to stargazer
#' @param model A model or list of models. Use this named argument (instead of
#'   passing models via \code{...}) when you need \code{odds.ratio = TRUE}.
#' @param odds.ratio Logical; if TRUE and \code{model} is provided, exponentiate
#'   coefficients to display odds ratios with delta-method standard errors.
#' @param type Output format. If NULL (default), auto-detected.
#' @param digits Number of digits to display
#' @param star.cutoffs Significance cutoffs.
#' @param omit Optional regex pattern for coefficients to omit.
#' @param omit.stat Optional character vector of model statistics to omit.
#' @param notes Custom table note.
#' @param font.size LaTeX font size. Defaults to \code{"scriptsize"}.
#'
#' @export
#' @examples
#' \dontrun{
#' # Basic usage
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' star_compact(model)
#'
#' # Hide fixed-effect coefficients while retaining them in the model
#' model_fe <- lm(mpg ~ wt + hp + factor(cyl), data = mtcars)
#' star_compact(model_fe, omit = "^factor\\(cyl\\)")
#' }
star_compact <- function(..., model = NULL, odds.ratio = FALSE, type = NULL,
                         digits = 3, star.cutoffs = star_cut_vector,
                         omit = NULL, omit.stat = NULL,
                         notes = "*$p<0.05$", font.size = "scriptsize") {
    if (is.null(type)) type <- get_star_format()

    # Common stargazer options
    star_opts <- list(
        digits = digits,
        header = FALSE,
        type = type,
        align = TRUE,
        star.cutoffs = star.cutoffs,
        notes.append = FALSE,
        notes = notes,
        omit = omit,
        omit.stat = omit.stat
    )
    if (!is.null(font.size)) star_opts$font.size <- font.size

    # Handle model passed as named argument (for odds.ratio support)
    if (!is.null(model)) {
        model_list <- if (inherits(model, "list")) model else list(model)

        if (odds.ratio) {
            coef_OR <- lapply(model_list, function(x) exp(stats::coef(x)))
            se_OR <- lapply(model_list, function(x) {
                exp(stats::coef(x)) * summary(x)$coef[, 2]
            })
            p_vals <- lapply(model_list, function(x) summary(x)$coefficients[, 4])

            do.call(stargazer::stargazer, c(
                list(model_list, coef = coef_OR, se = se_OR, p = p_vals),
                list(...),
                star_opts
            ))
        } else {
            do.call(stargazer::stargazer, c(
                list(model_list),
                list(...),
                star_opts
            ))
        }
    } else {
        # Original behavior - models passed via ...
        do.call(stargazer::stargazer, c(
            list(...),
            star_opts
        ))
    }
}

#' Stargazer Table with Small LaTeX Font
#'
#' Readability-oriented variant of \code{star_compact()} that uses LaTeX's
#' \code{small} font and displays two digits. Font size only affects LaTeX.
#'
#' @inheritParams star_compact
#' @export
#' @examples
#' \dontrun{
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' star_small(model, title = "Fuel Economy")
#' }
star_small <- function(..., model = NULL, odds.ratio = FALSE, type = NULL,
                       digits = 2, star.cutoffs = star_cut_vector,
                       omit = NULL, omit.stat = NULL,
                       notes = "*$p<0.05$") {
    star_compact(
        ..., model = model, odds.ratio = odds.ratio, type = type,
        digits = digits, star.cutoffs = star.cutoffs, omit = omit,
        omit.stat = omit.stat, notes = notes, font.size = "small"
    )
}

#' Stargazer Table with Normal LaTeX Font
#'
#' Readability-oriented variant of \code{star_compact()} that uses Stargazer's
#' normal LaTeX font and displays two digits. Font size only affects LaTeX.
#'
#' @inheritParams star_compact
#' @export
#' @examples
#' \dontrun{
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' star_normal(model, title = "Fuel Economy")
#' }
star_normal <- function(..., model = NULL, odds.ratio = FALSE, type = NULL,
                        digits = 2, star.cutoffs = star_cut_vector,
                        omit = NULL, omit.stat = NULL,
                        notes = "*$p<0.05$") {
    star_compact(
        ..., model = model, odds.ratio = odds.ratio, type = type,
        digits = digits, star.cutoffs = star.cutoffs, omit = omit,
        omit.stat = omit.stat, notes = notes, font.size = NULL
    )
}

#' Deprecated Stargazer Wrapper Names
#'
#' These legacy names remain available for compatibility. Use
#' \code{star_compact()}, \code{star_small()}, or \code{star_normal()} in new
#' code. Replace \code{star1(...)} with
#' \code{star_normal(..., omit.stat = "theta")}.
#'
#' @inheritParams star_compact
#' @name deprecated-star-wrappers
#' @keywords internal
NULL

#' @rdname deprecated-star-wrappers
#' @export
star0 <- function(..., model = NULL, odds.ratio = FALSE, type = NULL,
                  digits = 3, star.cutoffs = star_cut_vector) {
    .Deprecated("star_compact")
    star_compact(
        ..., model = model, odds.ratio = odds.ratio, type = type,
        digits = digits, star.cutoffs = star.cutoffs
    )
}

#' @rdname deprecated-star-wrappers
#' @export
star_ft <- function(..., type = NULL, omit = NULL, notes = "*$p<0.05$",
                    digits = 3, star.cutoffs = star_cut_vector) {
    .Deprecated("star_compact")
    star_compact(
        ..., type = type, digits = digits, star.cutoffs = star.cutoffs,
        omit = omit, notes = notes
    )
}

#' @rdname deprecated-star-wrappers
#' @export
star_sm <- function(..., type = NULL, digits = 2,
                    star.cutoffs = star_cut_vector) {
    .Deprecated("star_small")
    star_small(
        ..., type = type, digits = digits, star.cutoffs = star.cutoffs
    )
}

#' @rdname deprecated-star-wrappers
#' @export
star_nrm <- function(..., type = NULL, digits = 2,
                     star.cutoffs = star_cut_vector) {
    .Deprecated("star_normal")
    star_normal(
        ..., type = type, digits = digits, star.cutoffs = star.cutoffs
    )
}

#' @rdname deprecated-star-wrappers
#' @export
star1 <- function(..., type = NULL, digits = 2,
                  star.cutoffs = star_cut_vector) {
    .Deprecated("star_normal")
    star_normal(
        ..., type = type, digits = digits, star.cutoffs = star.cutoffs,
        omit.stat = "theta"
    )
}

#' Extract and Convert Variable Labels from Stargazer
#'
#' Extracts variable names from stargazer output and converts them to
#' human-readable labels using convert_labels(). Use the \code{omit} parameter
#' to exclude variables (like state fixed effects) from the labels - this
#' ensures proper alignment when using with stargazer's \code{covariate.labels}
#' and \code{omit} arguments together.
#'
#' @param ... Models to pass to stargazer
#' @param omit Regex pattern of variables to omit from labels. Should match
#'   the pattern used in your stargazer call's \code{omit} argument.
#'
#' @return Character vector of human-readable variable labels suitable for
#'   use with stargazer's covariate.labels argument.
#'
#' @details When using stargazer's \code{omit} parameter along with
#'   \code{covariate.labels}, you must ensure the labels don't include entries
#'   for omitted variables. Pass the same \code{omit} pattern to \code{star_var}
#'   to generate correctly aligned labels.
#'
#' @export
#' @examples
#' \dontrun{
#' # Basic usage
#' model <- lm(mpg ~ wt + hp + cyl, data = mtcars)
#' labels <- star_var(model)
#' star_ft(model, covariate.labels = labels)
#'
#' # With state fixed effects - use omit to exclude them from labels
#' model_fe <- lm(y ~ x + state_abb, data = mydata)
#' labels <- star_var(model_fe, omit = "^state_abb")
#' stargazer(model_fe, covariate.labels = labels, omit = "^state_abb")
#' }
star_var <- function(..., omit = NULL) {

    # Capture the stargazer output as text
    stargazer_output <- utils::capture.output(
        stargazer::stargazer(..., type = "text", omit = omit)
    )

    # Remove Constant and model stats like observations, R^2, AIC
    drop_below_constant <- which(stringr::str_detect(stargazer_output, "^Constant"))
    if (length(drop_below_constant) > 0) {
        stargazer_output <- stargazer_output[1:(drop_below_constant[1] - 1)]
    }

    # Filter lines containing variable names
    variable_lines <- grep("^[[:alpha:]]", stargazer_output, value = TRUE)

    # Extract variable names (first word in each line)
    variable_names <- sapply(variable_lines, function(line) {
        strsplit(line, "  +")[[1]][1]
    })
    variable_names <- unname(variable_names)

    # Convert to human-readable labels
    cov_labels <- convert_labels(variable_names, extracted = TRUE)

    return(cov_labels)
}

#' Stargazer with Odds Ratios (GLM)
#'
#' Wrapper for stargazer that displays odds ratios with delta-method
#' standard errors for GLM models. For mixed models (glmer), use
#' \code{star_glmer()} instead.
#'
#' @param model_list A model or list of models
#' @param odds.ratio Logical; if TRUE (default), exponentiate coefficients.
#' @param ... Additional arguments passed to stargazer
#'
#' @return Stargazer output
#' @export
#' @examples
#' \dontrun{
#' m <- glm(am ~ wt + hp, data = mtcars, family = binomial)
#' star_or(m)
#' }
star_or <- function(model_list, odds.ratio = TRUE, ...) {
    if (!inherits(model_list, "list")) model_list <- list(model_list)

    if (odds.ratio) {
        coef_OR <- lapply(model_list, function(x) exp(stats::coef(x)))
        se_OR <- lapply(model_list, function(x) {
            exp(stats::coef(x)) * summary(x)$coef[, 2]
        })
        p_vals <- lapply(model_list, function(x) summary(x)$coefficients[, 4])
        stargazer::stargazer(model_list, coef = coef_OR, se = se_OR, p = p_vals, ...)
    } else {
        stargazer::stargazer(model_list, ...)
    }
}

#' Deprecated Stargazer GLM Wrapper
#'
#' \code{stargazer2()} has been renamed to \code{star_or()} to make its purpose
#' clear. It remains available for compatibility and preserves its original
#' default of \code{odds.ratio = FALSE}.
#'
#' @inheritParams star_or
#' @param odds.ratio Logical; if TRUE, exponentiate coefficients. Defaults to
#'   FALSE for backward compatibility. New code should use \code{star_or()},
#'   where the default is TRUE.
#' @return Stargazer output
#' @export
#' @keywords internal
stargazer2 <- function(model_list, odds.ratio = FALSE, ...) {
    .Deprecated("star_or")
    star_or(model_list, odds.ratio = odds.ratio, ...)
}
