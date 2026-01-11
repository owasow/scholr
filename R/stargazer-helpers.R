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

#' Basic Stargazer Wrapper
#'
#' Stargazer with sensible defaults: auto-detected format, no header,
#' scriptsize font, single significance star at p < 0.05.
#'
#' @param ... Arguments passed to stargazer
#' @param type Output format. If NULL (default), auto-detected.
#' @param digits Number of digits to display
#' @param star.cutoffs Significance cutoffs
#'
#' @export
#' @examples
#' \dontrun{
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' star0(model)
#' }
star0 <- function(..., type = NULL, digits = 3, star.cutoffs = star_cut_vector) {
    if (is.null(type)) type <- get_star_format()

    stargazer::stargazer(
        ...,
        digits = digits,
        header = FALSE,
        type = type,
        align = TRUE,
        font.size = 'scriptsize',
        star.cutoffs = star.cutoffs,
        notes.append = FALSE,
        notes = "*$p<0.05$"
    )
}

#' Stargazer Wrapper (Simplified)
#'
#' Like star0 but omits theta statistic (useful for negative binomial models).
#'
#' @inheritParams star0
#' @export
star1 <- function(..., type = NULL, digits = 2, star.cutoffs = star_cut_vector) {
    if (is.null(type)) type <- get_star_format()

    stargazer::stargazer(
        ...,
        digits = digits,
        header = FALSE,
        type = type,
        align = TRUE,
        omit.stat = c("theta"),
        star.cutoffs = star.cutoffs,
        notes.append = FALSE,
        notes = "*$p<0.05$"
    )
}

#' Stargazer with Omit Pattern
#'
#' Stargazer wrapper with an omit parameter for filtering variables.
#'
#' @inheritParams star0
#' @param omit Regex pattern of variables to omit from output
#' @param notes Custom notes string
#'
#' @export
star_ft <- function(..., type = NULL, omit = NULL, notes = "*$p<0.05$",
                    digits = 3, star.cutoffs = star_cut_vector) {
    if (is.null(type)) type <- get_star_format()

    stargazer::stargazer(
        ...,
        digits = digits,
        header = FALSE,
        type = type,
        align = TRUE,
        font.size = 'scriptsize',
        star.cutoffs = star.cutoffs,
        notes.append = FALSE,
        omit = omit,
        notes = notes
    )
}

#' Stargazer with Small Font
#'
#' Stargazer wrapper using small font size instead of scriptsize.
#'
#' @inheritParams star0
#' @export
star_sm <- function(..., type = NULL, digits = 2, star.cutoffs = star_cut_vector) {
    if (is.null(type)) type <- get_star_format()

    stargazer::stargazer(
        ...,
        digits = digits,
        header = FALSE,
        type = type,
        align = TRUE,
        font.size = 'small',
        star.cutoffs = star.cutoffs,
        notes.append = FALSE,
        notes = "*$p<0.05$"
    )
}

#' Stargazer with Normal Font
#'
#' Stargazer wrapper using default (normal) font size.
#'
#' @inheritParams star0
#' @export
star_nrm <- function(..., type = NULL, digits = 2, star.cutoffs = star_cut_vector) {
    if (is.null(type)) type <- get_star_format()

    stargazer::stargazer(
        ...,
        digits = digits,
        header = FALSE,
        type = type,
        align = TRUE,
        star.cutoffs = star.cutoffs,
        notes.append = FALSE,
        notes = "*$p<0.05$"
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
