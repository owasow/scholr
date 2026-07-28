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
#' scriptsize font, single significance star at p < 0.05. Supports odds
#' ratios for GLM models when \code{model} is provided as a named argument.
#'
#' @param ... Arguments passed to stargazer
#' @param model A model or list of models. Use this named argument (instead of
#'   passing models via \code{...}) when you need \code{odds.ratio = TRUE}.
#' @param odds.ratio Logical; if TRUE and \code{model} is provided, exponentiate
#'   coefficients to display odds ratios with delta-method standard errors.
#' @param type Output format. If NULL (default), auto-detected.
#' @param digits Number of digits to display
#' @param star.cutoffs Significance cutoffs
#'
#' @export
#' @examples
#' \dontrun{
#' # Basic usage
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' star0(model)
#'
#' # With odds ratios for logistic regression
#' m <- glm(am ~ wt + hp, data = mtcars, family = binomial)
#' star0(model = m, odds.ratio = TRUE)
#' }
star0 <- function(..., model = NULL, odds.ratio = FALSE, type = NULL,
                  digits = 3, star.cutoffs = star_cut_vector) {
    if (is.null(type)) type <- get_star_format()

    # Common stargazer options
    star_opts <- list(
        digits = digits,
        header = FALSE,
        type = type,
        align = TRUE,
        font.size = 'scriptsize',
        star.cutoffs = star.cutoffs,
        notes.append = FALSE,
        notes = "*$p<0.05$"
    )

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
#' @param warn_unmatched Logical. If TRUE, warn listing any displayed term
#'   that no label mapping matched (passed to \code{convert_labels()}).
#' @param engine \code{"coef"} (default) derives the display order directly
#'   from the models' coefficient names, mirroring stargazer's internal
#'   merge/order/keep/omit logic -- robust to long variable names and header
#'   noise, and does not run stargazer at all. \code{"text"} forces the
#'   legacy behavior of parsing \code{stargazer(type = "text")} output.
#'   The coef engine falls back to text automatically for model classes
#'   without a usable \code{coef()} method or for numeric omit/keep indices.
#' @return Character vector of human-readable variable labels suitable for
#'   use with stargazer's covariate.labels argument.
#'
#' @details When using stargazer's \code{omit} parameter along with
#'   \code{covariate.labels}, you must ensure the labels don't include entries
#'   for omitted variables. Pass the same \code{omit} pattern to \code{star_var}
#'   to generate correctly aligned labels.
#'
#'   Pass the same \code{keep}, \code{order}, \code{omit} (and, if used,
#'   \code{perl}, \code{intercept.bottom}, \code{intercept.top}) values you
#'   give the real stargazer call; star_var reproduces the resulting row
#'   order from the models' coefficients.
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
star_var <- function(..., omit = NULL, warn_unmatched = FALSE,
                     engine = c("coef", "text")) {
    engine <- match.arg(engine)
    dots <- list(...)
    nm <- names(dots)
    if (is.null(nm)) nm <- rep("", length(dots))

    keep <- if ("keep" %in% nm) dots[["keep"]] else NULL
    order <- if ("order" %in% nm) dots[["order"]] else NULL
    perl <- if ("perl" %in% nm) isTRUE(dots[["perl"]]) else FALSE
    intercept.bottom <- if ("intercept.bottom" %in% nm) {
        isTRUE(dots[["intercept.bottom"]])
    } else TRUE
    intercept.top <- if ("intercept.top" %in% nm) {
        isTRUE(dots[["intercept.top"]])
    } else FALSE

    if (engine == "coef") {
        models <- .flatten_models(dots[nm == ""])
        coef_lists <- lapply(models, .model_coefs)
        computable <- length(coef_lists) > 0 &&
            !any(vapply(coef_lists, is.null, logical(1))) &&
            !is.numeric(omit) && !is.numeric(keep)
        if (computable) {
            display <- .display_covariates(
                coef_lists, omit = omit, keep = keep, order = order,
                perl = perl, intercept.bottom = intercept.bottom,
                intercept.top = intercept.top
            )
            return(convert_labels(display, extracted = TRUE,
                                  warn_unmatched = warn_unmatched))
        }
        message("star_var: could not derive coefficient order directly; ",
                "falling back to parsing stargazer text output.")
    }
    .star_var_text(..., omit = omit, warn_unmatched = warn_unmatched)
}

# Legacy engine: run stargazer(type = "text") and parse the printed table.
# Kept as a fallback for model classes coef() cannot handle and for numeric
# omit/keep indices; fragile when covariate names are long or when stargazer
# prints header junk (e.g. models built with paste()-constructed formulas).
.star_var_text <- function(..., omit = NULL, warn_unmatched = FALSE) {

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
    cov_labels <- convert_labels(variable_names, extracted = TRUE,
                                 warn_unmatched = warn_unmatched)

    return(cov_labels)
}

# Unnamed ... arguments may be models or plain lists of models (stargazer
# accepts both); flatten one level of non-object lists.
.flatten_models <- function(args) {
    out <- list()
    for (a in args) {
        if (is.list(a) && !is.object(a)) {
            out <- c(out, a)
        } else {
            out <- c(out, list(a))
        }
    }
    out
}

# Named coefficient vector for one model, in model order, or NULL if the
# class is not supported (which routes star_var to the text engine).
.model_coefs <- function(m) {
    if (inherits(m, "coeftest")) {
        v <- m[, 1]
        return(stats::setNames(as.vector(v), rownames(m)))
    }
    cf <- tryCatch(stats::coef(m), error = function(e) NULL)
    if (is.matrix(cf) && !is.null(rownames(cf))) {
        return(stats::setNames(cf[, 1], rownames(cf)))
    }
    if (is.numeric(cf) && !is.null(names(cf))) {
        return(cf)
    }
    NULL
}

# Reproduce the covariate rows stargazer displays, in display order, from
# coefficient names alone -- no text parsing. Mirrors stargazer 5.2.3
# internals (.stargazer.wrap):
#   1. merge coefficient names across models with stargazer's interleaving
#      insert (a new variable goes after the run of merged-list variables
#      the current model does not share),
#   2. rename intercepts to "Constant" and move them to the bottom (or top),
#   3. apply `order` (character regexes, first-match-wins with masking, or
#      numeric indices; unmatched rows follow in existing order),
#   4. apply `keep`/`omit` regexes against the renamed rows -- when `keep`
#      is supplied stargazer ignores `omit` entirely, a quirk mirrored here,
#   5. drop the Constant row itself (covariate.labels supplied by star_var
#      label the rows above it). Aliased (NA) coefficients keep their row,
#      exactly as stargazer prints them with blank cells.
.display_covariates <- function(coef_lists, omit = NULL, keep = NULL,
                                order = NULL, perl = FALSE,
                                intercept.bottom = TRUE,
                                intercept.top = FALSE) {
    intercept_strings <- c("(Intercept)", "(intercept)", "Intercept")
    intercept_name <- "Constant"

    # 1. stargazer's merge: start from the first model's names, insert each
    # later model's new names after the longest run of names that model
    # does not share.
    gcv <- names(coef_lists[[1]])
    for (cl in coef_lists[-1]) {
        coef.var <- names(cl)
        position <- 0
        for (v in coef.var) {
            idx <- match(v, gcv)
            if (!is.na(idx)) {
                position <- idx
            } else {
                while (position < length(gcv) &&
                       !(gcv[position + 1] %in% coef.var)) {
                    position <- position + 1
                }
                gcv <- append(gcv, v, after = position)
                position <- position + 1
            }
        }
    }


    # 2. intercepts: rename, then relocate.
    gcv[gcv %in% intercept_strings] <- intercept_name
    ipos <- which(gcv == intercept_name)
    if (length(ipos) > 0) {
        rest <- gcv[-ipos]
        icept <- gcv[ipos]
        if (intercept.bottom) gcv <- c(rest, icept)
        if (intercept.top) gcv <- c(icept, rest)
    }

    # 3. order (applied to the full row list, after intercept relocation).
    if (!is.null(order) && length(gcv) > 0) {
        new.order <- NULL
        if (is.character(order)) {
            not.ordered.yet <- gcv
            for (rx in order) {
                add.these <- grep(rx, not.ordered.yet, perl = perl)
                not.ordered.yet[add.these] <- NA
                if (length(add.these) != 0) new.order <- c(new.order, add.these)
            }
        } else if (is.numeric(order)) {
            order <- unique(order)
            order <- order[order <= length(gcv)]
            new.order <- order
        }
        if (!is.null(new.order)) {
            gcv <- gcv[c(new.order, setdiff(seq_along(gcv), new.order))]
        }
    }

    # 4. keep / omit (keep, when present, overrides omit -- stargazer quirk).
    shown <- rep(TRUE, length(gcv))
    if (!is.null(omit)) {
        for (rx in omit) shown <- shown & !grepl(rx, gcv, perl = perl)
    }
    if (!is.null(keep)) {
        shown <- rep(FALSE, length(gcv))
        for (rx in keep) shown <- shown | grepl(rx, gcv, perl = perl)
    }
    gcv <- gcv[shown]

    # 5. covariate.labels fed by star_var label the non-Constant rows.
    gcv[gcv != intercept_name]
}


#' Stargazer with Odds Ratios (GLM)
#'
#' Wrapper for stargazer that displays odds ratios with delta-method
#' standard errors for GLM models. For mixed models (glmer), use
#' \code{star_glmer()} instead.
#'
#' @param model_list A model or list of models
#' @param odds.ratio Logical; if TRUE, exponentiate coefficients (default: FALSE)
#' @param ... Additional arguments passed to stargazer
#'
#' @return Stargazer output
#' @export
#' @examples
#' \dontrun{
#' m <- glm(am ~ wt + hp, data = mtcars, family = binomial)
#' stargazer2(m, odds.ratio = TRUE)
#' }
stargazer2 <- function(model_list, odds.ratio = FALSE, ...) {
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
