#' Stargazer Support for glmer Models
#'
#' Functions to create stargazer-compatible model objects from glmer models.
#' Two approaches are provided:
#' 1. glmer_to_stargazer() - creates a "trojan" glm object (simpler, some limitations)
#' 2. star_glmer() - wrapper that uses stargazer's coef/se override (recommended)
#'
#' @name glmer-stargazer
NULL

#' Stargazer Wrapper for glmer Models
#'
#' A wrapper around stargazer that properly handles glmer models by extracting
#' coefficients and SEs (with optional delta-method transformation for odds ratios)
#' and using stargazer's override parameters.
#'
#' @param ... One or more glmer models, or a list of glmer models
#' @param exponentiate Logical. If TRUE, exponentiates coefficients (for odds
#'   ratios) and computes delta-method SEs. Default FALSE.
#' @param type Output type: "text", "latex", or "html". Default uses get_star_format().
#' @param star_args Additional arguments passed to stargazer
#'
#' @return Stargazer output (printed or returned based on type)
#' @export
#'
#' @examples
#' \dontrun{
#' library(lme4)
#' model <- glmer(y ~ x + (1|group), data = df, family = binomial)
#'
#' # Log-odds coefficients
#' star_glmer(model, type = "text")
#'
#' # Odds ratios with delta-method SEs
#' star_glmer(model, exponentiate = TRUE, type = "text")
#'
#' # Multiple models
#' star_glmer(model1, model2, model3, exponentiate = TRUE)
#' }
star_glmer <- function(..., exponentiate = FALSE, type = NULL, star_args = list()) {

    # Collect models
    models <- list(...)
    if (length(models) == 1 && is.list(models[[1]]) && !inherits(models[[1]], "glmerMod")) {
        models <- models[[1]]
    }

    # Determine output type
    if (is.null(type)) {
        type <- get_star_format()
    }

    # Extract coefficients and SEs from each model
    coef_list <- lapply(models, function(m) {
        beta <- lme4::fixef(m)
        if (exponentiate) {
            beta <- exp(beta)
        }
        beta
    })

    se_list <- lapply(models, function(m) {
        beta <- lme4::fixef(m)
        se <- sqrt(diag(as.matrix(stats::vcov(m))))
        if (exponentiate) {
            # Delta method: SE(exp(b)) = exp(b) * SE(b)
            se <- exp(beta) * se
        }
        se
    })

    # Create template glm models for stargazer structure
    # Note: We use a for loop instead of lapply because stargazer
    # checks formula environments and lapply closures cause issues
    template_models <- vector("list", length(models))
    for (i in seq_along(models)) {
        m <- models[[i]]
        beta <- lme4::fixef(m)
        n_obs <- stats::nobs(m)
        family_obj <- stats::family(m)
        coef_names <- names(beta)
        n_coef <- length(beta)

        # Create fake data with right predictor names
        if (n_coef > 1) {
            pred_names <- coef_names[-1]
            fake_X <- matrix(0, nrow = 2, ncol = length(pred_names))
            colnames(fake_X) <- pred_names
            fake_data <- as.data.frame(fake_X)
            fake_data$y <- c(1, 0)
            formula_str <- paste("y ~", paste(pred_names, collapse = " + "))
        } else {
            fake_data <- data.frame(y = c(1, 0))
            formula_str <- "y ~ 1"
        }

        # Create template glm using eval in global environment
        # This ensures stargazer can properly inspect the model
        template_call <- bquote(
            suppressWarnings(
                glm(.(stats::as.formula(formula_str)), data = .(fake_data), family = .(family_obj))
            )
        )
        template <- eval(template_call, envir = globalenv())

        # Fix key statistics
        template$df.residual <- n_obs - n_coef
        template$aic <- stats::AIC(m)

        template_models[[i]] <- template
    }

    # Extract nobs for add.lines
    nobs_vals <- sapply(models, stats::nobs)

    # Merge omit.stat: always include "n", plus any user-specified stats
    user_omit_stat <- star_args$omit.stat
    merged_omit_stat <- unique(c("n", user_omit_stat))
    star_args$omit.stat <- NULL  # Remove from star_args to avoid duplicate

    # Merge add.lines: prepend Observations row to any user-specified lines
    user_add_lines <- star_args$add.lines
    merged_add_lines <- c(list(c("Observations", nobs_vals)), user_add_lines)
    star_args$add.lines <- NULL  # Remove from star_args to avoid duplicate

    # Build stargazer call
    # Use c() with unlist to flatten template_models into the args list
    sg_args <- c(
        template_models,
        list(
            type = type,
            coef = coef_list,
            se = se_list,
            omit.stat = merged_omit_stat,
            add.lines = merged_add_lines
        ),
        star_args
    )

    # stargazer expects models as unnamed positional args, then named args
    # So we need: list(model1, model2, ..., type="text", coef=..., se=...)
    do.call(stargazer::stargazer, sg_args)
}

#' Extract glmer Coefficients and SEs for Stargazer
#'
#' Extracts coefficients and standard errors from glmer models in a format
#' ready for stargazer's coef and se parameters.
#'
#' @param models A list of glmer models
#' @param exponentiate Logical. If TRUE, exponentiates coefficients and
#'   applies delta-method to SEs. Default FALSE.
#'
#' @return A list with components `coef` and `se`, each a list of vectors
#' @export
#'
#' @examples
#' \dontrun{
#' stats <- glmer_coef_se(list(model1, model2), exponentiate = TRUE)
#' stargazer(model1, model2, coef = stats$coef, se = stats$se)
#' }
glmer_coef_se <- function(models, exponentiate = FALSE) {
    if (!is.list(models)) {
        models <- list(models)
    }

    coef_list <- lapply(models, function(m) {
        beta <- lme4::fixef(m)
        if (exponentiate) exp(beta) else beta
    })

    se_list <- lapply(models, function(m) {
        beta <- lme4::fixef(m)
        se <- sqrt(diag(as.matrix(stats::vcov(m))))
        if (exponentiate) {
            exp(beta) * se
        } else {
            se
        }
    })

    list(coef = coef_list, se = se_list)
}

#' Convert glmer to Stargazer-Compatible Model
#'
#' Creates a fake glm object with coefficients and standard errors extracted
#' from a glmer model. Note: This approach has limitations with stargazer's

#' internal SE extraction. Consider using star_glmer() instead.
#'
#' @param model A glmer model object (from lme4::glmer)
#' @param exponentiate Logical. If TRUE, exponentiates coefficients (for odds
#'   ratios) and computes delta-method SEs. Default FALSE.
#'
#' @return A modified glm object with glmer_trojan class
#' @export
#'
#' @examples
#' \dontrun{
#' library(lme4)
#' model <- glmer(y ~ x + (1|group), data = df, family = binomial)
#' trojan <- glmer_to_stargazer(model)
#'
#' # Use with stargazer by extracting coef/se
#' stats <- glmer_coef_se(list(model))
#' stargazer(trojan, coef = stats$coef, se = stats$se)
#' }
glmer_to_stargazer <- function(model, exponentiate = FALSE) {

    # Extract fixed effects and variance-covariance matrix
    beta <- lme4::fixef(model)
    vcov_mat <- as.matrix(stats::vcov(model))
    se <- sqrt(diag(vcov_mat))

    # Apply exponentiation if requested (for odds ratios)
    if (exponentiate) {
        # Delta method: SE(exp(b)) = exp(b) * SE(b)
        se <- exp(beta) * se
        beta <- exp(beta)
    }

    # Get model info
    n_obs <- stats::nobs(model)
    family_obj <- stats::family(model)

    # Create design matrix for fake model
    n_coef <- length(beta)
    coef_names <- names(beta)

    # Create minimal fake data
    fake_y <- c(1, 0)

    # Build formula and fake data
    if (n_coef > 1) {
        # Has predictors beyond intercept
        pred_names <- coef_names[-1]  # Exclude intercept
        fake_X <- matrix(0, nrow = 2, ncol = length(pred_names))
        colnames(fake_X) <- pred_names
        fake_data <- as.data.frame(fake_X)
        fake_data$y <- fake_y
        formula_str <- paste("y ~", paste(pred_names, collapse = " + "))
    } else {
        # Intercept only
        fake_data <- data.frame(y = fake_y)
        formula_str <- "y ~ 1"
    }

    # Fit a trivial glm (it will converge instantly on 2 observations)
    suppressWarnings({
        trojan <- stats::glm(
            stats::as.formula(formula_str),
            data = fake_data,
            family = family_obj
        )
    })

    # Overwrite coefficients
    trojan$coefficients <- beta

    # Store custom vcov
    if (exponentiate) {
        orig_vcov <- as.matrix(stats::vcov(model))
        exp_beta <- exp(lme4::fixef(model))
        trojan_vcov <- outer(exp_beta, exp_beta) * orig_vcov
    } else {
        trojan_vcov <- vcov_mat
    }
    attr(trojan, "custom_vcov") <- trojan_vcov
    attr(trojan, "custom_se") <- se

    # Add our class
    class(trojan) <- c("glmer_trojan", class(trojan))

    # Store original model info
    attr(trojan, "original_nobs") <- n_obs
    attr(trojan, "exponentiated") <- exponentiate
    attr(trojan, "original_class") <- class(model)

    # Fix df.residual for stargazer
    trojan$df.residual <- n_obs - n_coef
    trojan$aic <- stats::AIC(model)

    return(trojan)
}

#' @importFrom stats vcov nobs logLik
#' @export
vcov.glmer_trojan <- function(object, ...) {
    attr(object, "custom_vcov")
}

#' @export
nobs.glmer_trojan <- function(object, ...) {
    attr(object, "original_nobs")
}

#' @export
logLik.glmer_trojan <- function(object, ...) {
    # Return a logLik object that stargazer can use
    ll <- -object$aic / 2 + length(stats::coef(object))  # Approximate from AIC
    attr(ll, "df") <- length(stats::coef(object))
    attr(ll, "nobs") <- stats::nobs(object)
    class(ll) <- "logLik"
    ll
}

#' Convert Multiple glmer Models for Stargazer
#'
#' Convenience function to convert a list of glmer models.
#'
#' @param models A list of glmer models
#' @param exponentiate Logical. If TRUE, exponentiates coefficients.
#'
#' @return A list of trojan glm objects
#' @export
#'
#' @examples
#' \dontrun{
#' models <- list(model1, model2, model3)
#' trojans <- glmer_list_to_stargazer(models, exponentiate = TRUE)
#' stargazer(trojans)
#' }
glmer_list_to_stargazer <- function(models, exponentiate = FALSE) {
    lapply(models, glmer_to_stargazer, exponentiate = exponentiate)
}
