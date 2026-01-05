#' Variable Label Conversion
#'
#' Convert technical variable names to human-readable labels for regression tables.
#'
#' @name convert-labels
NULL

# Package environment to store custom label mappings
.tablr_env <- new.env()

#' Set Custom Label Mappings
#'
#' Register project-specific variable name to label mappings. These take
#' precedence over the default mappings provided by the package.
#'
#' @param mappings A named list or named character vector where names are
#'   regex patterns and values are the replacement labels.
#' @param append If TRUE (default), add to existing mappings. If FALSE,
#'   replace all custom mappings.
#'
#' @export
#' @examples
#' # Set mappings for a specific project
#' set_label_mappings(c(
#'   "^immig_stcontext" = "State Immigration Context",
#'   "^isss_5item"      = "ISSS (5-item)",
#'   "^likelydeported"  = "Likely Deported"
#' ))
#'
#' # Clear all custom mappings
#' set_label_mappings(list(), append = FALSE)
set_label_mappings <- function(mappings, append = TRUE) {
    if (append && exists("custom_mappings", envir = .tablr_env)) {
        existing <- get("custom_mappings", envir = .tablr_env)
        mappings <- c(mappings, existing)
    }
    assign("custom_mappings", mappings, envir = .tablr_env)
    invisible(mappings)
}

#' Get Current Label Mappings
#'
#' Retrieve the currently registered custom label mappings.
#'
#' @return A named character vector of mappings, or NULL if none set.
#' @export
#' @examples
#' get_label_mappings()
get_label_mappings <- function() {
    if (exists("custom_mappings", envir = .tablr_env)) {
        get("custom_mappings", envir = .tablr_env)
    } else {
        NULL
    }
}

#' Clear Custom Label Mappings
#'
#' Remove all custom label mappings.
#'
#' @export
clear_label_mappings <- function() {
    if (exists("custom_mappings", envir = .tablr_env)) {
        rm("custom_mappings", envir = .tablr_env)
    }
    invisible(NULL)
}

#' Strip Wave Suffixes from Variable Names
#'
#' Internal helper to normalize variable names by removing wave/year suffixes.
#'
#' @param x Character vector of variable names
#' @return Character vector with wave suffixes removed
#' @keywords internal
.norm_strip_wave <- function(x) {
    # before type tags (e.g., pid7_16_int -> pid7_int)
    x <- stringr::str_replace(x, "(?:_(16|20|24))(?=_(fct|bin|int|ihs|z)\\b)", "")
    # trailing _16/_20/_24
    x <- stringr::str_replace(x, "(?:_(16|20|24))$", "")
    # trailing bare 16/20/24 after a non-digit (age16 -> age)
    x <- stringr::str_replace(x, "(?<=\\D)(16|20|24)$", "")
    x
}

#' Convert Variable Names to Human-Readable Labels
#'
#' Converts technical variable names from model output to human-readable labels
#' suitable for publication tables. Uses custom mappings (if set) first, then
#' falls back to built-in defaults for common variable names.
#'
#' @param model Either a model object (from which terms are extracted via broom::tidy)
#'   or a character vector of variable names.
#' @param extracted Logical. If FALSE (default), extracts terms from model object.
#'   If TRUE, treats `model` as a character vector of variable names.
#' @param use_defaults Logical. If TRUE (default), apply default mappings for

#'   common variables after custom mappings.
#'
#' @return Character vector of human-readable labels
#' @export
#'
#' @examples
#' # With a character vector
#' convert_labels(c("age", "female", "income_10k"), extracted = TRUE)
#'
#' # Set custom mappings first
#' set_label_mappings(c("^myvar" = "My Custom Variable"))
#' convert_labels(c("myvar", "age"), extracted = TRUE)
convert_labels <- function(model, extracted = FALSE, use_defaults = TRUE) {

    # Extract variable names
    if (extracted == FALSE) {
        labs <- broom::tidy(model)$term
    } else {
        labs <- model
    }

    # Store original for fallback
    labs_orig <- labs

    # Normalize to handle wave suffixes
    labs_norm <- .norm_strip_wave(labs)

    # Initialize output with original labels
    labs2 <- labs

    # Apply custom mappings first (if any)
    # Uses first-match-wins: once a variable is labeled, it won't be overwritten
    custom <- get_label_mappings()
    already_labeled <- rep(FALSE, length(labs))
    if (!is.null(custom) && length(custom) > 0) {
        for (i in seq_along(custom)) {
            pattern <- names(custom)[i]
            replacement <- custom[i]
            matches <- (stringr::str_detect(labs, pattern) |
                       stringr::str_detect(labs_norm, pattern)) & !already_labeled
            if (any(matches)) {
                labs2[matches] <- replacement
                already_labeled[matches] <- TRUE
            }
        }
    }

    # Apply default mappings if requested
    if (use_defaults) {
        labs2 <- apply_default_mappings(labs, labs2, labs_norm)
    }

    return(labs2)
}

#' Apply Default Label Mappings
#'
#' Internal function that applies built-in default mappings for common
#' variable names.
#'
#' @param labs Original variable names
#' @param labs2 Current labels (may have custom mappings applied)
#' @param labs_norm Normalized variable names (wave suffixes removed)
#' @return Character vector with default mappings applied
#' @keywords internal
apply_default_mappings <- function(labs, labs2, labs_norm) {

    # Only apply defaults where no custom mapping was applied
    # (i.e., where labs2 still equals labs)
    unchanged <- labs2 == labs

    result <- dplyr::case_when(
        !unchanged ~ labs2,  # Keep custom mappings

        # Intercept
        labs == "(Intercept)" ~ "(Intercept)",

        # Demographics - common patterns
        stringr::str_detect(labs_norm, "^age$") ~ "Age",
        stringr::str_detect(labs_norm, "^female") ~ "Female",
        stringr::str_detect(labs_norm, "^male") ~ "Male",
        stringr::str_detect(labs_norm, "^educ|^education") ~ "Education",
        stringr::str_detect(labs_norm, "^income") ~ "Income",
        stringr::str_detect(labs_norm, "^married") ~ "Married",
        stringr::str_detect(labs_norm, "^employed") ~ "Employed",

        # Race/ethnicity
        stringr::str_detect(labs_norm, "race.*[bB]lack") ~ "Race: Black",
        stringr::str_detect(labs_norm, "race.*[hH]ispanic") ~ "Race: Hispanic",
        stringr::str_detect(labs_norm, "race.*[wW]hite") ~ "Race: White",
        stringr::str_detect(labs_norm, "race.*[oO]ther") ~ "Race: Other",
        stringr::str_detect(labs_norm, "race.*[aA]sian") ~ "Race: Asian",
        stringr::str_detect(labs_norm, "race.*[nN]ative") ~ "Race: Native American",

        # Political variables
        stringr::str_detect(labs_norm, "^pid7") ~ "Party ID (7-point)",
        stringr::str_detect(labs_norm, "^pid3") ~ "Party ID (3-cat)",
        stringr::str_detect(labs_norm, "^ideo7") ~ "Ideology (7-point)",
        stringr::str_detect(labs_norm, "^ideo5") ~ "Ideology (5-point)",
        stringr::str_detect(labs_norm, "pid.*Dem$") ~ "Party: Democrat",
        stringr::str_detect(labs_norm, "pid.*Rep$") ~ "Party: Republican",
        stringr::str_detect(labs_norm, "pid.*Ind$") ~ "Party: Independent",

        # Region
        stringr::str_detect(labs_norm, "^west$") ~ "West",
        stringr::str_detect(labs_norm, "^northeast$") ~ "Northeast",
        stringr::str_detect(labs_norm, "^south$") ~ "South",
        stringr::str_detect(labs_norm, "^midwest$") ~ "Midwest",

        # Generation
        stringr::str_detect(labs_norm, "^gen_1st$") ~ "1st Generation",
        stringr::str_detect(labs_norm, "^gen_2nd$") ~ "2nd Generation",
        stringr::str_detect(labs_norm, "^gen_3rd$") ~ "3rd Generation",

        # Survey/study design
        stringr::str_detect(labs_norm, "^wave") ~ "Wave",
        stringr::str_detect(labs_norm, "^time$|^time_") ~ "Time",

        # Catch interaction terms - keep as is but clean up
        stringr::str_detect(labs, ":") ~ labs,

        # Default: return original
        TRUE ~ labs
    )

    return(result)
}
