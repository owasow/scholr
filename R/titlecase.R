#' Title Case Conversion Functions
#'
#' Functions to convert text to title case following the New York Times
#' Manual of Style conventions, and to process BibTeX files.
#'
#' @name titlecase
NULL

#' Convert Text to Title Case
#'
#' Converts text to title case following the New York Times Manual of Style.
#' Small words (a, an, the, and, etc.) are lowercased except at the start
#' or end of a title, or after punctuation like colons or semicolons.
#'
#' @param text Character string to convert
#' @param small_words Character vector of words to keep lowercase (unless at
#'   start/end of title). Defaults to NYT style guide list.
#' @param preserve_acronyms Character vector of acronyms to preserve in
#'   uppercase. Default includes common academic acronyms. All-caps words
#'   are also automatically preserved.
#' @param software_packages Character vector of software/package names to
#'   keep in lowercase and wrap in braces for BibTeX protection.
#'
#' @return Character string in title case
#' @export
#'
#' @details
#' The function handles several special cases:
#' \itemize{
#'   \item URLs, email addresses, and domains are preserved as-is
#'   \item Words with internal capitals (e.g., "iPhone", "McDonald's") are preserved
#'   \item All-caps words are preserved as acronyms (e.g., "US", "LGBTQ")
#'   \item LaTeX commands (starting with backslash) are preserved
#'   \item Abbreviations with periods (e.g., "U.S.", "U.K.") are preserved
#'   \item Small words are capitalized at the start/end of titles and after : ; ? !
#'   \item Handles text in parentheses, brackets, and quotes appropriately
#' }
#'
#' @examples
#' to_title_case("the quick brown fox")
#' to_title_case("a tale of two cities")
#' to_title_case("the iPhone: a revolution in mobile technology")
#' to_title_case("US policy on LGBTQ rights")
#'
to_title_case <- function(text,
                          small_words = c("a", "an", "and", "as", "at", "but",
                                          "by", "en", "for", "if", "in", "of",
                                          "on", "or", "the", "to", "v", "v.",
                                          "via", "vs", "vs.", "with"),
                          preserve_acronyms = c("US", "UK", "USA", "UN", "EU",
                                                "LGBTQ", "LGBTQ+", "LGBT",
                                                "HIV", "AIDS", "BET", "AM", "PM",
                                                "SSRN", "ICPSR", "CMPS", "NORC",
                                                "PS", "NYU", "UCLA", "MIT", "UFW",
                                                "NAACP", "CEO", "GDP", "GNP",
                                                "FBI", "CIA", "NSA", "NASA",
                                                "ANES", "CCES", "NES", "GSS",
                                                "ACS", "CPS", "IPUMS"),
                          software_packages = c("stm", "mice", "ggplot2",
                                                "ggridges", "tidyverse", "dplyr",
                                                "tidyr", "purrr", "stringr",
                                                "lubridate", "forcats", "readr")) {

    if (!is.character(text)) {
        stop("`text` must be a character vector.", call. = FALSE)
    }

    vapply(
        text,
        .to_title_case_one,
        character(1),
        USE.NAMES = FALSE,
        small_words = small_words,
        preserve_acronyms = preserve_acronyms,
        software_packages = software_packages
    )
}

.to_title_case_one <- function(text, small_words, preserve_acronyms,
                               software_packages) {

    if (is.na(text) || text == "") return(text)

    # Helper function to check if a word looks like a URL/email/domain
    is_url_or_email <- function(word) {
        # Must have :// (URL) or @ surrounded by alphanumerics (email)
        # or have multiple dots like a domain (example.com)
        grepl("://", word) ||
            grepl("[a-zA-Z0-9]@[a-zA-Z0-9]", word) ||
            grepl("[a-zA-Z0-9]\\.[a-zA-Z0-9]+\\.[a-zA-Z]", word)
    }

    # Helper function to check if word has TRUE internal capitals (e.g., iPhone, McDonald's)
    # All-caps words like "THE" should NOT be preserved (they should be title-cased)
    has_internal_caps <- function(word) {
        # Remove leading/trailing punctuation for checking
        clean_word <- gsub("^[^a-zA-Z]+|[^a-zA-Z]+$", "", word)
        if (nchar(clean_word) < 2) return(FALSE)

        # Must have BOTH uppercase and lowercase to be considered mixed case
        has_upper <- grepl("[A-Z]", clean_word)
        has_lower <- grepl("[a-z]", clean_word)
        if (!has_upper || !has_lower) return(FALSE)

        # Check if there are capitals after the first character
        middle <- substring(clean_word, 2)
        grepl("[A-Z]", middle)
    }

    # Helper function to check if word is an acronym (all caps, 2+ letters)
    is_acronym <- function(word) {
        # Remove leading/trailing punctuation for checking
        clean_word <- gsub("^[^a-zA-Z0-9+]+|[^a-zA-Z0-9+]+$", "", word)
        if (nchar(clean_word) < 2) return(FALSE)

        # Check if in explicit preserve list
        if (toupper(clean_word) %in% toupper(preserve_acronyms)) return(TRUE)

        # Check for abbreviations with periods (e.g., U.S., U.K., N.Y.)
        # Pattern: single caps letter followed by period, repeated
        if (grepl("^([A-Z]\\.)+[A-Z]?\\.?$", clean_word)) return(TRUE)

        # All letters are uppercase (excluding numbers and + sign)
        letters_only <- gsub("[^a-zA-Z]", "", clean_word)
        if (nchar(letters_only) >= 2 && letters_only == toupper(letters_only)) {
            return(TRUE)
        }

        FALSE
    }

    # Helper function to check if word is a LaTeX command
    is_latex_command <- function(word) {
        grepl("\\\\[a-zA-Z]", word)
    }

    # Helper function to check if word is a software package name
    is_software_package <- function(word) {
        clean_word <- gsub("^[^a-zA-Z0-9]+|[^a-zA-Z0-9]+$", "", word)
        tolower(clean_word) %in% tolower(software_packages)
    }

    # Helper function to capitalize first letter of a word
    capitalize_word <- function(word) {
        if (nchar(word) == 0) return(word)

        # Find first letter position
        chars <- strsplit(word, "")[[1]]
        first_letter_pos <- which(grepl("[a-zA-Z]", chars))[1]

        if (is.na(first_letter_pos)) return(word)

        chars[first_letter_pos] <- toupper(chars[first_letter_pos])
        # Lowercase the rest
        if (first_letter_pos < length(chars)) {
            for (i in (first_letter_pos + 1):length(chars)) {
                if (grepl("[A-Za-z]", chars[i])) {
                    chars[i] <- tolower(chars[i])
                }
            }
        }
        paste(chars, collapse = "")
    }

    # Split into tokens (words and spaces/punctuation)
    # This regex captures words and non-word separators
    tokens <- unlist(strsplit(text, "(?<=[^\\s])(?=\\s)|(?<=\\s)(?=[^\\s])", perl = TRUE))

    # Process each token
    words <- strsplit(text, "\\s+")[[1]]
    result_words <- character(length(words))

    for (i in seq_along(words)) {
        word <- words[i]

        # Preserve URLs, emails, domains
        if (is_url_or_email(word)) {
            result_words[i] <- word
            next
        }

        # Preserve LaTeX commands as-is
        if (is_latex_command(word)) {
            result_words[i] <- word
            next
        }

        # Preserve words with internal capitals
        if (has_internal_caps(word)) {
            result_words[i] <- word
            next
        }

        # Preserve acronyms (all-caps words)
        if (is_acronym(word)) {
            result_words[i] <- word
            next
        }

        # Handle software package names (keep lowercase)
        if (is_software_package(word)) {
            # Extract punctuation
            leading_punct <- gsub("^([^a-zA-Z0-9]*).*", "\\1", word)
            trailing_punct <- gsub(".*[a-zA-Z0-9]([^a-zA-Z0-9]*)$", "\\1", word)
            if (trailing_punct == word) trailing_punct <- ""
            core_word <- gsub("^[^a-zA-Z0-9]*|[^a-zA-Z0-9]*$", "", word)
            result_words[i] <- paste0(leading_punct, tolower(core_word), trailing_punct)
            next
        }

        # Extract the core word (without leading/trailing punctuation)
        leading_punct <- gsub("^([^a-zA-Z]*).*", "\\1", word)
        trailing_punct <- gsub(".*[a-zA-Z]([^a-zA-Z]*)$", "\\1", word)
        if (trailing_punct == word) trailing_punct <- ""
        core_word <- gsub("^[^a-zA-Z]*|[^a-zA-Z]*$", "", word)

        # Check if it's a small word
        core_lower <- tolower(core_word)
        is_small <- core_lower %in% tolower(small_words)

        if (is_small) {
            result_words[i] <- paste0(leading_punct, tolower(core_word), trailing_punct)
        } else {
            result_words[i] <- paste0(leading_punct, capitalize_word(core_word), trailing_punct)
        }
    }

    result <- paste(result_words, collapse = " ")

    # Capitalize first word of title (but not if it starts with a LaTeX command)
    if (!grepl("^\\\\[a-zA-Z]", result)) {
        result <- sub("^([^a-zA-Z]*)([a-z])", "\\1\\U\\2", result, perl = TRUE)
    }

    # Capitalize after sentence-ending punctuation followed by space
    result <- gsub("([:.;?!])\\s+([a-z])", "\\1 \\U\\2", result, perl = TRUE)

    # Capitalize first word after opening quotes/brackets at start
    result <- gsub("^([\"'`\\[\\(]+)([a-z])", "\\1\\U\\2", result, perl = TRUE)

    # Capitalize small words at the end of title (before final punctuation)
    # Use a helper function since R's regex case modifiers can be unreliable
    cap_first <- function(x) {
        paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2)))
    }

    small_pattern <- paste0("\\b(", paste(small_words, collapse = "|"), ")([,;:!?'\".\\)\\]]*)$")
    end_match <- regexpr(small_pattern, result, ignore.case = TRUE, perl = TRUE)
    if (end_match > 0) {
        match_len <- attr(end_match, "match.length")
        before <- substring(result, 1, end_match - 1)
        matched <- substring(result, end_match, end_match + match_len - 1)

        # Split matched into word and trailing punctuation
        word_match <- regexpr("^[a-zA-Z]+", matched)
        if (word_match > 0) {
            word_len <- attr(word_match, "match.length")
            word <- substring(matched, 1, word_len)
            punct <- substring(matched, word_len + 1)
            result <- paste0(before, cap_first(word), punct)
        }
    }

    result
}


#' Protect Abbreviations from CSL Case Conversion
#'
#' Wraps abbreviations and acronyms in braces to protect them from being
#' lowercased by CSL (Citation Style Language) text-case transformations.
#' This is necessary because CSL processors ignore BibTeX brace protection
#' and apply their own case rules.
#'
#' @param text Character string (typically a BibTeX title field)
#' @param protect_dotted Logical. If TRUE (default), protect dotted
#'   abbreviations like U.S., U.K., E.U.
#' @param protect_single_letter Logical. If TRUE (default), protect single
#'   uppercase letters that are likely programming languages or abbreviations
#'   (R, C, S, Q, J) but not common words (A, I).
#' @param protect_acronyms Character vector of specific acronyms to wrap in
#'   braces. Default includes common terms that CSL might lowercase.
#'
#' @return Character string with protected terms wrapped in braces
#' @export
#'
#' @details
#' CSL (Citation Style Language) processors like Pandoc's citeproc apply
#' their own case conversion rules that ignore BibTeX's brace protection.
#' When a CSL style specifies \code{text-case="title"} or
#' \code{text-case="sentence"}, terms like "U.S." become "u.s." and "R"
#' becomes "r".
#'
#' This function adds BibTeX braces around terms that need protection:
#' \itemize{
#'   \item Dotted abbreviations: U.S. -> \{U.S.\}, U.K. -> \{U.K.\}
#'   \item Single-letter languages/tools: R -> \{R\}, C -> \{C\}
#'   \item Explicit acronyms: AIDS -> \{AIDS\}, COVID -> \{COVID\}
#' }
#'
#' The braces tell BibTeX (and some CSL processors) to preserve the exact
#' capitalization.
#'
#' @examples
#' protect_caps_for_csl("The Effect of U.S. Policy on R Users")
#' # Protects U.S. and R with BibTeX braces.
#'
#' protect_caps_for_csl("COVID-19 and AIDS Research")
#' # Protects COVID and AIDS with BibTeX braces.
#'
protect_caps_for_csl <- function(text,
                                 protect_dotted = TRUE,
                                 protect_single_letter = TRUE,
                                 protect_acronyms = c(
                                   # Diseases/medical
                                   "AIDS", "HIV", "COVID", "SARS", "MERS",
                                   # Biology
                                   "DNA", "RNA", "PCR", "CRISPR",
                                   # Organizations
                                   "NATO", "UNESCO", "UNICEF", "WHO",
                                   # Technology
                                   "API", "SQL", "HTML", "CSS", "JSON", "XML",
                                   # Statistics/methods
                                   "OLS", "IV", "RCT", "DID", "RDD", "LATE",
                                   "GMM", "MLE", "MCMC", "HLM", "SEM",
                                   # Social science surveys
                                   "ANES", "GSS", "CPS", "ACS", "CCES", "CMPS",
                                   # Common acronyms
                                   "US", "UK", "EU", "UN", "GDP", "GNP",
                                   "LGBTQ", "LGBT", "CEO", "CFO", "NGO",
                                   "IRB", "NSF", "NIH", "DOJ", "DOD"
                                 )) {

  if (is.na(text) || text == "") return(text)

  # 1. Protect dotted abbreviations like U.S., U.K., E.U., N.Y.
  # Pattern: 2+ capital letters each followed by period
  if (protect_dotted) {
    # Match: capital letter + period, repeated 2+ times, optionally ending
    # with capital. Examples: U.S., U.K., E.U., N.Y., U.S.A.
    # Negative lookbehind/ahead to avoid already-braced content
    text <- gsub(
      "(?<!\\{)(([A-Z]\\.){2,}[A-Z]?)(?!\\})",
      "{\\1}",
      text,
      perl = TRUE
    )
  }

  # 2. Protect single uppercase letters that are programming languages/tools
  # R, C, S, Q (for Q methodology), J (for J language)
  # But NOT: A, I (common English words)
  if (protect_single_letter) {
    single_letters <- c("R", "C", "S", "Q", "J")

    for (letter in single_letters) {
      # Match the letter at word boundary, not already in braces
      pattern <- paste0("(?<!\\{)\\b", letter, "\\b(?![}])")
      replacement <- paste0("{", letter, "}")
      text <- gsub(pattern, replacement, text, perl = TRUE)
    }
  }

  # 3. Protect explicit acronyms from the list
  for (acronym in protect_acronyms) {
    # Case-sensitive match at word boundaries, not already in braces
    pattern <- paste0("(?<!\\{)\\b", acronym, "\\b(?!\\})")
    replacement <- paste0("{", acronym, "}")
    text <- gsub(pattern, replacement, text, perl = TRUE)
  }

  # 4. Clean up any double-bracing that might have occurred
  text <- gsub("\\{\\{", "{", text)
  text <- gsub("\\}\\}", "}", text)

  text
}


#' Process BibTeX File with Title Case Conversion
#'
#' Reads a BibTeX file and converts Title, Journal, Publisher, and Booktitle
#' fields to title case following NYT Manual of Style conventions.
#'
#' @param input_file Path to the input BibTeX file
#' @param output_file Path for the output file. If NULL (default), creates a
#'   timestamped file in the same directory as input.
#' @param clean_locations Logical. If TRUE (default), standardizes location
#'   names (e.g., "New York, NY" becomes "New York").
#' @param clean_abbreviations Logical. If TRUE (default), expands common
#'   publisher abbreviations (e.g., "Pr" becomes "Press") and wraps software
#'   package names in braces.
#' @param protect_for_csl Logical. If TRUE (default), wrap abbreviations and
#'   acronyms in braces to protect them from CSL case conversion. This is
#'   important when using Pandoc/citeproc which applies its own title-case
#'   rules that ignore BibTeX brace conventions. Only applied to Title fields.
#' @param fields_to_process Character vector of BibTeX field names to process.
#'   Defaults to c("Title", "Journal", "Publisher", "Booktitle").
#' @param preserve_acronyms Character vector of acronyms to preserve in
#'   uppercase. All-caps words are also automatically preserved. Default
#'   includes common academic acronyms (US, UK, LGBTQ, AIDS, SSRN, etc.).
#' @param software_packages Character vector of software/package names to
#'   keep in lowercase and wrap in braces for BibTeX protection. Default
#'   includes common R packages (stm, mice, ggplot2, tidyverse, etc.).
#' @param csl_protect_acronyms Character vector of additional acronyms to
#'   protect for CSL. These are added to the built-in list in
#'   \code{\link{protect_caps_for_csl}}. Set to NULL (default) to use only
#'   the built-in list.
#'
#' @return Invisibly returns the path to the output file
#' @export
#'
#' @examples
#' \dontrun{
#' # Process a BibTeX file with CSL protection (default)
#' bib_to_titlecase("references.bib")
#'
#' # Specify output file
#' bib_to_titlecase("references.bib", "references_cleaned.bib")
#'
#' # Add custom acronyms to protect for CSL
#' bib_to_titlecase("references.bib", csl_protect_acronyms = c("WEIRD", "fMRI"))
#'
#' # Disable CSL protection
#' bib_to_titlecase("references.bib", protect_for_csl = FALSE)
#'
#' # Process only Title fields
#' bib_to_titlecase("references.bib", fields_to_process = "Title")
#' }
#'
bib_to_titlecase <- function(input_file,
                             output_file = NULL,
                             clean_locations = TRUE,
                             clean_abbreviations = TRUE,
                             protect_for_csl = TRUE,
                             fields_to_process = c("Title", "Journal",
                                                   "Publisher", "Booktitle"),
                             preserve_acronyms = c("US", "UK", "USA", "UN", "EU",
                                                   "LGBTQ", "LGBTQ+", "LGBT",
                                                   "HIV", "AIDS", "BET", "AM", "PM",
                                                   "SSRN", "ICPSR", "CMPS", "NORC",
                                                   "PS", "NYU", "UCLA", "MIT", "UFW",
                                                   "NAACP", "CEO", "GDP", "GNP",
                                                   "FBI", "CIA", "NSA", "NASA",
                                                   "ANES", "CCES", "NES", "GSS",
                                                   "ACS", "CPS", "IPUMS"),
                             software_packages = c("stm", "mice", "ggplot2",
                                                   "ggridges", "tidyverse", "dplyr",
                                                   "tidyr", "purrr", "stringr",
                                                   "lubridate", "forcats", "readr",
                                                   "brms", "rstan", "lme4", "lavaan",
                                                   "fixest", "modelsummary", "knitr",
                                                   "rmarkdown", "shiny", "devtools"),
                             csl_protect_acronyms = NULL) {

    if (!file.exists(input_file)) {
        stop("Input file does not exist: ", input_file)
    }

    # Read the file
    lines <- readLines(input_file, warn = FALSE)

    # Build regex pattern for fields to process (case-insensitive)
    field_pattern <- paste0(
        "^\\s*(",
        paste(fields_to_process, collapse = "|"),
        ")\\s*=\\s*\\{",
        collapse = ""
    )

    # Find rows with target fields
    field_rows <- grep(field_pattern, lines, ignore.case = TRUE)

    if (length(field_rows) == 0) {
        message("No matching fields found in the BibTeX file.")
        return(invisible(input_file))
    }

    # Process each matching line
    for (i in field_rows) {
        line <- lines[i]

        # Extract the part before the opening brace content
        # Pattern: whitespace + fieldname + = + {
        prefix_match <- regexpr("^\\s*[a-zA-Z]+\\s*=\\s*\\{+", line)
        if (prefix_match == -1) next

        prefix_len <- attr(prefix_match, "match.length")
        prefix <- substring(line, 1, prefix_len)

        # Extract content between braces
        # Handle potentially nested braces and content ending with },
        content_start <- prefix_len + 1
        remaining <- substring(line, content_start)

        # Find the content (everything before the closing },)
        # Account for possible double braces
        content_match <- regexpr("^[^}]+", remaining)
        if (content_match == -1) next

        content <- substring(remaining, 1, attr(content_match, "match.length"))
        suffix <- substring(remaining, attr(content_match, "match.length") + 1)

        # Apply title case to content
        new_content <- to_title_case(content,
                                     preserve_acronyms = preserve_acronyms,
                                     software_packages = software_packages)

        # Remove trailing period before closing brace if present
        new_content <- gsub("\\.$", "", new_content)

        # Protect for CSL (only for Title fields)
        if (protect_for_csl && grepl("^\\s*title\\s*=", line, ignore.case = TRUE)) {
            # Combine default and user-specified acronyms
            all_acronyms <- c(
                # Default protection list
                "AIDS", "HIV", "COVID", "SARS", "DNA", "RNA", "PCR",
                "US", "UK", "EU", "UN", "GDP", "GNP",
                "LGBTQ", "LGBT", "CEO", "NGO", "IRB",
                "OLS", "IV", "RCT", "DID", "RDD", "MLE", "MCMC",
                "API", "SQL", "HTML"
            )
            if (!is.null(csl_protect_acronyms)) {
                all_acronyms <- unique(c(all_acronyms, csl_protect_acronyms))
            }
            new_content <- protect_caps_for_csl(new_content,
                                                protect_acronyms = all_acronyms)
        }

        # Reconstruct the line
        lines[i] <- paste0(prefix, new_content, suffix)
    }

    # Clean up location names
    if (clean_locations) {
        lines <- gsub("New York, NY", "New York", lines)
        lines <- gsub("Chicago, IL", "Chicago", lines)
        lines <- gsub("Philadelphia, PA", "Philadelphia", lines)
        lines <- gsub("Princeton, NJ", "Princeton", lines)
        lines <- gsub("Cambridge, MA", "Cambridge", lines)
        lines <- gsub("Boston, MA", "Boston", lines)
        lines <- gsub("Los Angeles, CA", "Los Angeles", lines)
        lines <- gsub("Washington, DC", "Washington", lines)
        lines <- gsub(", USA$", "", lines)
    }

    # Clean up abbreviations
    if (clean_abbreviations) {
        lines <- gsub("Pr\\}", "Press}", lines)
        lines <- gsub("Pub\\}", "Publishers}", lines)
        lines <- gsub("Univ of|Univ\\. of|Univ\\. Of", "University of", lines)
        lines <- gsub("Press\\.\\}", "Press}", lines)
        lines <- gsub("Rev\\}", "Review}", lines)
        lines <- gsub("Soc'y", "Society", lines)

        # R package names should be in braces (lowercase)
        # Wrap any that got title-cased back to lowercase with braces
        for (pkg in software_packages) {
            # Match capitalized version and replace with braced lowercase
            pattern <- paste0("\\b", tools::toTitleCase(pkg), "\\b")
            replacement <- paste0("{", pkg, "}")
            lines <- gsub(pattern, replacement, lines)
        }

        # Fix LaTeX formatting
        lines <- gsub("\\$\\\\Times\\$", "$\\\\times$", lines)

        # Fix quote marks
        lines <- gsub(" '", " `", lines)
    }

    # Determine output file name
    if (is.null(output_file)) {
        timestamp <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
        dir_name <- dirname(input_file)
        base_name <- tools::file_path_sans_ext(basename(input_file))
        output_file <- file.path(dir_name, paste0(base_name, "_", timestamp, ".bib"))
    }

    # Write output
    writeLines(lines, output_file, useBytes = TRUE)
    message("Processed BibTeX file written to: ", output_file)

    invisible(output_file)
}
