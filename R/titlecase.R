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
#'
#' @return Character string in title case
#' @export
#'
#' @details
#' The function handles several special cases:
#' \itemize{
#'   \item URLs, email addresses, and domains are preserved as-is
#'   \item Words with internal capitals (e.g., "iPhone", "McDonald's") are preserved
#'   \item Small words are capitalized at the start/end of titles and after : ; ? !
#'   \item Handles text in parentheses, brackets, and quotes appropriately
#' }
#'
#' @examples
#' to_title_case("the quick brown fox")
#' to_title_case("a tale of two cities")
#' to_title_case("the iPhone: a revolution in mobile technology")
#'
to_title_case <- function(text,
                          small_words = c("a", "an", "and", "as", "at", "but",
                                          "by", "en", "for", "if", "in", "of",
                                          "on", "or", "the", "to", "v", "v.",
                                          "via", "vs", "vs.", "with")) {

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

        # Preserve words with internal capitals
        if (has_internal_caps(word)) {
            result_words[i] <- word
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

    # Capitalize first word of title
    result <- sub("^([^a-zA-Z]*)([a-z])", "\\1\\U\\2", result, perl = TRUE)

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
#'   publisher abbreviations (e.g., "Pr" becomes "Press").
#' @param fields_to_process Character vector of BibTeX field names to process.
#'   Defaults to c("Title", "Journal", "Publisher", "Booktitle").
#'
#' @return Invisibly returns the path to the output file
#' @export
#'
#' @examples
#' \dontrun{
#' # Process a BibTeX file
#' bib_to_titlecase("references.bib")
#'
#' # Specify output file
#' bib_to_titlecase("references.bib", "references_cleaned.bib")
#'
#' # Process only Title fields
#' bib_to_titlecase("references.bib", fields_to_process = "Title")
#' }
#'
bib_to_titlecase <- function(input_file,
                             output_file = NULL,
                             clean_locations = TRUE,
                             clean_abbreviations = TRUE,
                             fields_to_process = c("Title", "Journal",
                                                   "Publisher", "Booktitle")) {

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
        new_content <- to_title_case(content)

        # Remove trailing period before closing brace if present
        new_content <- gsub("\\.$", "", new_content)

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
        lines <- gsub("Ggridges", "{ggridges}", lines)
        lines <- gsub("Ggplot2", "{ggplot2}", lines)

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
