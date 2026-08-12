#' TeXcount Word Count Functions
#'
#' Functions to count words in LaTeX/R Markdown documents using texcount.
#' Requires texcount to be installed (included with most TeX distributions).
#'
#' @name texcount
NULL

#' Count Words in a TeX Document
#'
#' Runs texcount on a .tex file and returns parsed word counts.
#' For R Markdown documents, this counts the .tex file from a previous render.
#'
#' @param file Path to .tex file. If NULL, attempts to find .tex file matching
#'   current knitr input file.
#' @param include_bib Logical. Include bibliography in count? Default TRUE.
#' @param include_headers Logical. Include header words in total? Default FALSE.
#'
#' @return A list with components:
#'   \item{text}{Words in text (body)}
#'   \item{headers}{Words in headers}
#'   \item{outside}{Words outside text (footnotes, captions)}
#'   \item{total}{Total word count (text + outside, optionally + headers)}
#'   \item{raw}{Raw texcount output}
#'
#' @export
#' @examples
#' \dontrun{
#' # In an R Markdown document:
#' counts <- tc_count()
#' counts$total
#'
#' # Specify file directly:
#' counts <- tc_count("my_paper.tex")
#' }
tc_count <- function(file = NULL, include_bib = TRUE, include_headers = FALSE) {

    # Find file if not specified
    if (is.null(file)) {
        if (requireNamespace("knitr", quietly = TRUE) && !is.null(knitr::current_input())) {
            file <- sub("\\.[Rr]md$", ".tex", knitr::current_input())
        } else {
            stop("No file specified and cannot detect current input file.")
        }
    }

    # Check file exists
    if (!file.exists(file)) {
        warning("TeX file not found: ", file, "\nCompile document first to generate .tex file.")
        return(list(
            text = NA,
            headers = NA,
            outside = NA,
            total = NA,
            total_formatted = "[compile twice for word count]",
            raw = NULL
        ))
    }

    # Check texcount is available
    texcount <- Sys.which("texcount")
    if (!nzchar(texcount)) {
        warning("texcount not found. Install TeX distribution or texcount separately.")
        return(list(
            text = NA,
            headers = NA,
            outside = NA,
            total = NA,
            total_formatted = "[texcount not installed]",
            raw = NULL
        ))
    }

    # Pass the executable separately and quote the path as one safe argument.
    args <- c("-inc", "-total", "-sum")
    if (include_bib) args <- c("-inc", "-incbib", "-total", "-sum")
    tc_out <- suppressWarnings(system2(
        texcount,
        args = c(args, shQuote(file)),
        stdout = TRUE,
        stderr = TRUE
    ))
    status <- attr(tc_out, "status")
    if (!is.null(status) && status != 0L) {
        warning(
            "texcount failed with status ", status, ":\n",
            paste(tc_out, collapse = "\n")
        )
        return(list(
            text = NA,
            headers = NA,
            outside = NA,
            total = NA,
            total_formatted = "[texcount failed]",
            raw = tc_out
        ))
    }

    # Parse output
    extract_count <- function(pattern) {
        line <- grep(pattern, tc_out, value = TRUE)
        if (length(line) == 0) return(NA)
        as.numeric(sub(".*?(\\d+).*", "\\1", line[1]))
    }

    text_count <- extract_count("Words in text")
    header_count <- extract_count("Words in headers")
    outside_count <- extract_count("Words outside text")

    # Calculate total
    total <- sum(c(text_count, outside_count), na.rm = TRUE)
    if (include_headers) {
        total <- sum(c(total, header_count), na.rm = TRUE)
    }

    list(
        text = text_count,
        headers = header_count,
        outside = outside_count,
        total = total,
        total_formatted = format(total, big.mark = ",", scientific = FALSE),
        raw = tc_out
    )
}

#' Get Formatted Word Count
#'
#' Convenience function that returns just the formatted word count string.
#' Useful for inline R code in documents.
#'
#' @inheritParams tc_count
#'
#' @return Character string with formatted word count (e.g., "8,542")
#' @export
#' @examples
#' \dontrun{
#' # In R Markdown inline code:
#' # Word count: `r tc_words()`
#' }
tc_words <- function(file = NULL, include_bib = TRUE, include_headers = FALSE) {
    tc_count(file = file, include_bib = include_bib, include_headers = include_headers)$total_formatted
}
