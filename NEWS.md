# scholr 0.3.1

* `set_label_mappings()` now upserts: re-registering a pattern replaces the
  existing entry instead of prepending a duplicate, so re-sourcing a project
  dictionary is idempotent.
* New `fixed = TRUE` argument in `set_label_mappings()`: treat names as exact
  variable names (regex-escaped and anchored `^name$`) so callers need no
  regex knowledge.
* New `set_label_mappings_from_file()`: load a pattern/label dictionary from
  a TSV or CSV file, keeping label crosswalks as data.
* New `warn_unmatched` argument in `convert_labels()` and `star_var()`: warn
  when a displayed term has no mapping, instead of silently passing the raw
  variable name through to the table.
* First testthat suite (label registry round-trips).

# scholr 0.3.0

## New Features

* Added `%nin%` operator - negation of `%in%` for checking if elements are NOT in a set

* Added `pval()` for categorical p-value formatting in LaTeX (e.g., "$p < 0.001$")

* Added `format_exp()` to format exponentiated coefficients as percent change,
  useful for interpreting logistic regression results

* Added `pseudo()` to calculate McFadden's pseudo R-squared for GLM model lists
  (requires pscl package)

* Added `stargazer2()` - stargazer wrapper that supports odds ratios with
  delta-method standard errors for GLM models

# scholr 0.2.0

## New Features

* Added `to_title_case()` for converting text to title case following NYT Manual
  of Style conventions. Handles small words (a, an, the, etc.), preserves URLs
  and email addresses, maintains mixed-case words like "iPhone", and properly
  capitalizes after colons and semicolons.

* Added `bib_to_titlecase()` for processing BibTeX files with automatic title
  case conversion of Title, Journal, Publisher, and Booktitle fields. Includes
  cleanup of location names (e.g., "New York, NY" to "New York") and expansion
  of common abbreviations (e.g., "Univ." to "University", "Pr" to "Press").

# scholr 0.1.0

* Initial release with table formatting utilities for R Markdown and LaTeX
* Helper functions for stargazer, kable, and xtable
* Automatic output format detection (LaTeX/HTML/text)
* Variable label conversion for regression tables
* Utility functions: `add_comma()`, `number_to_word()`, `round1()`, `round2()`,
  `na_to_dash()`, `na_to_blank()`
