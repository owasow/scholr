# scholr 0.3.1

## Bug Fixes

* `convert_labels()` now labels party-ID factor levels correctly. The bare
  `^pid3` / `^pid7` scale patterns previously shadowed the level-specific rules,
  so `pid3Republican` came back as "Party ID (3-cat)" instead of a party label.
  Level patterns now run first, match full-word levels (`Republican`,
  `Democrat`, `Independent`) as well as abbreviations, and the scale patterns
  are anchored (`^pid3$`) so they only match the bare numeric variable.

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
