# scholr 0.4.0

* `star_var()` no longer parses stargazer's printed text to learn the display
  order. A new default engine (`engine = "coef"`) derives the displayed
  covariate rows directly from the models' coefficient names, mirroring
  stargazer 5.2.3's internal merge, intercept relocation, `order` masking,
  and `keep`/`omit` logic (including the quirk that `keep` overrides `omit`).
  Verified against stargazer's actual printed output in the test suite.
* This fixes two latent bugs in the text-parsing approach: (1) with `keep=`
  the Constant row is suppressed, so the parser swept summary-statistics
  lines ("Observations", "R2", "Note:") into the label vector — stargazer
  silently discarded the excess, hiding the misalignment risk; (2) models
  built from paste()-constructed formulas leaked deparsed-call header junk
  into the first label.
* The legacy parser remains available via `engine = "text"` and is used
  automatically for model classes without a usable `coef()` method and for
  numeric `omit`/`keep` indices.
* `star_var()` also runs faster: it no longer executes stargazer twice per
  table.

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
