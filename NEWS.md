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
