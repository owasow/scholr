# scholr

A collection of helper functions for creating publication-ready tables in R Markdown documents. Provides wrappers for `stargazer`, `kable`, and `xtable` with sensible defaults, automatic output format detection (LaTeX/HTML/text), and customizable variable label conversion for regression tables.
## Installation

```r
# Install from GitHub
devtools::install_github("owasow/scholr")
```

## Features

- **Auto-detect output format**: Functions automatically detect whether you're rendering to PDF (LaTeX), HTML, or text and adjust accordingly
- **Stargazer wrappers**: `star0()`, `star_ft()`, `star_sm()`, etc. with sensible defaults
- **Variable label conversion**: `convert_labels()` translates technical variable names to human-readable labels
- **Customizable mappings**: Register project-specific variable name mappings with `set_label_mappings()`
- **Kable helpers**: `kable0()`, `kable_striped()`, `kable_scaled()` with consistent styling
- **ANOVA helpers**: `print_anova()`, `print_models()` for ANOVA table output
- **glmer support**: `star_glmer()` creates proper stargazer tables from `lme4::glmer` models with correct delta-method standard errors for odds ratios
- **Inline reporting**: `b()`, `p()`, `se()`, `or()`, `bp()`, `orp()`, `z()`, `ci95()` for inline R Markdown results
- **Title case conversion**: `to_title_case()` and `bib_to_titlecase()` for converting text and BibTeX files to title case following NYT Manual of Style

## Quick Start

```r
library(scholr)

# Basic regression table with auto-detected format
model <- lm(mpg ~ wt + hp + cyl, data = mtcars)
star0(model)

# Extract and convert variable labels
labels <- star_var(model)
star_ft(model, covariate.labels = labels)
```

## Custom Label Mappings

The key feature is the ability to set project-specific variable label mappings:

```r
library(scholr)

# Register custom mappings for your project
set_label_mappings(c(
    "^immig_stcontext"  = "State Immigration Context",
    "^isss_5item"       = "ISSS (5-item)",
    "^likelydeported"   = "Likely Deported",
    "^avg_emotions"     = "ICE Emotions",
    "^health_fp"        = "Fair/Poor Health"
))

# Now convert_labels() and star_var() will use your mappings
model <- lm(health_fp ~ immig_stcontext + isss_5item, data = mydata)
labels <- star_var(model)
# Returns: c("State Immigration Context", "ISSS (5-item)")
```

### Mapping Syntax

Mappings use regex patterns as names and replacement strings as values:

```r
set_label_mappings(c(
    "^age$"           = "Age (years)",           # Exact match
    "^female"         = "Female",                # Starts with
    "income.*_10k$"   = "Income (\\$10K)",       # Pattern match
    ":wave$"          = " x Wave"                # Interaction suffix
))
```

### Default Mappings

The package includes defaults for common variables:
- Demographics: age, female, income, education, married, employed
- Race/ethnicity: Black, Hispanic, White, Asian, etc.
- Political: Party ID, ideology, feeling thermometers
- Region: West, Northeast, South, Midwest
- Study design: wave, time

Custom mappings take precedence over defaults.

## glmer Model Support

Stargazer doesn't properly handle `glmer` models from `lme4`. The `star_glmer()` function solves this by extracting coefficients and standard errors and using stargazer's override parameters:

```r
library(scholr)
library(lme4)

# Fit a mixed-effects logistic regression
model <- glmer(y ~ x1 + x2 + (1|group), data = mydata, family = binomial)

# Log-odds coefficients (default)
star_glmer(model)

# Odds ratios with delta-method standard errors
star_glmer(model, exponentiate = TRUE)

# Multiple models with custom labels
star_glmer(model1, model2, model3,
           exponentiate = TRUE,
           star_args = list(
               covariate.labels = c("Variable 1", "Variable 2"),
               title = "My Table"
           ))
```

The delta method correctly transforms standard errors when exponentiating:
`SE(OR) = exp(β) × SE(β)`

## Title Case Conversion

Convert text to title case following the New York Times Manual of Style:

```r
library(scholr)

to_title_case("the quick brown fox")
# [1] "The Quick Brown Fox"

to_title_case("a tale of two cities: the best of times")
# [1] "A Tale of Two Cities: The Best of Times"

# Preserves mixed-case words like iPhone, URLs, and emails
to_title_case("testing iPhone and http://example.com")
# [1] "Testing iPhone and http://example.com"
```

### BibTeX File Processing

Process BibTeX files to standardize title case in Title, Journal, Publisher, and Booktitle fields:

```r
# Creates a timestamped output file
bib_to_titlecase("references.bib")

# Or specify output file
bib_to_titlecase("references.bib", "references_clean.bib")
```

The function also cleans up:
- Location names: "New York, NY" becomes "New York"
- Abbreviations: "Univ." becomes "University", "Pr" becomes "Press"
- LaTeX formatting issues

## Format Detection Functions

```r
get_star_format()   # Returns "latex", "html", or "text"
get_kable_format()  # Returns "latex", "html", or "markdown"
get_xtable_format() # Returns "latex" or "html"
```

## Inline Reporting Helpers

Functions to streamline inline R code when reporting regression results in R Markdown:

```r
m <- glm(am ~ wt + hp, data = mtcars, family = binomial)

b(m, "wt")           # Coefficient: -8.08
se(m, "wt")          # Standard error: 3.07
p(m, "wt")           # P-value: "= 0.014" or "< .001"
or(m, "wt")          # Odds ratio: 0
z(m, "wt")           # Z-statistic: -2.63
ci95(m, "wt")        # 95% CI: "[-16.42, -3.03]"
ci95(m, "wt", exp = TRUE)  # OR 95% CI: "[0, 0.05]"

# Combined formats
bp(m, "wt")          # "b = -8.08, p = 0.014"
orp(m, "wt")         # "OR = 0, p = 0.014"
```

Use in R Markdown prose:
```
Weight significantly predicted transmission (`r bp(m, 'wt')`).
```
Renders as: "Weight significantly predicted transmission (b = -8.08, p = 0.014)."

## Utility Functions

```r
add_comma(1234567)         # "1,234,567"
number_to_word(3)          # "three"
round1(3.456)              # 3.5
round2(3.456)              # 3.46
na_to_dash(c(1, NA, 3))    # c("1", "-", "3")
na_to_blank(c(1, NA, 3))   # c("1", "", "3")
```

## License

MIT
