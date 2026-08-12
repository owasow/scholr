# scholr

A collection of helper functions for creating publication-ready tables in R Markdown documents. Provides wrappers for `stargazer`, `kable`, and `xtable` with sensible defaults, automatic output format detection (LaTeX/HTML/text), and customizable variable label conversion for regression tables.
## Installation

```r
# Install from GitHub
devtools::install_github("owasow/scholr")
```

## Features

- **Auto-detect output format**: Functions automatically detect whether you're rendering to PDF (LaTeX), HTML, or text and adjust accordingly
- **Stargazer wrappers**: Ready-made table styles for compact output, omitted
  fixed effects, different font sizes, and odds ratios
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
star_compact(model)

# Extract and convert variable labels
labels <- star_var(model)
star_compact(model, covariate.labels = labels)
```

## Stargazer Wrappers

The wrappers share several publication-oriented defaults: they detect LaTeX,
HTML, or text output automatically; suppress the standalone LaTeX header; align
columns; use one significance threshold (`p < 0.05`); and replace stargazer's
default notes with a concise note.

| Function | Digits | LaTeX font | Distinctive behavior | Use it when |
|---|---:|---|---|---|
| `star_compact()` | 3 | `scriptsize` | Supports coefficient/statistic omission and custom notes | You want the standard compact scholr table |
| `star_small()` | 2 | `small` | Uses a larger font and fewer digits | The table is short enough to prioritize readability |
| `star_normal()` | 2 | normal | Uses Stargazer's normal font | The table is short and space is not constrained |
| `star_or()` | 3 | Stargazer default | Shows GLM odds ratios with delta-method SEs | You are reporting logistic-model odds ratios |

### `star_compact()`: standard table

```r
model <- lm(mpg ~ wt + hp + cyl, data = mtcars)
star_compact(model)

# Override any stargazer option through ...
star_compact(model, title = "Fuel Economy Models", dep.var.labels = "MPG")
```

Use `omit` to hide coefficient groups such as fixed effects, `omit.stat` to hide
model statistics, and `notes` to replace the default table note:

```r
model_fe <- lm(mpg ~ wt + hp + factor(cyl), data = mtcars)

star_compact(
    model_fe,
    omit = "^factor\\(cyl\\)",
    notes = "Cylinder fixed effects included. *p < 0.05"
)

# Replacement for the old star1() helper
star_normal(model, omit.stat = "theta")
```

When supplying `covariate.labels`, pass the same omission pattern to
`star_var()` so labels remain aligned:

```r
labels <- star_var(model_fe, omit = "^factor\\(cyl\\)")
star_compact(model_fe, omit = "^factor\\(cyl\\)", covariate.labels = labels)
```

### `star_small()` and `star_normal()`: more readable tables

Both use two digits rather than three. `star_small()` uses LaTeX's `small` font;
`star_normal()` leaves the font at Stargazer's normal size.

```r
star_small(model, title = "Fuel Economy")
star_normal(model, title = "Fuel Economy")
```

Font-size settings affect LaTeX output; HTML and text output still benefit from
the wrapper's other defaults.

### Deprecated wrapper names

The old names remain functional so existing documents continue to render, but
they emit deprecation warnings:

| Old name | Replacement |
|---|---|
| `star0()` | `star_compact()` |
| `star_ft()` | `star_compact()` |
| `star_sm()` | `star_small()` |
| `star_nrm()` | `star_normal()` |
| `star1()` | `star_normal(..., omit.stat = "theta")` |

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

Functions to streamline inline R code when reporting regression results. Two sets of functions cover both output formats:

| Plain text (Rmd) | LaTeX (Rnw) | Output |
|-------------------|-------------|--------|
| `p(model, "var")` | `pv(model, "var")` or `pv(0.03)` | p-value |
| `bp(model, "var")` | `bpv(model, "var")` | coefficient + p-value |

### Plain text helpers (for R Markdown)

```r
m <- glm(am ~ wt + hp, data = mtcars, family = binomial)

b(m, "wt")           # Coefficient: -8.08
se(m, "wt")          # Standard error: 3.07
p(m, "wt")           # P-value: "= .014" or "< .001"
or(m, "wt")          # Odds ratio: 0
z(m, "wt")           # Z-statistic: -2.63
ci95(m, "wt")        # 95% CI: "[-16.42, -3.03]"
ci95(m, "wt", exp = TRUE)  # OR 95% CI: "[0, 0.05]"

# Combined formats
bp(m, "wt")          # "b = -8.08, p = .014"
orp(m, "wt")         # "OR = 0, p = .014"
```

Use in R Markdown prose:
```
Weight significantly predicted transmission (`r bp(m, 'wt')`).
```
Renders as: "Weight significantly predicted transmission (b = -8.08, p = .014)."

### LaTeX helpers (for Rnw/Sweave)

```r
m <- lm(mpg ~ wt, data = mtcars)

# From a model object
pv(m, "wt")          # "$p$ < 0.001"
bpv(m, "wt")         # "$b$ = -5.34, $p$ < 0.001"

# From a raw numeric p-value
pv(0.03)             # "$p$ < 0.05"
pv(0.002)            # "$p$ < 0.01"
pv(0.072)            # "$p$ = 0.072"
```

Use in .Rnw files:
```
The effect was significant (\Sexpr{bpv(m, "wt")}).
```
Renders as: "The effect was significant ($b$ = -5.34, $p$ < 0.001)."

## Utility Functions

```r
add_comma(1234567)         # "1,234,567"
number_to_word(3)          # "three"
round1(3.456)              # 3.5
round2(3.456)              # 3.46
na_to_dash(c(1, NA, 3))    # c("1", "-", "3")
na_to_blank(c(1, NA, 3))   # c("1", "", "3")
c(1, 2, 3) %nin% c(2, 4)   # TRUE FALSE TRUE ("not in" operator)
```

## P-value and Effect Size Formatting

```r
# Categorical p-value for LaTeX (accepts numeric, tibble, or matrix input)
pval(0.001)                # "$p < 0.001$"
pval(0.03)                 # "$p < 0.05$"

# Percent change from exponentiated coefficient (logistic regression)
m <- glm(am ~ wt, data = mtcars, family = binomial)
format_exp(m, "wt")        # "-99.97" (i.e., 99.97% decrease in odds)

# Pseudo R-squared for GLM model lists
pseudo(list(m1, m2))       # c("0.48", "0.52")
```

## Odds Ratio Tables

For GLM models with odds ratios and delta-method standard errors:

```r
m <- glm(am ~ wt + hp, data = mtcars, family = binomial)

# Odds ratios with delta-method SEs (default)
star_or(m)

# Log-odds, when needed
star_or(m, odds.ratio = FALSE)
```

For mixed-effects models (glmer), use `star_glmer()` instead.

## License

MIT
