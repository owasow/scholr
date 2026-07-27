test_that("set_label_mappings upserts instead of duplicating", {
  clear_label_mappings()
  set_label_mappings(c("^x$" = "First"))
  set_label_mappings(c("^x$" = "Second"))
  m <- get_label_mappings()
  expect_equal(sum(names(m) == "^x$"), 1)
  expect_equal(unname(m["^x$"]), "Second")
  clear_label_mappings()
})

test_that("fixed = TRUE anchors and escapes exact names", {
  clear_label_mappings()
  set_label_mappings(c("var.one" = "Var One"), fixed = TRUE)
  expect_equal(convert_labels("var.one", extracted = TRUE), "Var One")
  expect_equal(convert_labels("varXone", extracted = TRUE,
                              use_defaults = FALSE), "varXone")
  clear_label_mappings()
})

test_that("warn_unmatched flags unmapped terms", {
  clear_label_mappings()
  set_label_mappings(c("^known$" = "Known"))
  expect_warning(
    convert_labels(c("known", "zz_unmapped_zz"), extracted = TRUE,
                   warn_unmatched = TRUE),
    "zz_unmapped_zz")
  clear_label_mappings()
})

test_that("set_label_mappings_from_file reads a TSV dictionary", {
  clear_label_mappings()
  tf <- tempfile(fileext = ".tsv")
  writeLines(c("pattern\tlabel", "^abc$\tThe ABC"), tf)
  set_label_mappings_from_file(tf)
  expect_equal(convert_labels("abc", extracted = TRUE), "The ABC")
  clear_label_mappings()
})
