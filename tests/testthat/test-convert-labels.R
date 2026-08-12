test_that("default mappings normalize waves and preserve party levels", {
    clear_label_mappings()
    on.exit(clear_label_mappings(), add = TRUE)

    variables <- c(
        "age16", "female_20", "pid3", "pid7_24",
        "pid3Republican", "pid3Democrat", "pid3Independent"
    )

    expect_equal(
        convert_labels(variables, extracted = TRUE),
        c(
            "Age", "Female", "Party ID (3-cat)", "Party ID (7-point)",
            "Party: Republican", "Party: Democrat", "Party: Independent"
        )
    )
})

test_that("custom mappings have deterministic precedence", {
    clear_label_mappings()
    on.exit(clear_label_mappings(), add = TRUE)

    set_label_mappings(c("^age$" = "Original age"))
    set_label_mappings(c("^age$" = "Updated age", "^income" = "Earnings"))

    expect_equal(
        convert_labels(c("age", "income_10k"), extracted = TRUE),
        c("Updated age", "Earnings")
    )
    expect_equal(names(get_label_mappings()), c("^age$", "^income"))
})

test_that("custom mapping inputs are validated", {
    clear_label_mappings()
    on.exit(clear_label_mappings(), add = TRUE)

    expect_error(set_label_mappings("Age"), "non-empty regex")
    expect_error(set_label_mappings(stats::setNames("Age", "")), "non-empty regex")
    expect_error(
        set_label_mappings(structure(c("One", "Two"), names = c("^x$", "^x$"))),
        "unique"
    )
    expect_error(set_label_mappings(c("[" = "Invalid")), "Invalid regular expression")
    expect_error(set_label_mappings(list("^x$" = c("One", "Two"))), "exactly one")
    expect_error(set_label_mappings(c("^x$" = NA_character_)), "non-missing")
    expect_error(set_label_mappings(c("^x$" = "X"), append = NA), "single TRUE or FALSE")
})

test_that("an empty mapping set clears mappings when append is false", {
    set_label_mappings(c("^age$" = "Custom age"))
    on.exit(clear_label_mappings(), add = TRUE)

    set_label_mappings(list(), append = FALSE)

    expect_length(get_label_mappings(), 0L)
    expect_equal(convert_labels("age", extracted = TRUE), "Age")
})
