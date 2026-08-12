test_that("texcount reports missing source files without invoking a command", {
    expect_warning(
        result <- tc_count(file.path(tempdir(), "does-not-exist.tex")),
        "TeX file not found"
    )

    expect_true(is.na(result$total))
    expect_equal(result$total_formatted, "[compile twice for word count]")
})

test_that("texcount safely handles paths containing spaces", {
    skip_on_os("windows")

    fake_bin <- tempfile("fake-texcount-bin-")
    dir.create(fake_bin)
    fake_texcount <- file.path(fake_bin, "texcount")
    writeLines(
        c(
            "#!/bin/sh",
            "[ \"$#\" -eq 5 ] || exit 9",
            "[ -f \"$5\" ] || exit 10",
            "printf '%s\\n' 'Words in text: 100'",
            "printf '%s\\n' 'Words in headers: 10'",
            "printf '%s\\n' 'Words outside text: 5'"
        ),
        fake_texcount
    )
    Sys.chmod(fake_texcount, mode = "0755")

    source_dir <- tempfile("tex source ")
    dir.create(source_dir)
    source_file <- file.path(source_dir, "paper draft.tex")
    writeLines("Sample", source_file)

    old_path <- Sys.getenv("PATH")
    on.exit(Sys.setenv(PATH = old_path), add = TRUE)
    Sys.setenv(PATH = paste(fake_bin, old_path, sep = .Platform$path.sep))

    result <- tc_count(source_file, include_headers = TRUE)

    expect_equal(result$text, 100)
    expect_equal(result$headers, 10)
    expect_equal(result$outside, 5)
    expect_equal(result$total, 115)
    expect_equal(result$total_formatted, "115")
})
