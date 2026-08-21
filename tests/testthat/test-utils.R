test_that("split_lines() keeps a trailing newline strsplit() would drop", {
  expect_identical(rd2sphinx:::split_lines("a\n"), c("a", ""))
  expect_identical(rd2sphinx:::split_lines("\n"), c("", ""))
  expect_identical(rd2sphinx:::split_lines("a\nb"), c("a", "b"))
  expect_identical(rd2sphinx:::split_lines(""), "")
})

test_that("slugify() never produces a hidden file or an empty name", {
  # "[.tuber_result" used to become ".tuber_result.rst", which several static
  # hosts refuse to serve; "%>%" used to collapse to the constant "topic".
  expect_identical(rd2sphinx:::slugify("[.tuber_result"), "tuber_result")
  expect_identical(rd2sphinx:::slugify("yt_search"), "yt_search")
  expect_identical(rd2sphinx:::slugify("%plus%"), "plus")
  expect_false(startsWith(rd2sphinx:::slugify("%>%"), "."))
  expect_true(nzchar(rd2sphinx:::slugify("%>%")))
  expect_false(identical(rd2sphinx:::slugify("%>%"), rd2sphinx:::slugify("%in%")))
})

test_that("usage_name() recovers the object being documented", {
  expect_identical(rd2sphinx:::usage_name("add_numbers(x, y = 1)", "fallback"), "add_numbers")
  expect_identical(rd2sphinx:::usage_name("demo_label(x) <- value", "fallback"), "demo_label<-")
  expect_identical(rd2sphinx:::usage_name("x %plus% y", "fallback"), "%plus%")
  expect_identical(rd2sphinx:::usage_name("not a call", "fallback"), "fallback")
})

test_that("split_usage() splits signatures without breaking nested calls", {
  usage <- "f(x = c(1, 2),\n  y = 3)\ng(z)"
  expect_identical(rd2sphinx:::split_usage(usage), c("f(x = c(1, 2), y = 3)", "g(z)"))
})

test_that("rst_text() escapes markup characters but keeps backtick code", {
  expect_identical(rd2sphinx:::rst_text("a * b"), "a \\* b")
  expect_identical(rd2sphinx:::rst_text("snake_case"), "snake\\_case")
  expect_identical(rd2sphinx:::rst_text("`code`"), "``code``")
})
