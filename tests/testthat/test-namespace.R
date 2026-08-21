write_namespace <- function(lines) {
  dir <- tempfile("ns-")
  dir.create(dir)
  writeLines(lines, file.path(dir, "NAMESPACE"))
  dir
}

test_that("export(), quoted names and S3method() all count as exported", {
  dir <- write_namespace(c(
    "export(plain)",
    'export("%op%")',
    "S3method(print, thing)",
    "# export(commented_out)"
  ))
  exports <- rd2sphinx:::read_namespace_exports(dir)

  expect_true(rd2sphinx:::namespace_exports_alias("plain", exports))
  expect_true(rd2sphinx:::namespace_exports_alias("%op%", exports))
  expect_true(rd2sphinx:::namespace_exports_alias("print.thing", exports))
  expect_false(rd2sphinx:::namespace_exports_alias("commented_out", exports))
  expect_false(rd2sphinx:::namespace_exports_alias("hidden", exports))
})

test_that("exportPattern() is applied as a regular expression", {
  exports <- rd2sphinx:::read_namespace_exports(write_namespace('exportPattern("^yt_")'))

  expect_true(rd2sphinx:::namespace_exports_alias("yt_search", exports))
  expect_false(rd2sphinx:::namespace_exports_alias("internal_helper", exports))
})

test_that("a package with no NAMESPACE documents everything", {
  dir <- tempfile("no-ns-")
  dir.create(dir)
  exports <- rd2sphinx:::read_namespace_exports(dir)

  expect_true(exports$include_all)
  expect_true(rd2sphinx:::namespace_exports_alias("anything", exports))
})
