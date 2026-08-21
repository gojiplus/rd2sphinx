test_that("use_sphinx() writes a Furo-configured Sphinx tree", {
  fixture <- demo_fixture()
  use_sphinx(path = fixture, docs_dir = "docs", github_actions = FALSE)

  docs <- file.path(fixture, "docs")
  expect_true(file.exists(file.path(docs, "conf.py")))
  expect_true(file.exists(file.path(docs, "index.rst")))
  expect_true(file.exists(file.path(docs, "requirements.txt")))
  expect_true(file.exists(file.path(docs, "Makefile")))
  expect_true(file.exists(file.path(docs, "_static", "custom.css")))
  expect_true(file.exists(file.path(docs, "_ext", "rd2sphinx_domain.py")))

  conf <- paste(readLines(file.path(docs, "conf.py"), warn = FALSE), collapse = "\n")
  expect_match(conf, "html_theme = 'furo'", fixed = TRUE)
  expect_match(conf, "extensions = ['rd2sphinx_domain']", fixed = TRUE)
})

test_that("use_sphinx() gitignores the generated inputs", {
  fixture <- demo_fixture()
  use_sphinx(path = fixture, docs_dir = "docs", github_actions = FALSE)

  ignore <- readLines(file.path(fixture, ".gitignore"), warn = FALSE)
  expect_true("_site/" %in% ignore)
  expect_true("docs/reference/" %in% ignore)
  expect_true("docs/_ext/rd2sphinx_domain.py" %in% ignore)
  expect_true("__pycache__/" %in% ignore)
})

test_that("use_sphinx() leaves an existing conf.py alone unless told otherwise", {
  fixture <- demo_fixture()
  use_sphinx(path = fixture, docs_dir = "docs", github_actions = FALSE)
  conf <- file.path(fixture, "docs", "conf.py")
  writeLines("# hand edited", conf)

  use_sphinx(path = fixture, docs_dir = "docs", github_actions = FALSE)
  expect_identical(readLines(conf, warn = FALSE), "# hand edited")

  use_sphinx(path = fixture, docs_dir = "docs", github_actions = FALSE, overwrite = TRUE)
  expect_gt(length(readLines(conf, warn = FALSE)), 1L)
})

test_that("use_sphinx() rejects a directory that is not an R package", {
  empty <- tempfile("not-a-package-")
  dir.create(empty)
  expect_error(use_sphinx(path = empty), "No DESCRIPTION")
})
