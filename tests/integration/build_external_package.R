args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) {
  stop(
    "Usage: build_external_package.R PACKAGE_PATH OUTPUT_PATH [SOURCE_BRANCH]",
    call. = FALSE
  )
}
package_path <- normalizePath(args[[1L]], mustWork = TRUE)
output_path <- normalizePath(args[[2L]], mustWork = FALSE)
source_branch <- if (length(args) >= 3L) args[[3L]] else NULL

docs_dir <- "sphinx-docs"
rd2sphinx::use_sphinx(
  path = package_path,
  docs_dir = docs_dir,
  overwrite = TRUE,
  github_actions = FALSE,
  source_branch = source_branch
)
rd2sphinx::build_sphinx(
  path = package_path,
  docs_dir = docs_dir,
  output_dir = output_path,
  include = "exports",
  warnings_as_errors = TRUE,
  keep_going = TRUE,
  fresh = TRUE
)

html <- list.files(output_path, pattern = "\\.html$", recursive = TRUE, full.names = TRUE)
stopifnot(length(html) > 1L)
stopifnot(file.exists(file.path(output_path, "index.html")))
stopifnot(file.exists(file.path(output_path, "objects.inv")))
stopifnot(file.exists(file.path(output_path, "genindex.html")))
stopifnot(file.exists(file.path(output_path, "searchindex.js")))
stopifnot(file.exists(file.path(output_path, "_static", "styles", "furo.css")))
message("Built ", length(html), " Furo HTML pages in ", output_path)
