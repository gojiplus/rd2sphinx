#' Build Sphinx documentation for an R package
#'
#' Render the package's Rd reference pages and invoke `sphinx-build`.
#'
#' @param path Path to an R package source directory.
#' @param docs_dir Sphinx source directory, relative to `path` by default.
#' @param output_dir Output directory, relative to `path` by default.
#' @param builder Sphinx builder name, usually `"html"`.
#' @param include Passed to [render_sphinx()].
#' @param warnings_as_errors Pass `-W` to Sphinx.
#' @param keep_going Ask Sphinx to report all warnings before failing.
#' @param fresh Ignore the saved Sphinx environment.
#' @param sphinx_build Path to the `sphinx-build` executable.
#' @param quiet Suppress the render summary.
#' @return Invisibly, the normalized output directory.
#' @export
build_sphinx <- function(
  path = ".",
  docs_dir = "docs",
  output_dir = "_site",
  builder = "html",
  include = c("exports", "all"),
  warnings_as_errors = TRUE,
  keep_going = TRUE,
  fresh = FALSE,
  sphinx_build = Sys.which("sphinx-build"),
  quiet = FALSE) {
  include <- match.arg(include)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  docs_path <- rd2sphinx_path(docs_dir, path)
  output_path <- rd2sphinx_path(output_dir, path)

  render_sphinx(
    path = path,
    docs_dir = docs_dir,
    include = include,
    quiet = quiet
  )

  if (!length(sphinx_build) || !nzchar(sphinx_build)) {
    stop(
      "sphinx-build was not found. Install docs/requirements.txt with Python first.",
      call. = FALSE
    )
  }

  args <- c(
    if (isTRUE(warnings_as_errors)) "-W" else character(),
    if (isTRUE(keep_going)) "--keep-going" else character(),
    if (isTRUE(fresh)) "-E" else character(),
    "-b", builder,
    docs_path,
    output_path
  )

  status <- system2(sphinx_build, args = args)
  if (!identical(status, 0L)) {
    stop("Sphinx build failed with exit status ", status, ".", call. = FALSE)
  }

  invisible(output_path)
}
