args <- commandArgs(trailingOnly = TRUE)
package_root <- normalizePath(
  if (length(args)) args[[1L]] else "rd2sphinx",
  mustWork = TRUE
)
fixture_source <- file.path(package_root, "tests", "fixtures", "demo")
fixture <- tempfile("rd2sphinx-demo-")
dir.create(fixture)
files <- list.files(fixture_source, all.files = TRUE, no.. = TRUE, full.names = TRUE)
file.copy(files, fixture, recursive = TRUE)

rd2sphinx::use_sphinx(path = fixture, docs_dir = "docs", github_actions = FALSE)
generated <- rd2sphinx::render_sphinx(path = fixture, docs_dir = "docs", include = "exports")

reference <- file.path(fixture, "docs", "reference")
add_page <- file.path(reference, "add_numbers.rst")
data_page <- file.path(reference, "demo_data.rst")
infix_page <- file.path(reference, "plus.rst")
replacement_page <- file.path(reference, "demo_label.rst")
internal_page <- file.path(reference, "internal_helper.rst")

stopifnot(
  file.exists(add_page),
  file.exists(data_page),
  file.exists(infix_page),
  file.exists(replacement_page),
  !file.exists(internal_page)
)
add_text <- paste(readLines(add_page, warn = FALSE), collapse = "\n")
stopifnot(grepl(".. r:function:: add_numbers", add_text, fixed = TRUE))
stopifnot(grepl(":r:obj:`demo_data`", add_text, fixed = TRUE))
stopifnot(length(generated) == 5L)
infix_text <- paste(readLines(infix_page, warn = FALSE), collapse = "\n")
replacement_text <- paste(readLines(replacement_page, warn = FALSE), collapse = "\n")
stopifnot(
  grepl(":canonical: %plus%", infix_text, fixed = TRUE),
  grepl(".. r:function:: demo_label(x) <- value", replacement_text, fixed = TRUE),
  grepl(":canonical: demo_label<-", replacement_text, fixed = TRUE)
)

site <- file.path(fixture, "_site")
status <- system2(
  Sys.which("sphinx-build"),
  c("-W", "--keep-going", "-b", "html", file.path(fixture, "docs"), site)
)
stopifnot(identical(status, 0L))
stopifnot(file.exists(file.path(site, "index.html")))
stopifnot(file.exists(file.path(site, "objects.inv")))
stopifnot(file.exists(file.path(site, "genindex.html")))
stopifnot(file.exists(file.path(site, "searchindex.js")))
stopifnot(file.exists(file.path(site, "_static", "styles", "furo.css")))

artifact <- file.path(package_root, "tests", "integration", "_site")
unlink(artifact, recursive = TRUE)
dir.create(dirname(artifact), recursive = TRUE, showWarnings = FALSE)
file.copy(site, artifact, recursive = TRUE)
message("Built Furo integration site at ", artifact)
