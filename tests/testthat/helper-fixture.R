# Copy the bundled demo package somewhere writable and hand back the path.
# Rendering writes into the package tree, so every test needs its own copy.
demo_fixture <- function() {
  source <- test_path("..", "fixtures", "demo")
  skip_if_not(dir.exists(source), "demo fixture not available")

  fixture <- tempfile("rd2sphinx-demo-")
  dir.create(fixture)
  files <- list.files(source, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  stopifnot(all(file.copy(files, fixture, recursive = TRUE)))
  fixture
}

rendered_page <- function(fixture, slug, docs_dir = "docs") {
  path <- file.path(fixture, docs_dir, "reference", paste0(slug, ".rst"))
  paste(readLines(path, warn = FALSE), collapse = "\n")
}
