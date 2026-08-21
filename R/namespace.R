read_namespace_exports <- function(path) {
  namespace <- file.path(path, "NAMESPACE")
  if (!file.exists(namespace)) {
    return(list(names = character(), patterns = character(), include_all = TRUE))
  }

  lines <- readLines(namespace, warn = FALSE, encoding = "UTF-8")
  lines <- trimws(sub("#.*$", "", lines))
  lines <- lines[nzchar(lines)]

  directive_values <- function(directive) {
    pattern <- paste0("^", directive, "\\((.*)\\)$")
    hits <- grep(pattern, lines, value = TRUE)
    if (!length(hits)) {
      return(character())
    }
    values <- sub(pattern, "\\1", hits)
    unlist(lapply(values, split_namespace_values), use.names = FALSE)
  }

  export_names <- unique(c(
    directive_values("export"),
    directive_values("exportMethods"),
    directive_values("exportClasses")
  ))

  patterns <- directive_values("exportPattern")
  s3_methods <- namespace_s3_methods(lines)

  list(
    names = unique(c(export_names, s3_methods)),
    patterns = patterns,
    include_all = !length(export_names) && !length(patterns) && !length(s3_methods)
  )
}

split_namespace_values <- function(x) {
  if (!length(x) || !nzchar(x)) {
    return(character())
  }

  values <- strsplit(x, ",", fixed = TRUE)[[1L]]
  values <- trimws(values)
  values <- gsub("^['\"`]|['\"`]$", "", values)
  values[nzchar(values)]
}

namespace_exports_alias <- function(alias, exports) {
  if (isTRUE(exports$include_all) || alias %in% exports$names) {
    return(TRUE)
  }

  if (length(exports$patterns)) {
    return(any(vapply(
      exports$patterns,
      function(pattern) grepl(pattern, alias, perl = TRUE),
      logical(1L)
    )))
  }

  FALSE
}


namespace_s3_methods <- function(lines) {
  hits <- grep("^S3method\\(", lines, value = TRUE)
  if (!length(hits)) {
    return(character())
  }

  methods <- vapply(hits, function(line) {
    inside <- sub("^S3method\\((.*)\\)$", "\\1", line)
    parts <- split_namespace_values(inside)
    if (length(parts) < 2L) {
      return("")
    }
    paste0(parts[[1L]], ".", parts[[2L]])
  }, character(1L), USE.NAMES = FALSE)
  unique(methods[nzchar(methods)])
}
