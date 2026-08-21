# Deliberately not named `%||%`: base R gained that operator in 4.4 with
# NULL-only semantics, and this one also treats empty, NA and "" as absent.
# Shadowing the base operator with a different rule is a trap.
first_nonempty <- function(...) {
  candidates <- list(...)
  for (candidate in candidates) {
    if (is.null(candidate) || length(candidate) == 0L) {
      next
    }
    first <- as.character(candidate[[1L]])[[1L]]
    if (!is.na(first) && nzchar(first)) {
      return(candidate)
    }
  }
  if (length(candidates)) candidates[[length(candidates)]] else NULL
}

rd2sphinx_path <- function(path, root) {
  if (grepl("^(/|[A-Za-z]:[/\\\\])", path)) {
    normalizePath(path, winslash = "/", mustWork = FALSE)
  } else {
    normalizePath(file.path(root, path), winslash = "/", mustWork = FALSE)
  }
}

read_package_description <- function(path) {
  description <- file.path(path, "DESCRIPTION")
  if (!file.exists(description)) {
    stop("No DESCRIPTION file found at ", normalizePath(path, mustWork = FALSE), call. = FALSE)
  }
  as.list(read.dcf(description)[1L, , drop = TRUE])
}

ensure_dir <- function(path) {
  if (!dir.exists(path) && !dir.create(path, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create directory: ", path, call. = FALSE)
  }
  invisible(path)
}

write_utf8 <- function(text, path) {
  ensure_dir(dirname(path))
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(text), con = con, useBytes = TRUE)
  invisible(path)
}

write_if_allowed <- function(text, path, overwrite = FALSE) {
  if (file.exists(path) && !isTRUE(overwrite)) {
    return(invisible(FALSE))
  }
  write_utf8(text, path)
  invisible(TRUE)
}

append_unique_lines <- function(path, lines) {
  old <- if (file.exists(path)) readLines(path, warn = FALSE, encoding = "UTF-8") else character()
  add <- setdiff(lines, old)
  if (length(add)) {
    spacer <- if (length(old) && nzchar(tail(old, 1L))) "" else character()
    write_utf8(c(old, spacer, add), path)
  }
  invisible(path)
}

collapse_whitespace <- function(x) {
  x <- gsub("\\r", "", x, fixed = FALSE)
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
  trimws(x)
}

clean_block <- function(x) {
  if (!length(x)) {
    return("")
  }
  x <- paste(x, collapse = "\n")
  if (is.na(x) || !nzchar(x)) {
    return("")
  }

  lines <- strsplit(gsub("\r", "", enc2utf8(x)), "\n", fixed = TRUE)[[1L]]
  lines <- sub("[ \t]+$", "", lines, perl = TRUE)

  while (length(lines) && !nzchar(lines[[1L]])) lines <- lines[-1L]
  while (length(lines) && !nzchar(lines[[length(lines)]])) lines <- lines[-length(lines)]
  if (!length(lines)) {
    return("")
  }

  nonblank <- lines[nzchar(trimws(lines))]
  indent <- if (length(nonblank)) {
    min(nchar(nonblank) - nchar(sub("^[ \t]+", "", nonblank, perl = TRUE)))
  } else {
    0L
  }
  if (is.finite(indent) && indent > 0L) {
    lines <- substring(lines, indent + 1L)
  }

  x <- paste(lines, collapse = "\n")
  gsub("\n{3,}", "\n\n", x, perl = TRUE)
}

indent_lines <- function(x, spaces = 3L) {
  if (!length(x)) {
    return("")
  }
  x <- paste(x, collapse = "\n")
  if (is.na(x) || !nzchar(x)) {
    return("")
  }
  prefix <- strrep(" ", spaces)
  lines <- split_lines(x)
  # A blank line stays blank: prefixing it only leaves trailing whitespace in
  # the generated file.
  indented <- ifelse(nzchar(lines), paste0(prefix, lines), lines)
  paste(indented, collapse = "\n")
}

rst_heading <- function(title, marker = "=") {
  c(title, strrep(marker, max(1L, nchar(title, type = "width"))))
}

rst_literal <- function(x) {
  x <- gsub("`", "'", x, fixed = TRUE)
  paste0("``", x, "``")
}

# strsplit() drops a trailing empty field, so "a\n" comes back as "a" and a
# lone "\n" comes back as "". Rd text nodes routinely are exactly that, and
# losing the newline runs consecutive paragraphs together.
split_lines <- function(x) {
  parts <- strsplit(x, "\n", fixed = TRUE)[[1L]]
  if (!length(parts)) {
    parts <- ""
  }
  if (endsWith(x, "\n")) parts <- c(parts, "")
  parts
}

escape_rst_plain <- function(x) {
  escape_characters <- function(value) {
    chars <- strsplit(value, "", fixed = TRUE)[[1L]]
    if (!length(chars)) {
      return("")
    }
    escaped <- vapply(
      chars,
      function(char) {
        if (char %in% c("\\", "*", "_", "|")) paste0("\\", char) else char
      },
      character(1L),
      USE.NAMES = FALSE
    )
    paste(escaped, collapse = "")
  }

  lines <- split_lines(x)
  lines <- vapply(lines, escape_characters, character(1L), USE.NAMES = FALSE)
  special <- grepl("^(\\.\\.[[:space:]]|:[^:]+:|[-+] |#\\. )", lines, perl = TRUE)
  lines[special] <- paste0("\\", lines[special])
  paste(lines, collapse = "\n")
}

replace_fixed_once <- function(x, pattern, replacement) {
  hit <- regexpr(pattern, x, fixed = TRUE)
  match <- hit[[1L]]
  if (match < 0L) {
    return(x)
  }

  match_length <- attr(hit, "match.length")[[1L]]
  before <- if (match > 1L) substr(x, 1L, match - 1L) else ""
  after_start <- match + match_length
  after <- if (after_start <= nchar(x)) substr(x, after_start, nchar(x)) else ""
  paste0(before, replacement, after)
}

rst_text <- function(x) {
  if (!length(x)) {
    return("")
  }
  x <- paste(x, collapse = "")
  if (is.na(x) || !nzchar(x)) {
    return("")
  }
  x <- enc2utf8(x)

  matches <- gregexpr("`[^`\\n]+`", x, perl = TRUE)[[1L]]
  code <- character()
  placeholders <- character()
  if (!identical(matches[[1L]], -1L)) {
    matched <- regmatches(x, list(matches))[[1L]]
    code <- substring(matched, 2L, nchar(matched) - 1L)
    placeholders <- paste0("\uE000", seq_along(code), "\uE001")
    regmatches(x, list(matches)) <- list(placeholders)
  }

  x <- escape_rst_plain(x)
  if (length(code)) {
    for (i in seq_along(code)) {
      x <- replace_fixed_once(x, placeholders[[i]], rst_literal(code[[i]]))
    }
  }
  x
}

# Slugs become filenames and URL segments. A leading dot would make a hidden
# file, which "[.tuber_result" produces and which several static hosts refuse
# to serve; a name made only of operator characters, such as "%>%", would
# otherwise collapse to a constant.
operator_words <- c(
  "%" = "pct", ">" = "gt", "<" = "lt", "=" = "eq", "!" = "not",
  "&" = "and", "|" = "or", "+" = "plus", "-" = "minus", "*" = "star",
  "/" = "slash", "^" = "hat", "$" = "dollar", "@" = "at",
  "[" = "sub", "]" = "sub", "?" = "help", "~" = "tilde", ":" = "colon"
)

slugify <- function(x) {
  slug <- gsub("[^A-Za-z0-9_.-]+", "-", x)
  slug <- gsub("^[-.]+|-+$", "", slug)
  if (nzchar(slug)) {
    return(slug)
  }

  chars <- strsplit(x, "", fixed = TRUE)[[1L]]
  words <- unname(operator_words[chars])
  words <- words[!is.na(words)]
  if (length(words)) paste(words, collapse = "-") else "topic"
}


detect_git_branch <- function(path, fallback = "main") {
  git <- Sys.which("git")
  if (!length(git) || !nzchar(git)) {
    return(fallback)
  }

  branch <- tryCatch(
    suppressWarnings(system2(
      git,
      c("-C", path, "rev-parse", "--abbrev-ref", "HEAD"),
      stdout = TRUE,
      stderr = FALSE
    )),
    error = function(...) character()
  )
  branch <- trimws(first_nonempty(branch[[1L]], ""))
  if (!nzchar(branch) || identical(branch, "HEAD")) fallback else branch
}

python_repr <- function(x) {
  chars <- strsplit(enc2utf8(x), "", fixed = TRUE)[[1L]]
  escaped <- vapply(
    chars,
    function(char) {
      if (identical(char, "\\")) {
        "\\\\"
      } else if (identical(char, "'")) {
        "\\'"
      } else {
        char
      }
    },
    character(1L),
    USE.NAMES = FALSE
  )
  paste0("'", paste(escaped, collapse = ""), "'")
}
