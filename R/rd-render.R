rd_tag <- function(x) {
  tag <- attr(x, "Rd_tag", exact = TRUE)
  if (is.null(tag)) "" else sub("^\\\\", "", tag)
}

rd_option <- function(x) {
  option <- attr(x, "Rd_option", exact = TRUE)
  if (is.null(option)) "" else collapse_whitespace(paste(option, collapse = ""))
}

rd_sections <- function(rd, tag) {
  Filter(function(x) identical(rd_tag(x), tag), rd)
}

rd_section <- function(rd, tag) {
  sections <- rd_sections(rd, tag)
  if (length(sections)) sections[[1L]] else NULL
}


rd_contains_tag <- function(x, tag) {
  if (is.null(x) || !length(x)) {
    return(FALSE)
  }
  if (identical(rd_tag(x), tag)) {
    return(TRUE)
  }
  if (is.character(x)) {
    return(FALSE)
  }
  any(vapply(x, rd_contains_tag, logical(1L), tag = tag))
}

rd_raw <- function(x) {
  if (is.null(x) || !length(x)) {
    return("")
  }

  tag <- rd_tag(x)
  if (is.character(x)) {
    if (identical(tag, "dots")) {
      return("...")
    }
    return(paste(x, collapse = ""))
  }

  if (tag %in% c("dots", "ldots")) {
    return("...")
  }

  if (tag %in% c("method", "S3method", "S4method")) {
    parts <- vapply(x, rd_raw, character(1L), USE.NAMES = FALSE)
    parts <- parts[nzchar(parts)]
    if (length(parts) >= 2L) {
      return(paste0(parts[[1L]], ".", parts[[2L]], paste0(parts[-c(1L, 2L)], collapse = "")))
    }
  }

  paste(vapply(x, rd_raw, character(1L), USE.NAMES = FALSE), collapse = "")
}

rd_inline <- function(x, aliases = list()) {
  if (is.null(x) || !length(x)) {
    return("")
  }

  tag <- rd_tag(x)
  if (is.character(x)) {
    text <- paste(x, collapse = "")
    if (tag %in% c("RCODE", "VERB")) {
      return(rst_literal(collapse_whitespace(text)))
    }
    return(rst_text(text))
  }

  inner <- function() {
    paste(vapply(x, rd_inline, character(1L), aliases = aliases, USE.NAMES = FALSE), collapse = "")
  }

  if (tag %in% c("dots", "ldots")) {
    return(rst_literal("..."))
  }

  if (tag == "code") {
    if (rd_contains_tag(x, "link")) {
      return(clean_inline(inner()))
    }
    return(rst_literal(collapse_whitespace(rd_raw(x))))
  }

  if (tag %in% c("command", "env", "file", "kbd", "option", "samp")) {
    return(rst_literal(collapse_whitespace(rd_raw(x))))
  }

  if (tag == "pkg") {
    return(rst_literal(collapse_whitespace(rd_raw(x))))
  }

  if (tag == "emph") {
    return(paste0("*", clean_inline(inner()), "*"))
  }

  if (tag == "strong") {
    return(paste0("**", clean_inline(inner()), "**"))
  }

  if (tag == "link") {
    display <- collapse_whitespace(rd_raw(x))
    option <- rd_option(x)
    target <- display
    package <- ""

    if (startsWith(option, "=")) {
      target <- substring(option, 2L)
    } else if (nzchar(option) && grepl(":", option, fixed = TRUE)) {
      parts <- strsplit(option, ":", fixed = TRUE)[[1L]]
      package <- parts[[1L]]
      target <- paste(parts[-1L], collapse = ":")
    } else if (nzchar(option)) {
      package <- option
    }

    target <- sub("\\(\\)$", "", target)
    qualified <- if (nzchar(package)) paste0(package, "::", target) else target
    canonical <- first_nonempty(
      aliases[[qualified]], aliases[[target]], aliases[[display]]
    )
    if (!is.null(canonical) && nzchar(canonical)) {
      if (identical(display, canonical) || identical(display, paste0(canonical, "()"))) {
        return(paste0(":r:obj:`", canonical, "`"))
      }
      return(paste0(":r:obj:`", display, " <", canonical, ">`"))
    }

    return(rst_literal(if (nzchar(package)) qualified else display))
  }

  if (tag == "url") {
    return(collapse_whitespace(rd_raw(x)))
  }

  if (tag == "email") {
    email <- collapse_whitespace(rd_raw(x))
    return(paste0("`", email, " <mailto:", email, ">`_"))
  }

  if (tag == "href") {
    parts <- lapply(x, rd_raw)
    if (length(parts) >= 2L) {
      url <- collapse_whitespace(parts[[1L]])
      label <- collapse_whitespace(paste(unlist(parts[-1L]), collapse = ""))
      return(paste0("`", label, " <", url, ">`_"))
    }
    return(inner())
  }

  if (tag %in% c("eqn", "deqn")) {
    expression <- collapse_whitespace(rd_raw(if (length(x)) x[[1L]] else x))
    return(paste0(":math:`", gsub("`", "\\\\`", expression, fixed = TRUE), "`"))
  }

  if (tag %in% c("method", "S3method", "S4method")) {
    return(rst_literal(collapse_whitespace(rd_raw(x))))
  }

  inner()
}

clean_inline <- function(x) {
  collapse_whitespace(x)
}

# Inside \itemize and \enumerate, R's parser emits \item as an EMPTY marker:
# the item's content is the sibling nodes that follow it, up to the next
# \item. (\describe is different -- there \item carries {label}{body}.)
# Rendering the \item node itself therefore yields an empty bullet for every
# list in every package, which is what this used to do.
# An item's continuation lines keep whatever indent they had in the Rd source,
# and clean_block() cannot strip it because the item's first line starts at
# column 0. Left alone, the extra columns turn the continuation into a block
# quote once it is re-indented under the bullet.
indent_continuation <- function(lines) {
  if (!length(lines)) {
    return("")
  }
  nonblank <- lines[nzchar(trimws(lines))]
  indent <- if (length(nonblank)) {
    min(nchar(nonblank) - nchar(sub("^[ \t]+", "", nonblank, perl = TRUE)))
  } else {
    0L
  }
  if (is.finite(indent) && indent > 0L) lines <- substring(lines, indent + 1L)
  paste0("\n  ", paste(lines, collapse = "\n  "))
}

rd_item_groups <- function(x) {
  groups <- list()
  current <- NULL
  for (child in x) {
    if (identical(rd_tag(child), "item")) {
      if (!is.null(current)) groups[[length(groups) + 1L]] <- current
      # \item{...} inside \itemize is legal too, and puts the content inline.
      current <- if (length(child)) as.list(child) else list()
    } else if (!is.null(current)) {
      current <- c(current, list(child))
    }
  }
  if (!is.null(current)) groups[[length(groups) + 1L]] <- current
  groups
}

rd_block_nodes <- function(nodes, aliases = list()) {
  if (!length(nodes)) {
    return("")
  }
  paste(
    vapply(nodes, rd_block, character(1L), aliases = aliases, USE.NAMES = FALSE),
    collapse = ""
  )
}

# reStructuredText needs a blank line either side of a block construct. The
# surrounding text supplies its own newlines inconsistently, so pad here and
# let clean_block() collapse the runs.
rst_block <- function(x) {
  if (!nzchar(x)) {
    return("")
  }
  paste0("\n\n", x, "\n\n")
}

rd_block <- function(x, aliases = list()) {
  if (is.null(x) || !length(x)) {
    return("")
  }

  tag <- rd_tag(x)
  if (is.character(x)) {
    text <- paste(x, collapse = "")
    if (tag %in% c("RCODE", "VERB")) {
      return(text)
    }
    return(rst_text(text))
  }

  inline_tags <- c(
    "code", "command", "env", "file", "kbd", "option", "samp", "pkg",
    "emph", "strong", "link", "url", "email", "href", "eqn", "deqn",
    "method", "S3method", "S4method", "dots", "ldots"
  )
  if (tag %in% inline_tags) {
    return(rd_inline(x, aliases = aliases))
  }

  if (tag %in% c("itemize", "enumerate")) {
    ordered <- identical(tag, "enumerate")
    prefix <- if (ordered) "#. " else "- "
    rendered <- vapply(rd_item_groups(x), function(nodes) {
      body <- clean_block(rd_block_nodes(nodes, aliases = aliases))
      lines <- split_lines(body)
      if (!length(lines) || !nzchar(body)) {
        return("")
      }
      paste0(prefix, lines[[1L]], indent_continuation(lines[-1L]))
    }, character(1L), USE.NAMES = FALSE)
    rendered <- rendered[nzchar(rendered)]
    return(rst_block(paste(rendered, collapse = "\n")))
  }

  if (tag == "describe") {
    items <- Filter(function(child) identical(rd_tag(child), "item"), x)
    rendered <- vapply(items, function(item) {
      label <- if (length(item)) clean_inline(rd_inline(item[[1L]], aliases = aliases)) else ""
      body <- if (length(item) >= 2L) clean_block(rd_block(item[[2L]], aliases = aliases)) else ""
      paste0(label, "\n", indent_lines(body, 3L))
    }, character(1L), USE.NAMES = FALSE)
    return(rst_block(paste(rendered, collapse = "\n\n")))
  }

  if (tag %in% c("preformatted", "verbatim")) {
    return(rst_block(rst_code_block(rd_raw(x), language = "text")))
  }

  if (tag == "tabular") {
    return(rst_block(rst_code_block(collapse_whitespace(rd_raw(x)), language = "text")))
  }

  if (tag == "if") {
    return(if (length(x) >= 2L) rd_block(x[[2L]], aliases = aliases) else "")
  }

  if (tag == "ifelse") {
    return(if (length(x) >= 2L) rd_block(x[[2L]], aliases = aliases) else "")
  }

  if (tag %in% c("section", "subsection")) {
    title <- if (length(x)) clean_inline(rd_inline(x[[1L]], aliases = aliases)) else "Section"
    body <- if (length(x) >= 2L) clean_block(rd_block(x[[2L]], aliases = aliases)) else ""
    return(rst_block(paste0(".. rubric:: ", title, "\n\n", body)))
  }

  paste(vapply(x, rd_block, character(1L), aliases = aliases, USE.NAMES = FALSE), collapse = "")
}

rd_code <- function(x) {
  if (is.null(x) || !length(x)) {
    return("")
  }

  tag <- rd_tag(x)
  if (is.character(x)) {
    return(paste(x, collapse = ""))
  }

  if (tag %in% c("dontshow", "testonly")) {
    return("")
  }

  if (tag %in% c("dontrun", "donttest")) {
    return(paste(vapply(x, rd_code, character(1L), USE.NAMES = FALSE), collapse = ""))
  }

  if (tag %in% c("dots", "ldots")) {
    return("...")
  }

  paste(vapply(x, rd_code, character(1L), USE.NAMES = FALSE), collapse = "")
}

rst_code_block <- function(code, language = "r") {
  code <- clean_block(code)
  if (!nzchar(code)) {
    return("")
  }
  paste0(".. code-block:: ", language, "\n\n", indent_lines(code, 3L))
}

rd_argument_items <- function(section, aliases = list()) {
  if (is.null(section)) {
    return(list())
  }

  items <- Filter(function(child) identical(rd_tag(child), "item"), section)
  lapply(items, function(item) {
    name <- if (length(item)) collapse_whitespace(rd_raw(item[[1L]])) else ""
    description <- if (length(item) >= 2L) {
      clean_block(rd_block(item[[2L]], aliases = aliases))
    } else {
      ""
    }
    list(name = name, description = description)
  })
}

rd_field <- function(name, value) {
  value <- clean_block(value)
  if (!nzchar(value)) {
    return("")
  }

  lines <- strsplit(value, "\n", fixed = TRUE)[[1L]]
  first <- paste0(":", name, ": ", lines[[1L]])
  if (length(lines) == 1L) {
    return(first)
  }
  paste(c(first, paste0("   ", lines[-1L])), collapse = "\n")
}

rd_param_field <- function(name, value) {
  value <- clean_block(value)
  if (!nzchar(name) || !nzchar(value)) {
    return("")
  }

  lines <- strsplit(value, "\n", fixed = TRUE)[[1L]]
  first <- paste0(":param ", name, ": ", lines[[1L]])
  if (length(lines) == 1L) {
    return(first)
  }
  paste(c(first, paste0("   ", lines[-1L])), collapse = "\n")
}

split_usage <- function(x) {
  x <- clean_block(x)
  if (!nzchar(x)) {
    return(character())
  }

  chars <- strsplit(x, "", fixed = TRUE)[[1L]]
  depth <- 0L
  quoted <- ""
  escaped <- FALSE
  start <- 1L
  out <- character()

  for (i in seq_along(chars)) {
    ch <- chars[[i]]
    if (escaped) {
      escaped <- FALSE
      next
    }
    if (identical(ch, "\\")) {
      escaped <- TRUE
      next
    }
    if (nzchar(quoted)) {
      if (identical(ch, quoted)) quoted <- ""
      next
    }
    if (ch %in% c("'", "\"", "`")) {
      quoted <- ch
      next
    }
    if (ch %in% c("(", "{", "[")) depth <- depth + 1L
    if (ch %in% c(")", "}", "]")) depth <- max(0L, depth - 1L)
    if (identical(ch, "\n") && depth == 0L) {
      part <- if (i > start) {
        collapse_whitespace(paste(chars[seq.int(start, i - 1L)], collapse = ""))
      } else {
        ""
      }
      if (nzchar(part)) out <- c(out, part)
      start <- i + 1L
    }
  }

  if (start <= length(chars)) {
    part <- collapse_whitespace(paste(chars[seq.int(start, length(chars))], collapse = ""))
    if (nzchar(part)) out <- c(out, part)
  }

  if (!length(out)) collapse_whitespace(x) else unique(out)
}

usage_name <- function(signature, fallback) {
  signature <- collapse_whitespace(signature)
  replacement_match <- regexec(
    "^([^[:space:](]+)[[:space:]]*\\(.*\\)[[:space:]]*<-[[:space:]]*",
    signature,
    perl = TRUE
  )
  replacement_parts <- regmatches(signature, replacement_match)[[1L]]
  if (length(replacement_parts) >= 2L && nzchar(replacement_parts[[2L]])) {
    return(paste0(replacement_parts[[2L]], "<-"))
  }

  call_match <- regexec("^([^[:space:](]+)[[:space:]]*\\(", signature, perl = TRUE)
  call_parts <- regmatches(signature, call_match)[[1L]]
  if (length(call_parts) >= 2L && nzchar(call_parts[[2L]])) {
    return(call_parts[[2L]])
  }

  infix_match <- regexpr("%[^%[:space:]]+%", signature, perl = TRUE)
  if (infix_match[[1L]] >= 0L) {
    return(regmatches(signature, infix_match))
  }

  fallback
}

read_rd_topic <- function(rd, file = "") {
  raw_section <- function(tag) {
    section <- rd_section(rd, tag)
    if (is.null(section)) "" else collapse_whitespace(rd_raw(section))
  }

  usage_section <- rd_section(rd, "usage")
  usage <- if (is.null(usage_section)) character() else split_usage(rd_raw(usage_section))
  section_text <- function(x) collapse_whitespace(rd_raw(x))
  aliases <- unique(vapply(rd_sections(rd, "alias"), section_text, character(1L)))
  aliases <- aliases[nzchar(aliases)]
  fallback_name <- if (nzchar(file)) tools::file_path_sans_ext(basename(file)) else "topic"
  name <- first_nonempty(raw_section("name"), fallback_name)

  doc_type <- tolower(raw_section("docType"))
  keywords <- tolower(vapply(rd_sections(rd, "keyword"), section_text, character(1L)))
  kind <- if (identical(doc_type, "package")) {
    "package"
  } else if (identical(doc_type, "data") || "datasets" %in% keywords) {
    "data"
  } else if (identical(doc_type, "class") || "classes" %in% keywords) {
    "class"
  } else if (length(usage)) {
    "function"
  } else {
    "topic"
  }

  first_alias <- if (length(aliases)) aliases[[1L]] else name
  usage_names <- unique(vapply(usage, usage_name, character(1L), fallback = first_alias))
  usage_names <- usage_names[nzchar(usage_names)]
  canonical <- if (length(usage_names)) usage_names[[1L]] else first_alias

  list(
    file = file,
    rd = rd,
    name = name,
    canonical = canonical,
    aliases = unique(c(canonical, usage_names, aliases)),
    usage_names = usage_names,
    title = first_nonempty(raw_section("title"), canonical),
    usage = usage,
    doc_type = doc_type,
    keywords = keywords,
    kind = kind
  )
}

build_alias_map <- function(topics, package) {
  out <- list()
  for (topic in topics) {
    object_names <- topic$usage_names
    for (object_name in object_names) {
      out[[object_name]] <- object_name
      out[[paste0(package, "::", object_name)]] <- object_name
    }
    for (alias in setdiff(topic$aliases, object_names)) {
      out[[alias]] <- topic$canonical
      out[[paste0(package, "::", alias)]] <- topic$canonical
    }
  }
  out
}

should_include_topic <- function(topic, exports, include) {
  if (identical(include, "all")) {
    return(TRUE)
  }

  always <- c("package", "data", "class")
  if (topic$doc_type %in% always || topic$kind %in% always) {
    return(TRUE)
  }

  any(vapply(topic$aliases, namespace_exports_alias, logical(1L), exports = exports))
}

render_rd_section <- function(topic, tag, aliases) {
  section <- rd_section(topic$rd, tag)
  if (is.null(section)) "" else clean_block(rd_block(section, aliases = aliases))
}

render_custom_sections <- function(topic, aliases) {
  sections <- rd_sections(topic$rd, "section")
  if (!length(sections)) {
    return(character())
  }
  vapply(
    sections,
    function(section) clean_block(rd_block(section, aliases = aliases)),
    character(1L)
  )
}

render_topic_body <- function(topic, aliases) {
  body <- character()

  description <- render_rd_section(topic, "description", aliases)
  if (nzchar(description)) body <- c(body, description)

  details <- render_rd_section(topic, "details", aliases)
  if (nzchar(details)) body <- c(body, "", ".. rubric:: Details", "", details)

  arguments <- rd_argument_items(rd_section(topic$rd, "arguments"), aliases = aliases)
  if (length(arguments)) {
    fields <- vapply(arguments, function(argument) {
      rd_param_field(argument$name, argument$description)
    }, character(1L))
    fields <- fields[nzchar(fields)]
    if (length(fields)) body <- c(body, "", fields)
  }

  value <- render_rd_section(topic, "value", aliases)
  if (nzchar(value)) body <- c(body, "", rd_field("returns", value))

  format <- render_rd_section(topic, "format", aliases)
  if (nzchar(format)) body <- c(body, "", ".. rubric:: Format", "", format)

  note <- render_rd_section(topic, "note", aliases)
  if (nzchar(note)) body <- c(body, "", ".. rubric:: Note", "", note)

  author <- render_rd_section(topic, "author", aliases)
  if (nzchar(author)) body <- c(body, "", ".. rubric:: Author", "", author)

  custom <- render_custom_sections(topic, aliases)
  if (length(custom)) body <- c(body, "", custom)

  seealso <- render_rd_section(topic, "seealso", aliases)
  if (nzchar(seealso)) body <- c(body, "", ".. rubric:: See also", "", seealso)

  references <- render_rd_section(topic, "references", aliases)
  if (nzchar(references)) body <- c(body, "", ".. rubric:: References", "", references)

  examples_section <- rd_section(topic$rd, "examples")
  if (!is.null(examples_section)) {
    examples <- clean_block(rd_code(examples_section))
    if (nzchar(examples)) {
      body <- c(body, "", ".. rubric:: Examples", "", rst_code_block(examples, language = "r"))
    }
  }

  body
}

render_topic_rst <- function(topic, package, aliases) {
  directive <- if (topic$kind %in% c("function", "data", "class", "package")) {
    topic$kind
  } else {
    "topic"
  }

  body <- render_topic_body(topic, aliases)
  signatures <- if (identical(directive, "function") && length(topic$usage)) {
    topic$usage
  } else {
    topic$canonical
  }
  signature_names <- if (identical(directive, "function")) {
    vapply(signatures, usage_name, character(1L), fallback = topic$canonical)
  } else {
    topic$canonical
  }

  unattached_aliases <- setdiff(topic$aliases, signature_names)
  directives <- character()
  seen_names <- character()
  for (i in seq_along(signatures)) {
    object_name <- signature_names[[i]]
    duplicate_name <- object_name %in% seen_names
    seen_names <- c(seen_names, object_name)
    alias_values <- if (i == 1L) unattached_aliases else character()
    options <- c(
      paste0("   :canonical: ", object_name),
      paste0("   :package: ", package),
      if (length(alias_values)) {
        paste0("   :aliases: ", paste(alias_values, collapse = ", "))
      } else {
        character()
      },
      if (duplicate_name) "   :no-target:" else character()
    )
    content <- if (i == 1L && length(body)) {
      c("", indent_lines(paste(body, collapse = "\n"), 3L))
    } else {
      character()
    }
    directives <- c(
      directives,
      paste0(".. r:", directive, ":: ", signatures[[i]]),
      options,
      content,
      ""
    )
  }

  c(
    ".. Generated by rd2sphinx; do not edit.",
    "",
    paste0(".. _rd2sphinx-page-", directive, "-", slugify(topic$canonical), ":"),
    "",
    rst_heading(rst_text(topic$title), "="),
    "",
    directives
  )
}
