#' rd2sphinx: Build Sphinx Documentation for R Packages
#'
#' `rd2sphinx` converts an R package's generated Rd help topics into semantic
#' Sphinx directives. The bundled R domain supplies object registration,
#' cross-references, search entries, and `objects.inv` inventory records, while
#' ordinary Sphinx handles themes and builders.
#'
#' Start with [use_sphinx()], generate references with [render_sphinx()], and
#' invoke Sphinx with [build_sphinx()].
#'
#' @keywords internal
#' @importFrom utils tail
"_PACKAGE"
