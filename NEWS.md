# rd2sphinx 0.1.0

First release.

* `use_sphinx()` scaffolds a Furo-themed Sphinx source tree for an R package.
* `render_sphinx()` reads the package's Rd database through `tools::Rd_db()`
  and writes semantic reStructuredText: `r:function`, `r:data`, `r:class`,
  `r:package`, and `r:topic` directives.
* `build_sphinx()` renders and then runs `sphinx-build` with warnings treated
  as errors.
* A bundled Sphinx domain registers each object under its local and
  package-qualified name, so objects reach the general index, Sphinx search,
  and `objects.inv`, and `\link{}` becomes a real cross-reference.
* Export filtering reads `export()`, `exportPattern()`, `exportMethods()`,
  `exportClasses()`, and `S3method()`.
* Infix operators, replacement functions, multiple usage sections, and S3 and
  S4 method signatures keep usable object names.
