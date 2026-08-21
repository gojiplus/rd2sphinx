# rd2sphinx

<!-- badges: start -->
[![R-CMD-check](https://github.com/gojiplus/rd2sphinx/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/gojiplus/rd2sphinx/actions/workflows/R-CMD-check.yml)
[![lint](https://github.com/gojiplus/rd2sphinx/actions/workflows/lint.yml/badge.svg)](https://github.com/gojiplus/rd2sphinx/actions/workflows/lint.yml)
<!-- badges: end -->

`rd2sphinx` turns an R package's help topics into Sphinx objects, so an R
package can be documented by the same toolchain as a Python one. It reads
`man/*.Rd` through R's own Rd parser and writes directives such as
`.. r:function::`. Sphinx then supplies the theme, the search index, the
builders, and the deployment.

```text
roxygen comments -> man/*.Rd -> rd2sphinx -> Sphinx R domain -> Furo HTML
```

The output is not Rd converted to Markdown. Each documented object is
registered in a bundled Sphinx domain, which puts it in the general index, in
Sphinx search, and in `objects.inv`, the inventory other Sphinx projects read
to link into yours. A `\link{}` in the Rd source becomes a real cross-reference
that fails the build when it dangles.

## Install

```r
remotes::install_github("gojiplus/rd2sphinx")
```

Sphinx and Furo are Python packages, so you need Python 3.10 or later
alongside R.

## Use

From an R package root, regenerate the Rd files if the roxygen comments have
changed, then scaffold a Sphinx source tree:

```r
roxygen2::roxygenise()
rd2sphinx::use_sphinx(docs_dir = "sphinx-docs")
```

That writes `conf.py`, `index.rst`, a `Makefile`, `requirements.txt`, and a
stylesheet. Install the Python side and build:

```sh
python -m pip install -r sphinx-docs/requirements.txt
```

```r
rd2sphinx::build_sphinx(docs_dir = "sphinx-docs", output_dir = "_site")
```

`build_sphinx()` renders the reference pages and then runs `sphinx-build` with
warnings treated as errors. When Sphinx is run by something else, such as a CI
job or a Makefile, render on its own:

```r
rd2sphinx::render_sphinx(docs_dir = "sphinx-docs", include = "exports")
```

```sh
sphinx-build -W --keep-going -b html sphinx-docs _site
```

`include = "exports"` documents what `NAMESPACE` exports, plus package, data,
and class topics. `include = "all"` documents every Rd topic, internals
included.

Generated files are build inputs, not source. `use_sphinx()` adds the
reference pages, the copied extension, and `_site/` to `.gitignore`.

## Continuous integration

The fleet's reusable workflow builds the site and uploads it as an artifact:

```yaml
name: sphinx-docs

on:
  push:
    branches: [main, master]
  pull_request:
  workflow_dispatch:

jobs:
  sphinx:
    uses: gojiplus/r-canon/.github/workflows/reusable-sphinx.yml@v2
    with:
      docs-dir: sphinx-docs
```

Add `deploy: true` to publish it to GitHub Pages from the default branch. A
repository already publishing a pkgdown site can leave that off and compare the
two before switching.

## What it handles

Rd topics for functions, data, classes, packages, and general help; multiple
usage sections in one topic; default and nested arguments; infix operators such
as `%>%`; replacement functions such as `names(x) <- value`; and S3 and S4
method signatures.

Within a topic it renders arguments, return values, details, format, notes,
references, see-also sections, examples, itemized and numbered and definition
lists, code, emphasis, links, and inline math. Export filtering understands
`export()`, `exportPattern()`, `exportMethods()`, `exportClasses()`, and
`S3method()`.

Objects are registered under their local name and their package-qualified name,
so ``:r:func:`tuber::yt_search` `` resolves from anywhere in the project.

## What it does not handle yet

Vignettes, README, and NEWS are not ingested; a Sphinx site can include them,
but you write the `.rst` yourself. `\tabular` becomes a preformatted block
rather than a Sphinx table. R6 and S4 classes render as ordinary topics without
per-method presentation. Inventories from other packages are not discovered
automatically, so `intersphinx_mapping` in `conf.py` is a manual edit.

## License

MIT.
