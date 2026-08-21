#' Add a Sphinx documentation site to an R package
#'
#' Scaffold a conventional Sphinx source directory configured for the Furo
#' theme and the R domain bundled with `rd2sphinx`.
#'
#' @param path Path to an R package source directory.
#' @param docs_dir Sphinx source directory, relative to `path` by default.
#' @param overwrite Replace existing scaffold files.
#' @param github_actions Create a standalone GitHub Pages workflow.
#' @param source_branch Source branch shown by Furo's source links. When `NULL`,
#'   detect the branch while Sphinx builds and fall back to `"main"`.
#' @return Invisibly, the paths that were created or updated.
#' @export
use_sphinx <- function(
    path = ".",
    docs_dir = "docs",
    overwrite = FALSE,
    github_actions = TRUE,
    source_branch = NULL) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  description <- read_package_description(path)
  package <- description$Package
  docs_path <- rd2sphinx_path(docs_dir, path)

  ensure_dir(docs_path)
  ensure_dir(file.path(docs_path, "_static"))
  ensure_dir(file.path(docs_path, "_ext"))

  files <- c(
    conf = file.path(docs_path, "conf.py"),
    index = file.path(docs_path, "index.rst"),
    requirements = file.path(docs_path, "requirements.txt"),
    css = file.path(docs_path, "_static", "custom.css"),
    makefile = file.path(docs_path, "Makefile")
  )

  write_if_allowed(
    sphinx_conf_template(
      docs_dir = docs_dir,
      source_branch = source_branch
    ),
    files[["conf"]],
    overwrite = overwrite
  )
  write_if_allowed(
    sphinx_index_template(package, description$Description),
    files[["index"]],
    overwrite = overwrite
  )
  write_if_allowed(
    c("sphinx>=7.2,<10", "furo>=2024.8.6"),
    files[["requirements"]],
    overwrite = overwrite
  )
  write_if_allowed(
    sphinx_css_template(),
    files[["css"]],
    overwrite = overwrite
  )
  write_if_allowed(
    sphinx_makefile_template(),
    files[["makefile"]],
    overwrite = overwrite
  )
  install_sphinx_extension(docs_path)

  append_unique_lines(
    file.path(path, ".gitignore"),
    c(
      "_site/",
      paste0(docs_dir, "/reference/"),
      paste0(docs_dir, "/_ext/rd2sphinx_domain.py")
    )
  )

  if (isTRUE(github_actions)) {
    workflow <- file.path(path, ".github", "workflows", "sphinx-docs.yml")
    write_if_allowed(
      sphinx_workflow_template(docs_dir),
      workflow,
      overwrite = overwrite
    )
    files <- c(files, workflow = workflow)
  }

  message("Sphinx documentation scaffolded in ", docs_path)
  invisible(unname(files))
}

sphinx_conf_template <- function(docs_dir = "docs", source_branch = NULL) {
  source_branch_literal <- if (
    is.null(source_branch) ||
      !length(source_branch) ||
      is.na(source_branch[[1L]]) ||
      !nzchar(source_branch[[1L]])
  ) {
    "None"
  } else {
    python_repr(source_branch)
  }
  docs_dir <- gsub("\\\\", "/", docs_dir)

  c(
    "from pathlib import Path",
    "import os",
    "import sys",
    "",
    "DOCS = Path(__file__).resolve().parent",
    "",
    "def find_package_root(start: Path) -> Path:",
    "    for candidate in (start, *start.parents):",
    "        if (candidate / 'DESCRIPTION').exists():",
    "            return candidate",
    "    raise RuntimeError(f'No R package DESCRIPTION found above {start}')",
    "",
    "ROOT = find_package_root(DOCS)",
    "sys.path.insert(0, str(DOCS / '_ext'))",
    "",
    "def read_description(path: Path) -> dict[str, str]:",
    "    fields: dict[str, str] = {}",
    "    current = None",
    "    for raw in path.read_text(encoding='utf-8').splitlines():",
    "        if raw[:1].isspace() and current:",
    "            fields[current] += ' ' + raw.strip()",
    "        elif ':' in raw:",
    "            current, value = raw.split(':', 1)",
    "            fields[current] = value.strip()",
    "    return fields",
    "",
    "def detect_source_branch(root: Path) -> str:",
    "    head = root / '.git' / 'HEAD'",
    "    if head.exists():",
    "        value = head.read_text(encoding='utf-8').strip()",
    "        prefix = 'ref: refs/heads/'",
    "        if value.startswith(prefix):",
    "            return value[len(prefix):]",
    "    github_ref = os.environ.get('GITHUB_HEAD_REF') or os.environ.get('GITHUB_REF_NAME')",
    "    if github_ref and github_ref not in {'merge', 'HEAD'}:",
    "        return github_ref",
    "    return 'main'",
    "",
    "description = read_description(ROOT / 'DESCRIPTION')",
    "project = description.get('Package', ROOT.name)",
    "author = description.get('Authors@R', '')",
    "release = description.get('Version', '')",
    "urls = [item.strip() for item in description.get('URL', '').split(',') if item.strip()]",
    "source_repository = next((item for item in urls if 'github.com/' in item), '')",
    "if source_repository and not source_repository.endswith('/'):",
    "    source_repository += '/'",
    paste0("source_branch = ", source_branch_literal, " or detect_source_branch(ROOT)"),
    "",
    "extensions = ['rd2sphinx_domain']",
    "source_suffix = {'.rst': 'restructuredtext'}",
    "root_doc = 'index'",
    "exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']",
    "nitpicky = True",
    "",
    "html_theme = 'furo'",
    "html_static_path = ['_static']",
    "html_css_files = ['custom.css']",
    "html_title = f'{project} {release}' if release else project",
    "html_theme_options = {}",
    "if source_repository:",
    "    html_theme_options.update({",
    "        'source_repository': source_repository,",
    "        'source_branch': source_branch,",
    paste0(
      "        'source_directory': ",
      python_repr(paste0(docs_dir, "/")),
      ","
    ),
    "    })",
    ""
  )
}

sphinx_index_template <- function(package, description) {
  description <- rst_text(clean_block(description %||% ""))
  c(
    rst_heading(package, "="),
    "",
    description,
    "",
    ".. toctree::",
    "   :maxdepth: 2",
    "   :caption: Contents",
    "",
    "   reference/index",
    "",
    "Indices",
    "-------",
    "",
    "* :ref:`genindex`",
    "* :ref:`search`",
    ""
  )
}

sphinx_css_template <- function() {
  c(
    ":root {",
    "  --content-width: 52rem;",
    "}",
    "",
    ".sig-name, .sig-prename {",
    "  font-variant-ligatures: none;",
    "}",
    ""
  )
}

sphinx_makefile_template <- function() {
  c(
    "SPHINXOPTS ?= -W --keep-going",
    "SPHINXBUILD ?= sphinx-build",
    "SOURCEDIR = .",
    "BUILDDIR = ../_site",
    "",
    ".PHONY: html clean",
    "html:",
    "\t$(SPHINXBUILD) $(SPHINXOPTS) -b html $(SOURCEDIR) $(BUILDDIR)",
    "",
    "clean:",
    "\trm -rf $(BUILDDIR)",
    ""
  )
}

sphinx_workflow_template <- function(docs_dir) {
  c(
    "name: Sphinx docs",
    "",
    "on:",
    "  push:",
    "    branches: [main, master]",
    "  pull_request:",
    "  workflow_dispatch:",
    "    inputs:",
    "      deploy:",
    "        description: Deploy this Furo build to GitHub Pages",
    "        type: boolean",
    "        default: false",
    "",
    "permissions:",
    "  contents: read",
    "",
    "jobs:",
    "  build:",
    "    runs-on: ubuntu-latest",
    "    steps:",
    "      - uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1 # v7.0.1",
    "      - uses: r-lib/actions/setup-r@d3c5be51b12e724e68f33216ca3c148b66d5f0b6 # v2",
    "        with:",
    "          use-public-rspm: true",
    "      - uses: actions/setup-python@5fda3b95a4ea91299a34e894583c3862153e4b97 # v7.0.0",
    "        with:",
    "          python-version: '3.12'",
    "      - name: Install rd2sphinx",
    "        run: |",
    "          install.packages('remotes')",
    "          remotes::install_github('gojiplus/rd2sphinx')",
    "        shell: Rscript {0}",
    "      - name: Install Sphinx",
    paste0("        run: python -m pip install -r ", shQuote(paste0(docs_dir, "/requirements.txt"))),
    "      - name: Render R reference",
    paste0("        run: rd2sphinx::render_sphinx(docs_dir = '", docs_dir, "')"),
    "        shell: Rscript {0}",
    "      - name: Build HTML",
    paste0("        run: sphinx-build -W --keep-going -b html ", shQuote(docs_dir), " _site"),
    "      - uses: actions/upload-artifact@043fb46d1a93c77aae656e7c1c64a875d1fc6a0a # v7.0.1",
    "        with:",
    "          name: sphinx-html",
    "          path: _site",
    "      - uses: actions/upload-pages-artifact@fc324d3547104276b827a68afc52ff2a11cc49c9 # v5.0.0",
    "        if: github.event_name != 'pull_request'",
    "        with:",
    "          path: _site",
    "",
    "  deploy:",
    "    if: github.event_name == 'workflow_dispatch' && inputs.deploy == true",
    "    needs: build",
    "    runs-on: ubuntu-latest",
    "    permissions:",
    "      pages: write",
    "      id-token: write",
    "    environment:",
    "      name: github-pages",
    "      url: ${{ steps.deployment.outputs.page_url }}",
    "    steps:",
    "      - id: deployment",
    "        uses: actions/deploy-pages@cd2ce8fcbc39b97be8ca5fce6e763baed58fa128 # v5.0.0",
    ""
  )
}
