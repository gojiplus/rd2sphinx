"""A small semantic Sphinx domain for R package documentation.

The extension is intentionally bundled with the R package. ``render_sphinx()``
copies it into the target documentation tree so a normal Sphinx build can load
it without installing a second Python distribution.
"""

from __future__ import annotations

import hashlib
import re
from collections.abc import Iterable, Iterator
from typing import Any

from docutils import nodes
from docutils.parsers.rst import directives
from sphinx import addnodes
from sphinx.application import Sphinx
from sphinx.builders import Builder
from sphinx.directives import ObjectDescription
from sphinx.domains import Domain, ObjType
from sphinx.environment import BuildEnvironment
from sphinx.roles import XRefRole
from sphinx.util import logging
from sphinx.util.docfields import Field, GroupedField
from sphinx.util.nodes import make_refnode

LOGGER = logging.getLogger(__name__)


def _canonical_name(signature: str) -> str:
    signature = " ".join(signature.split())
    replacement = re.match(r"^([^\s(]+)\s*\(.*\)\s*<-", signature)
    if replacement:
        return f"{replacement.group(1)}<-"
    call = re.match(r"^([^\s(]+)\s*\(", signature)
    if call:
        return call.group(1)
    infix = re.search(r"%[^%\s]+%", signature)
    if infix:
        return infix.group(0)
    return signature.split()[0] if signature else "object"


def _split_aliases(value: str | None) -> list[str]:
    if not value:
        return []
    return [item.strip() for item in value.split(",") if item.strip()]


def _split_parameters(value: str) -> list[str]:
    """Split an R argument list without breaking nested calls or strings."""
    if not value.strip():
        return []
    parts: list[str] = []
    start = 0
    depth = 0
    quote = ""
    escaped = False
    for index, char in enumerate(value):
        if escaped:
            escaped = False
            continue
        if char == "\\":
            escaped = True
            continue
        if quote:
            if char == quote:
                quote = ""
            continue
        if char in {"'", '"', "`"}:
            quote = char
            continue
        if char in "([{":
            depth += 1
        elif char in ")]}" and depth:
            depth -= 1
        elif char == "," and depth == 0:
            part = value[start:index].strip()
            if part:
                parts.append(part)
            start = index + 1
    final = value[start:].strip()
    if final:
        parts.append(final)
    return parts


class RObject(ObjectDescription):
    """Describe a function, data object, or generic R help topic."""

    option_spec = dict(ObjectDescription.option_spec)
    option_spec.update(
        {
            "aliases": directives.unchanged,
            "canonical": directives.unchanged,
            "package": directives.unchanged,
            "no-target": directives.flag,
        }
    )

    doc_field_types = [
        GroupedField(
            "parameter",
            label="Arguments",
            names=("param", "parameter", "arg"),
            can_collapse=True,
        ),
        Field(
            "returnvalue",
            label="Returns",
            names=("returns", "return", "value"),
            has_arg=False,
        ),
    ]

    def handle_signature(self, sig: str, signode: addnodes.desc_signature) -> str:
        normalized = " ".join(sig.split())
        name = self.options.get("canonical", "").strip() or _canonical_name(normalized)
        signode["fullname"] = name
        signode["package"] = self.options.get("package", "")

        replacement = re.match(
            r"^([^\s(]+)\s*\((.*)\)\s*<-\s*(.+)$", normalized
        )
        match = re.match(r"^([^\s(]+)\s*\((.*)\)$", normalized)
        if replacement:
            signode += addnodes.desc_name(replacement.group(1), replacement.group(1))
            parameter_list = addnodes.desc_parameterlist()
            for parameter in _split_parameters(replacement.group(2)):
                parameter_list += addnodes.desc_parameter(parameter, parameter)
            signode += parameter_list
            annotation = f" <- {replacement.group(3)}"
            signode += addnodes.desc_annotation(annotation, annotation)
        elif match:
            signode += addnodes.desc_name(name, name)
            parameter_list = addnodes.desc_parameterlist()
            for parameter in _split_parameters(match.group(2)):
                parameter_list += addnodes.desc_parameter(parameter, parameter)
            signode += parameter_list
        else:
            signode += addnodes.desc_name(normalized, normalized)
        return name

    def add_target_and_index(
        self,
        name: str,
        sig: str,
        signode: addnodes.desc_signature,
    ) -> None:
        if "no-target" in self.options:
            return

        domain = self.env.get_domain("r")
        assert isinstance(domain, RDomain)

        readable = nodes.make_id(name) or "object"
        digest = hashlib.sha1(name.encode("utf-8")).hexdigest()[:8]
        anchor = f"r-{self.objtype}-{readable}-{digest}"
        if anchor not in signode["ids"]:
            signode["ids"].append(anchor)
            self.state.document.note_explicit_target(signode)

        package = self.options.get("package", "").strip()
        aliases = _split_aliases(self.options.get("aliases"))
        domain.note_object(
            name=name,
            objtype=self.objtype,
            docname=self.env.docname,
            anchor=anchor,
            package=package,
            aliases=aliases,
        )

        if name not in self.names:
            self.names.append(name)

        if "no-index" not in self.options and "noindex" not in self.options:
            index_text = f"{name} (R {self.objtype})"
            self.indexnode["entries"].append(("single", index_text, anchor, "", None))


class RDomain(Domain):
    """Sphinx domain containing R objects extracted from Rd topics."""

    name = "r"
    label = "R"

    object_types = {
        "function": ObjType("function", "func", "obj"),
        "data": ObjType("data", "data", "obj"),
        "class": ObjType("class", "class", "obj"),
        "package": ObjType("package", "pkg", "obj"),
        "topic": ObjType("topic", "topic", "obj"),
    }

    directives = {
        "function": RObject,
        "data": RObject,
        "class": RObject,
        "package": RObject,
        "topic": RObject,
    }

    roles = {
        "func": XRefRole(fix_parens=True),
        "data": XRefRole(),
        "class": XRefRole(),
        "pkg": XRefRole(),
        "topic": XRefRole(),
        "obj": XRefRole(),
    }

    initial_data: dict[str, dict[str, tuple[str, str, str, str, bool]]] = {
        "objects": {}
    }

    @property
    def objects(self) -> dict[str, tuple[str, str, str, str, bool]]:
        return self.data.setdefault("objects", {})

    def note_object(
        self,
        *,
        name: str,
        objtype: str,
        docname: str,
        anchor: str,
        package: str,
        aliases: Iterable[str],
    ) -> None:
        names = [name, *aliases]
        if package:
            names.extend(f"{package}::{item}" for item in [name, *aliases])

        for candidate in dict.fromkeys(names):
            existing = self.objects.get(candidate)
            if existing and existing[0] != docname:
                LOGGER.warning(
                    "duplicate R object description of %s; other instance in %s",
                    candidate,
                    existing[0],
                    location=docname,
                )
            self.objects[candidate] = (
                docname,
                anchor,
                objtype,
                name,
                candidate == name,
            )

    def clear_doc(self, docname: str) -> None:
        for name, entry in list(self.objects.items()):
            if entry[0] == docname:
                del self.objects[name]

    def merge_domaindata(self, docnames: list[str], otherdata: dict[str, Any]) -> None:
        other_objects = otherdata.get("objects", {})
        for name, entry in other_objects.items():
            if entry[0] in docnames:
                self.objects[name] = entry

    def _find_object(
        self, target: str, role: str | None = None
    ) -> tuple[str, tuple[str, str, str, str, bool]] | None:
        candidates = [target.strip()]
        if target.endswith("()"):
            candidates.append(target[:-2])
        if "::" in target:
            candidates.append(target.rsplit("::", 1)[-1])

        allowed: set[str] | None = None
        if role and role != "obj":
            allowed = set(self.objtypes_for_role(role))

        for candidate in dict.fromkeys(candidates):
            entry = self.objects.get(candidate)
            if entry and (allowed is None or entry[2] in allowed):
                return candidate, entry
        return None

    def resolve_xref(
        self,
        env: BuildEnvironment,
        fromdocname: str,
        builder: Builder,
        typ: str,
        target: str,
        node: addnodes.pending_xref,
        contnode: nodes.Element,
    ) -> nodes.reference | None:
        found = self._find_object(target, role=typ)
        if found is None:
            return None
        _, (docname, anchor, _objtype, canonical, _primary) = found
        return make_refnode(
            builder,
            fromdocname,
            docname,
            anchor,
            contnode,
            canonical,
        )

    def resolve_any_xref(
        self,
        env: BuildEnvironment,
        fromdocname: str,
        builder: Builder,
        target: str,
        node: addnodes.pending_xref,
        contnode: nodes.Element,
    ) -> list[tuple[str, nodes.reference]]:
        found = self._find_object(target)
        if found is None:
            return []
        _, (docname, anchor, objtype, canonical, _primary) = found
        role = {
            "function": "func",
            "data": "data",
            "class": "class",
            "package": "pkg",
            "topic": "topic",
        }[objtype]
        refnode = make_refnode(
            builder,
            fromdocname,
            docname,
            anchor,
            contnode,
            canonical,
        )
        return [(f"r:{role}", refnode)]

    def get_objects(self) -> Iterator[tuple[str, str, str, str, str, int]]:
        for name, (docname, anchor, objtype, canonical, primary) in self.objects.items():
            yield (
                name,
                canonical,
                objtype,
                docname,
                anchor,
                1 if primary else -1,
            )


def setup(app: Sphinx) -> dict[str, Any]:
    app.add_domain(RDomain)
    return {
        "version": "0.1.0",
        "parallel_read_safe": True,
        "parallel_write_safe": True,
    }
