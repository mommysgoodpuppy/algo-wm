"""Generate a Millet-only view of the MLWorks basis.

The output is intentionally a type-checking shim, not an implementation.
It shadows Millet's built-in Standard Basis structures with structures whose
members are taken from the MLWorks signature files.
"""

from __future__ import annotations

import re
from pathlib import Path


MLWORKS = Path(r"C:\GIT\mlworks")
BASIS = MLWORKS / "src" / "basis"
OUT = Path(__file__).resolve().parent / "mlworks-generated.sml"

SIGNATURE_FILES = [
    "array.sml",
    "array2.sml",
    "bool.sml",
    "byte.sml",
    "char.sml",
    "command_line.sml",
    "date.sml",
    "general.sml",
    "ieee_real.sml",
    "integer.sml",
    "io.sml",
    "list.sml",
    "list_pair.sml",
    "math.sml",
    "option.sml",
    "real.sml",
    "string.sml",
    "string_cvt.sml",
    "substring.sml",
    "time.sml",
    "timer.sml",
    "vector.sml",
    "word.sml",
]

STRUCTURE_BY_SIGNATURE = {
    "ARRAY": "Array",
    "ARRAY2": "Array2",
    "BOOL": "Bool",
    "BYTE": "Byte",
    "CHAR": "Char",
    "COMMAND_LINE": "CommandLine",
    "DATE": "Date",
    "GENERAL": "General",
    "IEEE_REAL": "IEEEReal",
    "INTEGER": "Int",
    "IO": "IO",
    "LIST": "List",
    "LIST_PAIR": "ListPair",
    "MATH": "Math",
    "OPTION": "Option",
    "REAL": "Real",
    "STRING": "String",
    "STRING_CVT": "StringCvt",
    "SUBSTRING": "Substring",
    "TIME": "Time",
    "TIMER": "Timer",
    "VECTOR": "Vector",
    "WORD": "Word",
}

TYPE_FALLBACKS = {
    "char": "char",
    "int": "int",
    "real": "real",
    "string": "string",
    "unit": "unit",
    "word": "word",
    "exn": "exn",
}

INFIX_IDENTIFIERS = {"div", "mod", "o", "before"}

TYPE_REWRITES = {
    "PreInt.int": "int",
    "PreLargeInt.int": "LargeInt.int",
    "PreWord.word": "word",
    "PreLargeWord.word": "LargeWord.word",
    "PreLargeReal.real": "LargeReal.real",
}


def strip_comments(src: str) -> str:
    out: list[str] = []
    i = 0
    depth = 0
    while i < len(src):
        if src.startswith("(*", i):
            depth += 1
            i += 2
        elif depth and src.startswith("*)", i):
            depth -= 1
            i += 2
        else:
            if not depth:
                out.append(src[i])
            i += 1
    return "".join(out)


def one_line(s: str) -> str:
    return re.sub(r"\s+", " ", s.strip())


def signature_body(src: str) -> tuple[str, str] | None:
    src = strip_comments(src)
    match = re.search(r"\bsignature\s+([A-Z0-9_']+)\s*=\s*sig\b", src)
    if not match:
        return None
    name = match.group(1)
    start = match.end()
    depth = 1
    i = start
    token = re.compile(r"\b(sig|struct|end)\b")
    while True:
        m = token.search(src, i)
        if not m:
            raise ValueError(f"could not find end for signature {name}")
        if m.group(1) in {"sig", "struct"}:
            depth += 1
        elif m.group(1) == "end":
            depth -= 1
            if depth == 0:
                return name, src[start:m.start()]
        i = m.end()


def split_specs(body: str) -> list[str]:
    specs: list[str] = []
    cur: list[str] = []
    starters = (
        "val ",
        "type ",
        "eqtype ",
        "datatype ",
        "exception ",
        "structure ",
        "include ",
        "sharing ",
    )
    for raw in body.splitlines():
        line = raw.strip()
        if not line:
            continue
        if any(line.startswith(x) for x in starters) and cur:
            specs.append(one_line(" ".join(cur)))
            cur = [line]
        else:
            cur.append(line)
    if cur:
        specs.append(one_line(" ".join(cur)))
    return specs


def arity_prefix(type_name: str) -> str:
    vars_ = re.findall(r"'_?[A-Za-z0-9]+", type_name)
    if not vars_:
        return ""
    if len(vars_) == 1:
        return vars_[0] + " "
    return "(" + ", ".join(vars_) + ") "


def type_name(spec: str) -> str | None:
    match = re.match(r"(?:eqtype|type)\s+(.+)$", spec)
    if not match:
        return None
    lhs = match.group(1).split("=")[0].strip()
    parts = lhs.split()
    return parts[-1] if parts else None


def normalize_type_text(ty: str) -> str:
    for old, new in TYPE_REWRITES.items():
        ty = ty.replace(old, new)
    return ty.replace("'_", "'")


def type_rhs(spec: str) -> str | None:
    if "=" not in spec:
        return None
    return normalize_type_text(spec.split("=", 1)[1].strip())


def constructor_name(struct: str, name: str) -> str:
    safe = re.sub(r"[^A-Za-z0-9_]", "_", name)
    return f"MLWorks_{struct}_{safe}"


def val_name(spec: str) -> str | None:
    match = re.match(r"val\s+(.+?)\s*:", spec)
    return match.group(1).strip() if match else None


def val_type(spec: str) -> str | None:
    match = re.match(r"val\s+.+?\s*:\s*(.+)$", spec)
    if not match:
        return None
    ty = match.group(1).strip()
    return normalize_type_text(ty)


def val_binding_name(name: str) -> str:
    if name in INFIX_IDENTIFIERS:
        return "op " + name
    if re.match(r"^[A-Za-z_][A-Za-z0-9_']*$", name):
        return name
    return "op " + name


def emit_structure(sig: str, struct: str, specs: list[str]) -> list[str]:
    lines = [f"structure {struct} =", "  struct"]
    for spec in specs:
        if spec.startswith(("sharing ", "include ", "structure ")):
            continue
        if spec.startswith("datatype "):
            if " = datatype " in spec:
                continue
            lines.append("    " + spec)
            continue
        if spec.startswith("exception "):
            lines.append("    " + spec)
            continue
        name = type_name(spec)
        if name is not None:
            prefix = arity_prefix(spec.split("=")[0])
            rhs = type_rhs(spec)
            if rhs is not None:
                lines.append(f"    type {prefix}{name} = {rhs}")
            else:
                fallback = TYPE_FALLBACKS.get(name)
                if fallback is not None and not prefix:
                    lines.append(f"    type {name} = {fallback}")
                else:
                    lines.append(f"    datatype {prefix}{name} = {constructor_name(struct, name)}")
            continue
        name = val_name(spec)
        ty = val_type(spec)
        if name and ty:
            rhs = f"raise Fail \"MLWorks Millet shim: {struct}.{name}\""
            if "->" in ty:
                rhs = f"(fn _ => {rhs})"
            lines.append(f"    val {val_binding_name(name)} : {ty} = {rhs}")
    lines.append("  end")
    return lines


def main() -> None:
    signatures: list[tuple[str, list[str]]] = []
    for fname in SIGNATURE_FILES:
        got = signature_body((BASIS / fname).read_text())
        if got is None:
            continue
        sig, body = got
        signatures.append((sig, split_specs(body)))

    lines = [
        "(* Generated by .millet/gen.py. Do not edit by hand. *)",
        "(* This file is for Millet only; it is not part of the MLWorks build. *)",
        "",
        "val require : string -> unit = fn _ => ()",
        "",
    ]
    for sig, specs in signatures:
        struct = STRUCTURE_BY_SIGNATURE.get(sig)
        if not struct:
            continue
        lines.extend(emit_structure(sig, struct, specs))
        lines.append("")

    lines.extend(
        [
            "structure MLWorks =",
            "  struct",
            "    structure Internal =",
            "      struct",
            "        structure Bits =",
            "          struct",
            "            val andb : int * int -> int = fn _ => raise Fail \"MLWorks Millet shim: MLWorks.Internal.Bits.andb\"",
            "          end",
            "",
            "        structure Array =",
            "          struct",
            "            type 'a array = 'a Array.array",
            "            val array : int * 'a -> 'a array = fn _ => raise Fail \"MLWorks Millet shim: MLWorks.Internal.Array.array\"",
            "            val arrayoflist : 'a list -> 'a array = fn _ => raise Fail \"MLWorks Millet shim: MLWorks.Internal.Array.arrayoflist\"",
            "            val length : 'a array -> int = fn _ => raise Fail \"MLWorks Millet shim: MLWorks.Internal.Array.length\"",
            "            val sub : 'a array * int -> 'a = fn _ => raise Fail \"MLWorks Millet shim: MLWorks.Internal.Array.sub\"",
            "            val update : 'a array * int * 'a -> unit = fn _ => raise Fail \"MLWorks Millet shim: MLWorks.Internal.Array.update\"",
            "          end",
            "",
            "        structure ExtendedArray =",
            "          struct",
            "            type 'a array = 'a Array.array",
            "            val array : int * 'a -> 'a array = fn _ => raise Fail \"MLWorks Millet shim: MLWorks.Internal.ExtendedArray.array\"",
            "            val length : 'a array -> int = fn _ => raise Fail \"MLWorks Millet shim: MLWorks.Internal.ExtendedArray.length\"",
            "            val sub : 'a array * int -> 'a = fn _ => raise Fail \"MLWorks Millet shim: MLWorks.Internal.ExtendedArray.sub\"",
            "            val update : 'a array * int * 'a -> unit = fn _ => raise Fail \"MLWorks Millet shim: MLWorks.Internal.ExtendedArray.update\"",
            "            val reducel : ('b * 'a -> 'b) -> 'b * 'a array -> 'b = fn _ => raise Fail \"MLWorks Millet shim: MLWorks.Internal.ExtendedArray.reducel\"",
            "            val iterate : ('a -> unit) -> 'a array -> unit = fn _ => raise Fail \"MLWorks Millet shim: MLWorks.Internal.ExtendedArray.iterate\"",
            "            val iterate_index : (int * 'a -> unit) -> 'a array -> unit = fn _ => raise Fail \"MLWorks Millet shim: MLWorks.Internal.ExtendedArray.iterate_index\"",
            "            val map_index : (int * 'a -> 'b) -> 'a array -> 'b array = fn _ => raise Fail \"MLWorks Millet shim: MLWorks.Internal.ExtendedArray.map_index\"",
            "          end",
            "",
            "        structure Value =",
            "          struct",
            "            val unsafe_array_sub : 'a Array.array * int -> 'a = fn _ => raise Fail \"MLWorks Millet shim: MLWorks.Internal.Value.unsafe_array_sub\"",
            "            val unsafe_array_update : 'a Array.array * int * 'a -> unit = fn _ => raise Fail \"MLWorks Millet shim: MLWorks.Internal.Value.unsafe_array_update\"",
            "          end",
            "      end",
            "  end",
            "",
            "(* MLWorks keeps these old Basis aliases available at top level. *)",
            "val chr : int -> char = Char.chr",
            "val ord : char -> int = Char.ord",
            "val explode : string -> char list = String.explode",
            "val implode : char list -> string = String.implode",
            "val concat : string list -> string = String.concat",
            "val size : string -> int = String.size",
            "val substring : string * int * int -> string = String.substring",
            "",
        ]
    )
    OUT.write_text("\n".join(lines), newline="\n")


if __name__ == "__main__":
    main()
