"""Compare committed source-derived specimens with Clang's integrated assembler.

This optional audit never imports or executes the OCaml encoder. Clang's choice
of bytes is recorded separately from the fixed original-source expectations.
"""
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor
import argparse
import hashlib
import importlib.util
import json
import struct
import subprocess
import sys

sys.dont_write_bytecode = True
ROOT = Path(__file__).resolve().parents[2]
TARGET = "x86_64-pc-windows-msvc"
WIDTHS = {1: "byte", 2: "word", 4: "dword", 8: "qword"}
spec = importlib.util.spec_from_file_location(
    "forms", ROOT / "test/fixtures/assembly-encoder-forms.py"
)
forms = importlib.util.module_from_spec(spec)
spec.loader.exec_module(forms)


def textual(kind, operand):
    parts = operand.split(":")
    if parts[0] == "register":
        reg = parts[1].lower()
        return "st(" + reg[2:] + ")" if reg.startswith("st") else reg
    if parts[0] == "memory":
        return WIDTHS[int(parts[1])] + " ptr [" + parts[2].lower() + "]"
    if parts[0] == "address":
        return WIDTHS[int(parts[1])] + " ptr [" + parts[2] + "]"
    return ".Ltarget" if kind.startswith("REL") else parts[1]


def instruction(row, mode, operands):
    name = row["name"].lower()
    args = [textual(kind, value) for kind, value in zip(row["args"], operands)]
    notes = []
    if name == "imul2":
        name = "imul"
        if len(args) == 2 and row["args"][0].startswith("RM"):
            width = int(row["args"][0][2:])
            args.insert(0, {16: "bx", 32: "ebx", 64: "rbx"}[width])
            notes.append("original IMUL2 repeats memory base in the result register field")
    if name in {"shl1", "shr1", "sar1", "rol1", "ror1", "rcl1", "rcr1"}:
        name = name[:-1]
        args.append("1")
    if name in {"shld", "shrd"} and len(args) == 2:
        args.append("cl")
    if name == "ret1":
        name = "ret"
    if name == "retf1":
        name = "retf"
    if name == "enter":
        args.append("0")
    if name == "nop2":
        name, args = "xchg", ["ax", "ax"]
    if name.startswith("rep"):
        name = name.replace("_", " ")
    if name.startswith(("mov_cr", "mov_eax_cr", "mov_rax_cr")):
        fields = name.split("_")
        name, args = "mov", fields[1:]
    if name.startswith("f") and any(value.startswith("memory:") for value in operands):
        args = [arg for arg in args if arg != "st(0)"]
    if name == "fstsw" and not args:
        args = ["ax"]
    for index, (kind, operand) in enumerate(zip(row["args"], operands)):
        if name in {"int", "in", "out"} and kind == "IMM8":
            args[index] = str(int(operand.split(":")[1]) & 255)
            notes.append("explicit byte bit pattern for signed source immediate")
        if mode == 64 and kind == "UIMM32" and name != "mov":
            shift = int(row["name"] == "IMUL2" and row["args"][0].startswith("RM"))
            args[index + shift] = str(int(operand.split(":")[1]) - (1 << 32))
            notes.append("signed spelling of the identical qword imm32 bits")
    return name + (" " + ", ".join(args) if args else ""), notes


def code_bytes(path):
    data = path.read_bytes()
    count = struct.unpack_from("<H", data, 2)[0]
    optional = struct.unpack_from("<H", data, 16)[0]
    for index in range(count):
        offset = 20 + optional + 40 * index
        if data[offset:offset + 8].split(b"\0", 1)[0] == b".text":
            size, pointer = struct.unpack_from("<II", data, offset + 16)
            return data[pointer:pointer + size]
    raise ValueError("COFF object has no .text section")


def fixture_rows(path):
    result = {}
    for line in path.read_text(encoding="utf-8").splitlines():
        if not line or line.startswith("#"):
            continue
        source_line, mnemonic, index, mode, arg1, arg2, op1, op2, expected = line.split("\t")
        key = (int(source_line), mnemonic, int(index))
        if key in result:
            raise ValueError("duplicate fixed specimen " + str(key))
        result[key] = {
            "mode": int(mode),
            "args": [arg for arg in (arg1, arg2) if arg != "-"],
            "operands": [operand for operand in (op1, op2) if operand != "-"],
            "expected": expected,
        }
    return result


def check(row, fixture, clang, scratch):
    mode, operands, expected = fixture["mode"], fixture["operands"], fixture["expected"]
    if fixture["args"] != row["args"]:
        raise ValueError("fixed argument classes differ at source line " + str(row["line"]))
    result = {
        "line": row["line"], "mnemonic": row["name"],
        "mode": mode, "expected": expected,
    }
    if not row["opbytes"]:
        if expected != "!empty-opcode":
            raise ValueError("empty original record lacks its explicit rejection")
        return dict(result, status="empty-original-record")
    asm, notes = instruction(row, mode, operands)
    source = scratch / ("form-" + str(row["line"]) + ".s")
    obj = source.with_suffix(".obj")
    assembly = ".intel_syntax noprefix\n.text\n.code" + str(mode) + "\n.Lorigin:\n" + asm + "\n"
    for kind, operand in zip(row["args"], operands):
        if kind.startswith("REL"):
            assembly += ".set .Ltarget, .Lorigin + " + operand.split(":")[1] + "\n"
    source.write_text(assembly, encoding="utf-8", newline="\n")
    command = [clang, "--target=" + TARGET, "-c", "-x", "assembler", str(source), "-o", str(obj)]
    process = subprocess.run(command, text=True, capture_output=True, timeout=30)
    result.update(assembly=asm, notes=notes)
    if process.returncode:
        errors = [line.split("error:", 1)[1].strip()
                  for line in process.stderr.splitlines() if "error:" in line]
        return dict(result, status="assembler-rejected",
                    diagnostic=errors or ["assembler exited " + str(process.returncode)])
    actual = code_bytes(obj).hex()
    return dict(result, status="match" if expected == actual else "different-encoding", actual=actual)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--clang", default="clang")
    parser.add_argument("--scratch", type=Path, default=ROOT / "_build/encoder-audit-clang")
    parser.add_argument("--output", type=Path,
                        default=ROOT / "test/fixtures/assembly-encoder-forms.clang.json")
    args = parser.parse_args()
    version = subprocess.run([args.clang, "--version"], check=True, capture_output=True,
                             text=True, timeout=30).stdout.splitlines()[0]
    args.scratch.mkdir(parents=True, exist_ok=True)
    fixture_path = ROOT / "test/fixtures/assembly-encoder-forms.tsv"
    fixtures = fixture_rows(fixture_path)
    rows = forms.parse_source()
    keys = {(row["line"], row["name"], row["index"]) for row in rows}
    if keys != set(fixtures):
        raise ValueError("fixed specimens and original source records differ")
    with ThreadPoolExecutor(max_workers=4) as pool:
        results = list(pool.map(
            lambda row: check(row, fixtures[(row["line"], row["name"], row["index"])],
                              args.clang, args.scratch),
            rows,
        ))
    counts = {}
    for result in results:
        counts[result["status"]] = counts.get(result["status"], 0) + 1
    report = {
        "schema": "holyc-instruction-encoder-clang-audit-v1",
        "reference_commit": "c26482bb6ad3f80106d28504ec5db3c6a360732c",
        "clang_version": version,
        "target": TARGET,
        "command": ["clang", "--target=" + TARGET, "-c", "-x", "assembler", "form.s", "-o", "form.obj"],
        "source_prefix": ".intel_syntax noprefix; .text; .code16/32/64; .Lorigin:",
        "relative_target": ".set .Ltarget, .Lorigin + fixture_target",
        "fixture": "test/fixtures/assembly-encoder-forms.tsv",
        "fixture_lf_sha256": hashlib.sha256(fixture_path.read_text(encoding="utf-8").encode()).hexdigest(),
        "evidence": "External assembly comparison of fixed original-source specimens; no production encoder or live TempleOS compiler execution.",
        "interpretation": "A rejected result applies to the submitted Clang spelling, not to every possible hardware interpretation of the source record. Different bytes may reflect operand defaults, prefix order, alternative forms, or original source quirks.",
        "counts": counts,
        "matching_source_lines": [row["line"] for row in results if row["status"] == "match"],
        "other_results": [row for row in results if row["status"] != "match"],
    }
    args.output.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8", newline="\n")
    print(json.dumps({"counts": counts, "output": str(args.output)}))


if __name__ == "__main__":
    main()
