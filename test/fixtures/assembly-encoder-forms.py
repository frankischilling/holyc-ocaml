"""Rebuild the reviewed simple-operand inventory from the pinned source.

This is an independent fixture construction aid, not a test oracle at runtime.
It reads only original OpCodes.DD and applies the canonical specimens documented
in docs/instruction-encoder.md. The committed TSV contains the expected bytes.
It does not import, build, or execute the OCaml encoder.
"""
from pathlib import Path
import json
import re

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / "third_party/TempleOS/Compiler/OpCodes.DD"
OUTPUT = ROOT / "test/fixtures/assembly-encoder-forms.tsv"
ARGUMENT = re.compile(r"^(?:REL(?:8|16|32)|U?IMM(?:8|16|32|64)|R(?:M)?(?:8|16|32|64)|M(?:OFFS)?(?:8|16|32|64)|AL|AX|EAX|RAX|CL|DX|SREG|ES|CS|SS|DS|FS|GS|ST0|STI)$")
NUMBERS = {"IMM8": -85, "IMM16": -4660, "IMM32": -305419896,
           "IMM64": -72623859790382856, "UIMM8": 165,
           "UIMM16": 48879, "UIMM32": 2309737967,
           "UIMM64": 1234605616436508552,
           "REL8": 42, "REL16": 4660, "REL32": 305419896}
GPRS = {8: "DL", 16: "DX", 32: "EDX", 64: "RDX"}
REG_NUMBERS = {"AL": 0, "AX": 0, "EAX": 0, "RAX": 0, "CL": 1,
               "DX": 2, "DL": 2, "EDX": 2, "RDX": 2, "ST0": 0,
               "ST3": 3, "ES": 0, "CS": 1, "SS": 2, "DS": 3,
               "FS": 4, "GS": 5}
MODIFIERS = {"NO": 0, "CB": 1, "CW": 2, "CD": 4, "CP": 0,
             "IB": 1, "IW": 2, "ID": 4}

def parse_source():
    text = SOURCE.read_text(encoding="utf-8")
    text = re.sub(r"/\*.*?\*/", lambda m: "\n" * m.group().count("\n"), text, flags=re.S)
    records = []
    opcode = None
    index = 0
    for line_no, raw in enumerate(text.splitlines(), 1):
        line = raw.split("//", 1)[0].strip()
        if not line:
            continue
        match = re.match(r"OPCODE\s+(\w+)\b(.*)", line)
        if match:
            opcode, line = match.groups()
            index = 0
            line = line.strip()
        elif not opcode:
            continue
        if not line:
            continue
        original = line.split(":", 1)[0].rstrip(";").strip()
        if not original:
            if ";" in line:
                opcode = None
            continue
        # A bare comma at REP_OUTSB is the source's zero-byte record.
        if not original.startswith(("0x", ",")):
            raise ValueError((line_no, opcode, original))
        fields = original.split(",", 1)
        opbytes = [int(x, 16) for x in re.findall(r"0x[0-9A-Fa-f]+", fields[0])]
        tail = fields[1] if len(fields) == 2 else ""
        tokens = re.findall(r"[A-Z][A-Z0-9]*|[0-9]+|[^\s]", tail)
        args = [t for t in tokens if ARGUMENT.fullmatch(t)]
        if len(args) > 2:
            raise ValueError((line_no, args))
        modifier = next((t for t in tokens if t in MODIFIERS), "NO")
        slash = re.search(r"([/+])\s*([0-7RI])", tail)
        records.append(dict(line=line_no, name=opcode, index=index,
                            opbytes=opbytes, tail=tail, args=args,
                            modifier=modifier, slash=slash.group(2) if slash else None,
                            plus=bool(slash and slash.group(1) == "+")))
        index += 1
        if ";" in line:
            opcode = None
    assert len(records) == 924, len(records)
    return records

def mode_for(row):
    args, tail = row["args"], row["tail"]
    mode = 64 if any(a.endswith("64") for a in args) or "=" in tail or 0x48 in row["opbytes"] else 32
    if "%" in tail:
        mode = 32
    if "!" in tail and re.search(r"\b16\b", tail):
        mode = 16
    return mode

def operand(kind, mode):
    if kind in NUMBERS:
        return "imm:" + str(NUMBERS[kind])
    # CALL/JMP's USE16 RM16 records forbid the address prefix introduced by
    # memory operands. Asm.HC:555-557 clears that prefix for a direct register.
    if kind == "RM16" and mode == 16:
        return "register:DX"
    if kind.startswith("MOFFS"):
        return "address:" + str(int(kind[5:]) // 8) + ":305419896"
    if kind.startswith("RM") or re.fullmatch(r"M(?:8|16|32|64)", kind):
        width = int(re.search(r"\d+", kind).group()) // 8
        return "memory:" + str(width) + (":RBX" if mode == 64 else ":EBX")
    if re.fullmatch(r"R(?:8|16|32|64)", kind):
        return "register:" + GPRS[int(kind[1:])]
    if kind == "SREG":
        return "register:DS"
    return "register:" + ("ST3" if kind == "STI" else kind)

def register_number(specimen):
    if specimen.startswith("register:"):
        return REG_NUMBERS[specimen.split(":")[1]]
    if specimen.startswith("memory:"):
        return 3
    return -1

def little(value, count):
    return list((value & ((1 << (count * 8)) - 1)).to_bytes(count, "little"))

def specimen_bytes(row, mode, operands):
    if not row["opbytes"]:
        return "!empty-opcode"
    args, tail = row["args"], row["tail"]
    opcode = list(row["opbytes"])
    prefix = []
    if (mode == 16 and re.search(r"\b32\b", tail)) or (mode != 16 and re.search(r"\b16\b", tail)):
        prefix.append(0x66)
    rex = 8 if "=" in tail else 0
    if any(a in {"R64", "RM64", "M64"} for a in args):
        rex |= 8
    if row["plus"]:
        if row["slash"] == "R":
            chosen = 1 if len(args) == 2 and args[0] in {"AL", "AX", "EAX", "RAX"} else 0
            opcode[-1] |= register_number(operands[chosen]) & 7
        elif len(args) == 1:
            opcode[-1] += register_number(operands[0])
        else:
            for kind, specimen in zip(args, operands):
                if kind in {"ST0", "STI"}:
                    opcode[-1] |= register_number(specimen)
    rm_index = next((i for i, a in reversed(list(enumerate(args))) if a.startswith("RM") or re.fullmatch(r"M(?:8|16|32|64)", a)), None)
    modrm = []
    if row["slash"] and row["slash"].isdigit():
        modrm = [int(row["slash"]) << 3]
    if rm_index is not None:
        indirect = operands[rm_index].startswith("memory:")
        if mode == 16 and indirect:
            prefix.append(0x67)
        reg = int(row["slash"]) if row["slash"] and row["slash"].isdigit() else 0
        if len(args) == 2 and row["slash"] == "R":
            reg = register_number(operands[1 - rm_index])
            if reg == -1:
                reg = register_number(operands[rm_index])
        modrm = [(reg << 3) | register_number(operands[rm_index]) | (0 if indirect else 0xC0)]
    immediate = []
    displacement = []
    imm_width = MODIFIERS[row["modifier"]]
    imm_value = None
    for kind, specimen in zip(args, operands):
        if kind.startswith("MOFFS"):
            displacement = little(int(specimen.split(":")[2]), 2 if mode == 16 else 4)
        if kind in NUMBERS:
            imm_value = NUMBERS[kind]
            if len(args) == 2 and not kind.startswith("REL"):
                imm_width = int(re.search(r"\d+", kind).group()) // 8
                if imm_width == 8:
                    rex |= 8
            if kind == "UIMM32" and chr(96) in tail:
                rex &= ~8
    # The source suppresses REX.W-only on backtick forms with low registers.
    if mode == 64 and rex and chr(96) not in tail:
        prefix.append(0x40 | rex)
    if imm_width:
        assert imm_value is not None, row
        if row["modifier"] in {"CB", "CW", "CD"}:
            imm_value -= len(prefix) + len(opcode) + len(modrm) + len(displacement) + imm_width
        immediate = little(imm_value, imm_width)
    ending = [0] if "$$" in tail else []
    return bytes(prefix + opcode + modrm + displacement + immediate + ending).hex()

def write_inventory():
    rows = parse_source()
    output = [
        "# Fixed expected bytes derived from pinned Compiler/OpCodes.DD and Asm.HC.",
        "# source_line\tmnemonic\tentry_index\tmode\targument1\targument2\toperand1\toperand2\texpected_hex",
    ]
    for row in rows:
        mode = mode_for(row)
        operands = [operand(a, mode) for a in row["args"]]
        expected = specimen_bytes(row, mode, operands)
        args = row["args"] + ["-"] * (2 - len(row["args"]))
        operands = operands + ["-"] * (2 - len(operands))
        output.append("\t".join(map(str, [row["line"], row["name"], row["index"], mode, *args, *operands, expected])))
    OUTPUT.write_text("\n".join(output) + "\n", encoding="utf-8")
    print(json.dumps({"records": len(rows), "empty_records": [r["line"] for r in rows if not r["opbytes"]], "output": str(OUTPUT)}))

if __name__ == "__main__":
    write_inventory()
