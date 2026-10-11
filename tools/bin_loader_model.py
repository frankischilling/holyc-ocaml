"""Independent bounded data model of KLoad.HC; never executes module bytes."""
import struct

IMPORT_WIDTHS = {2: 0, 3: 0, 4: 1, 5: 1, 6: 2, 7: 2, 8: 4, 9: 4, 10: 8, 11: 8}


def inspect(raw):
    if len(raw) < 48 or len(raw) > 16 * 1024 * 1024:
        raise ValueError("module size")
    jump, align, reserved, signature, origin, patch, size = struct.unpack_from("<HBBIqqq", raw)
    if (jump, reserved, signature, size) != (0x1eeb, 0, 0x42534f54, len(raw)):
        raise ValueError("module header")
    if align > 62 or size % 16 or not 32 <= patch < size:
        raise ValueError("module coordinates")
    records = []
    cursor = patch

    def take(width):
        nonlocal cursor
        if cursor + width > size:
            raise ValueError("truncated record")
        value = int.from_bytes(raw[cursor:cursor+width], "little")
        cursor += width
        return value

    while True:
        kind = take(1)
        if kind == 0:
            break
        if kind not in {*IMPORT_WIDTHS, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25}:
            raise ValueError("unknown IET")
        value = take(4)
        end = raw.find(b"\0", cursor)
        if end < 0:
            raise ValueError("unterminated name")
        name = raw[cursor:end].decode("ascii")
        cursor = end+1
        record = dict(kind=kind, value=value, name=name)
        if kind in (20, 21, 22, 23, 24):
            if kind != 20:
                record["size"] = take(4 if kind in (21, 22) else 8)
                if record["size"] > 64 * 1024 * 1024:
                    raise ValueError("model heap budget")
            if value > (size-cursor)//4:
                raise ValueError("truncated references")
            record["offsets"] = [take(4) for _ in range(value)]
        records.append(record)
    if size-cursor > 15 or any(raw[cursor:]):
        raise ValueError("module padding")
    return bytearray(raw[32:patch]), records, origin, align


class Loader:
    def __init__(self, symbols=None):
        self.symbols = dict(symbols or {})
        self.pending = {}
        self.heaps = {}
        self.next_code = 0x20000000
        self.next_data = 0x30000000
        self.heap_bytes = 0

    @staticmethod
    def store(body, offset, width, value):
        if not 0 <= offset <= len(body)-width:
            raise ValueError("patch outside body")
        body[offset:offset+width] = (value % (1 << (8*width))).to_bytes(width, "little")

    def imports(self, group, body, base, target):
        for record in group:
            width = IMPORT_WIDTHS[record["kind"]]
            offset = record["value"]
            value = target
            if record["kind"] % 2 == 0:
                value -= base+offset+width
            self.store(body, offset, width, value)

    def load(self, raw, base, *, no_abs=False, just_load=False):
        body, records, origin, align = inspect(raw)
        if base < 0 or base+len(body) > (1 << 63)-1:
            raise ValueError("model address range")
        if origin == (1 << 63)-1 and base % (1 << align):
            raise ValueError("model alignment")
        mains = []
        index = 0
        while index < len(records):
            record = records[index]
            kind, value, name = record["kind"], record["value"], record["name"]
            index += 1
            if kind in IMPORT_WIDTHS:
                group = [record]
                if not name:
                    raise ValueError("unnamed import head")
                while index < len(records) and not records[index]["name"]:
                    continuation = records[index]
                    if continuation["kind"] not in IMPORT_WIDTHS:
                        raise ValueError("nonimport consumed by import group")
                    group.append(continuation)
                    index += 1
                if name in self.symbols:
                    self.imports(group, body, base, self.symbols[name])
                else:
                    self.pending.setdefault(name, []).append((group, body, base))
            elif kind in (16, 17, 18, 19):
                self.symbols[name] = value + (base if kind in (16, 18) else 0)
                for group, waiting, waiting_base in self.pending.pop(name, []):
                    self.imports(group, waiting, waiting_base, self.symbols[name])
            elif kind == 20:
                if not no_abs:
                    for offset in record["offsets"]:
                        initial = int.from_bytes(body[offset:offset+4], "little")
                        self.store(body, offset, 4, initial+base)
            elif kind in (21, 22, 23, 24):
                self.heap_bytes += record["size"]
                if self.heap_bytes > 64 * 1024 * 1024:
                    raise ValueError("cumulative model heap budget")
                code = kind in (21, 22)
                pointer = self.next_code if code else self.next_data
                step = max(16, (record["size"]+15)//16*16)
                if code:
                    self.next_code += step
                else:
                    self.next_data += step
                # Nonzeroed contents are deliberately unknown.
                self.heaps[pointer] = bytes(record["size"]) if kind in (22, 24) else None
                if name:
                    self.symbols[name] = pointer
                width = 4 if code else 8
                for offset in record["offsets"]:
                    initial = int.from_bytes(body[offset:offset+width], "little")
                    self.store(body, offset, width, initial+pointer)
            elif kind == 25:
                if not 0 <= value < len(body):
                    raise ValueError("MAIN outside body")
                if not just_load:
                    mains.append(base+value)
        return body, mains


def check_modules(modules):
    raw = {module["name"]: bytes.fromhex(module["hex"]) for module in modules}
    loader = Loader({"BMODCounter": 0x40000000})
    body, mains = loader.load(raw["ALL"], 0x10000000)
    assert mains == [0x1000007f]*2
    assert body[:30].hex() == "5f785c00785656000000785634125200000000000000efcdab8900000000"
    assert int.from_bytes(body[30:34], "little") == 0x20000000
    assert int.from_bytes(body[34:38], "little") == 0x10000000
    assert int.from_bytes(body[38:42], "little") == 0x40000000
    assert int.from_bytes(body[42:46], "little") == 0x20000000
    assert int.from_bytes(body[46:54], "little") == 0x60000000
    assert int.from_bytes(body[54:62], "little") == 0x30000000
    assert loader.heaps[loader.symbols["BMRZeroCode"]] == bytes(16)
    assert loader.heaps[loader.symbols["BMRZeroData"]] == bytes(16)
    no_abs, skipped = loader.load(raw["ALL"], 0x10001000, no_abs=True, just_load=True)
    assert no_abs[30:38] == bytes(8) and skipped == []
    code, mains = loader.load(raw["CODE"], 0x10002000)
    assert mains == [0x10002000, 0x1000200c, 0x1000200c]
    assert int.from_bytes(code[1:5], "little") == 0x40000000
    assert int.from_bytes(code[13:17], "little") == 0x40000000
    waiting, _ = loader.load(raw["WAIT"], 0x10003000, just_load=True)
    assert waiting == bytes([0xcc])*24 and set(loader.pending) == {"BMRLater", "BMRLaterHeap"}
    loader.load(raw["PROV"], 0x10004000, just_load=True)
    assert waiting[:5] == bytes.fromhex("4444332211")
    assert waiting[8:16] == bytes([0xcc])*8 and set(loader.pending) == {"BMRLaterHeap"}
    loader.load(raw["RES"], 0x10005000, just_load=True)
    assert waiting[8:16] == bytes.fromhex("5500000000000000") and not loader.pending


if __name__ == "__main__":
    import argparse
    import json
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("metadata")
    args = parser.parse_args()
    with open(args.metadata) as source:
        check_modules(json.load(source)["modules"])
    print("independent BIN loader model passed")
