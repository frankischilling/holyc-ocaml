"""Capture original Cmp-produced BIN artifacts from the verified TempleOS ISO."""
import argparse
import hashlib
import json
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
FIXTURES = ROOT / "test/oracle/bin-validator"
SOURCES = {
    "ASM": b"asm {\nUSE64\nIMPORT NVImport;\nNVEntry:: MOV RAX,NVImport; RET;\nNVDatum:: DU32 NVEntry;\n}\n",
    "HOLYC": b'#include "T:/Kernel/KernelA.HH"\nI64 NVGlobal;\nI64 NVFunction(I64 x) {return x+NVGlobal;}\nNVGlobal=17;\n',
}
def payload():
    helpers = (ROOT / "tools/bin-oracle.py").read_text().split("NATIVE = r'''", 1)[1].split("'''", 1)[0]
    # Only use byte output / file writing helpers, not the writer-produced modules.
    helpers = helpers[helpers.index("U0 BOut"):helpers.index("I64 BField")] + helpers[helpers.index("U0 BWrite"):helpers.index("U0 BSource")]
    calls = []
    for name, source in SOURCES.items():
        calls += [f'BWrite("B:/{name}.HC","{source.hex()}");',
                  f'BCap(0,"{name}-errors",Cmp("B:/{name}.HC",,"B:/{name}.BIN"));',
                  f'NCapture("{name}","B:/{name}.BIN");']
    capture = r'''
U0 NCapture(U8 *id,U8 *filename)
{
  I64 i,size;
  U8 *p=FileRead(filename,&size),text[32];
  BOut("NATIVE|");BOut(id);BOut("|");
  for (i=0;i<size;i++) {StrPrint(text,"%02X",p[i]);BOut(text);}
  BOut("\n");Free(p);
}
'''
    return (helpers + capture + "\n".join(calls) + '\nBOut("NATIVEDONE\\n");\n').encode()

def record(capture, iso):
    raw = capture.read_bytes()
    artifacts = {}
    errors = {}
    lines = raw.decode("ascii").splitlines()
    if not lines or lines[-1] != "NATIVEDONE":
        raise ValueError("capture is incomplete")
    for line in lines[:-1]:
        if line.startswith("NATIVE|"):
            _, name, encoded = line.split("|")
            if name in artifacts or name not in SOURCES:
                raise ValueError("duplicate or unknown artifact")
            artifacts[name] = bytes.fromhex(encoded)
        elif line.startswith("BN|0|"):
            _, _, name, value = line.split("|")
            if name in errors:
                raise ValueError("duplicate error count")
            errors[name] = int(value, 16)
        else:
            raise ValueError("unknown capture record")
    if set(artifacts) != set(SOURCES) or errors != {name + "-errors": 0 for name in SOURCES}:
        raise ValueError("original compilation failed or artifact missing")
    FIXTURES.mkdir(parents=True, exist_ok=True)
    meta = {"reference_commit": "c26482bb6ad3f80106d28504ec5db3c6a360732c",
            "original_iso_sha256": "3c68ca395d0b64f779f1ffa330e2685798b17862e3c36ae4e349537fd676b40b",
            "boot_iso_sha256": hashlib.sha256(iso.read_bytes()).hexdigest(),
            "payload_sha256": hashlib.sha256(payload()).hexdigest(),
            "capture_sha256": hashlib.sha256(raw).hexdigest(), "artifacts": []}
    for name, data in artifacts.items():
        (FIXTURES / (name + ".BIN")).write_bytes(data)
        (FIXTURES / (name + ".HC")).write_bytes(SOURCES[name])
        meta["artifacts"].append({"name": name, "sha256": hashlib.sha256(data).hexdigest(),
                                   "source_sha256": hashlib.sha256(SOURCES[name]).hexdigest()})
    (FIXTURES / "native.hc").write_bytes(payload())
    (FIXTURES / "native.capture").write_bytes(raw)
    (FIXTURES / "native.json").write_text(json.dumps(meta, indent=2) + "\n")
    print("captured original compiler artifacts: " + ", ".join(artifacts))

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--payload", type=Path)
    parser.add_argument("--capture", type=Path)
    parser.add_argument("--boot-iso", type=Path)
    parser.add_argument("--check-fixtures", action="store_true")
    args = parser.parse_args()
    if args.check_fixtures:
        meta = json.loads((FIXTURES / "native.json").read_text())
        if (FIXTURES / "native.hc").read_bytes() != payload():
            raise ValueError("native producer payload does not match its generator")
        if hashlib.sha256(payload()).hexdigest() != meta["payload_sha256"]:
            raise ValueError("native payload digest mismatch")
        for name, source in SOURCES.items():
            if (FIXTURES / (name + ".HC")).read_bytes() != source:
                raise ValueError("native producer source mismatch: " + name)
        print("original compiler capture recipes reproduced")
    elif args.capture and args.boot_iso:
        record(args.capture, args.boot_iso)
    elif args.payload:
        args.payload.write_bytes(payload())
        print("payload_sha256=" + hashlib.sha256(payload()).hexdigest())
    else:
        parser.error("use --payload PATH or --capture PATH --boot-iso PATH")
