"""Build public-command BIN fixtures and their pinned-loader capture payload."""
import argparse
import hashlib
import json
from pathlib import Path
import subprocess
from bin_loader_model import check_modules

ROOT = Path(__file__).resolve().parents[1]
FIXTURES = ROOT / "test/oracle/bin-modules"

EXPECTED = {
    "signature": 0x42534f54, "alignment": 0, "rel8": 95, "imm8": 0x78,
    "rel16": 92, "imm16": 0x5678, "rel32": 86, "imm32": 0x12345678,
    "rel64": 82, "imm64": 0x89abcdef, "zero-width": 0,
    "abs-duplicate": 0, "abs-single": 0, "code-duplicate": 0, "code-single": 0,
    "data-duplicate": 0, "data-single": 0, "zero-code-patch": 0, "zero-data-patch": 0,
    "zero-code-bytes": 1, "zero-data-bytes": 1, "rel32-export": 96,
    "rel64-export": 104, "imm32-export": 0x12345678, "imm64-export": 0x89abcdef,
    "no-abs": 0, "just-load": 0, "main-order": 25, "source-abs": 0,
    "source-export": 32, "unresolved-byte": 0xcc, "pending": 1,
    "deferred-byte": 0x44, "deferred-word": 0x11223344, "resolved": 0,
    "heap-pending": 1, "heap-unpatched": 0xcccccccccccccccc,
    "heap-resolved": 0, "heap-final-word": 85,
}

NATIVE = r'''
I64 BMODCounter;
U0 BOut(U8 *text)
{
  I64 i;
  for (i=0;i<StrLen(text);i++) OutU8(0xE9,text[i]);
}
U0 BCap(I64 pass,U8 *id,I64 value)
{
  U8 text[256];
  StrPrint(text,"BN|%d|%s|%X\n",pass,id,value);
  BOut(text);
}
I64 BField(U8 *p,I64 offset,I64 width)
{
  I64 i,value=0;
  for (i=0;i<width;i++) value|=p[offset+i]<<(8*i);
  return value;
}
I64 BExport(U8 *name)
{
  CHashExport *entry=HashFind(name,Fs->hash_table,HTT_EXPORT_SYS_SYM);
  if (!entry) return -1;
  return entry->val;
}
I64 BPending(U8 *name)
{
  return !!HashSingleTableFind(name,Fs->hash_table,HTT_IMPORT_SYS_SYM);
}
I64 BZero(U8 *p,I64 size)
{
  I64 i;
  for (i=0;i<size;i++) if (p[i]) return FALSE;
  return TRUE;
}
U0 BRemove(U8 *name)
{
  CHash *entry;
  while (entry=HashSingleTableFind(name,Fs->hash_table,
      HTT_EXPORT_SYS_SYM|HTT_IMPORT_SYS_SYM)) HashRemDel(entry,Fs->hash_table);
}
U0 BWrite(U8 *filename,U8 *hex)
{
  I64 i,count=StrLen(hex)/2,a,b;
  U8 *bytes=MAlloc(count);
  for (i=0;i<count;i++) {
    a=hex[2*i];b=hex[2*i+1];
    if (a>='a') a=a-'a'+10; else a-='0';
    if (b>='a') b=b-'a'+10; else b-='0';
    bytes[i]=a*16+b;
  }
  FileWrite(filename,bytes,count);
  Free(bytes);
}
U0 BSource(U8 *name)
{
  I64 i,size;
  U8 text[4],*bytes=FileRead(name,&size);
  BOut("SRC|");BOut(name);BOut("|");
  for (i=0;i<size;i++) {StrPrint(text,"%02X",bytes[i]);BOut(text);}
  BOut("\n");Free(bytes);
}
U0 BRun()
{
  I64 pass,base,code,data;
  CBinFile *header,*deferred;
  U8 *p,*d;
  // Original ISO sources, expanded by the original FileRead implementation.
  BSource("T:/Kernel/KLoad.HC.Z");
  BSource("T:/Kernel/KernelA.HH.Z");
  BSource("T:/Compiler/CMain.HC.Z");
  BSource("T:/Compiler/AsmResolve.HC.Z");
  for (pass=0;pass<2;pass++) {
    header=Load("B:/ALL.BIN");p=header(U8 *)+32;base=p;
    BCap(pass,"signature",header->bin_signature);
    BCap(pass,"alignment",p&15);
    BCap(pass,"rel8",BField(p,0,1));BCap(pass,"imm8",BField(p,1,1));
    BCap(pass,"rel16",BField(p,2,2));BCap(pass,"imm16",BField(p,4,2));
    BCap(pass,"rel32",BField(p,6,4));BCap(pass,"imm32",BField(p,10,4));
    BCap(pass,"rel64",BField(p,14,8));BCap(pass,"imm64",BField(p,22,8));
    BCap(pass,"zero-width",BField(p,125,2));
    BCap(pass,"abs-duplicate",(BField(p,30,4)-2*base)&0xFFFFFFFF);
    BCap(pass,"abs-single",(BField(p,34,4)-base)&0xFFFFFFFF);
    code=BExport("BMRCode");data=BExport("BMRData");
    BCap(pass,"code-duplicate",(BField(p,38,4)-2*code)&0xFFFFFFFF);
    BCap(pass,"code-single",(BField(p,42,4)-code)&0xFFFFFFFF);
    BCap(pass,"data-duplicate",BField(p,46,8)-2*data);
    BCap(pass,"data-single",BField(p,54,8)-data);
    BCap(pass,"zero-code-patch",(BField(p,62,4)-BExport("BMRZeroCode"))&0xFFFFFFFF);
    BCap(pass,"zero-data-patch",BField(p,66,8)-BExport("BMRZeroData"));
    BCap(pass,"zero-code-bytes",BZero(BExport("BMRZeroCode"),16));
    BCap(pass,"zero-data-bytes",BZero(BExport("BMRZeroData"),16));
    BCap(pass,"rel32-export",BExport("BMRRel32")-base);
    BCap(pass,"rel64-export",BExport("BMRRel64")-base);
    BCap(pass,"imm32-export",BExport("BMRImm32"));
    BCap(pass,"imm64-export",BExport("BMRImm64"));
    header=Load("B:/ALL.BIN",LDF_JUST_LOAD|LDF_NO_ABSS);p=header(U8 *)+32;
    BCap(pass,"no-abs",BField(p,30,8));
    BMODCounter=0;header=Load("B:/CODE.BIN",LDF_JUST_LOAD);
    BCap(pass,"just-load",BMODCounter);
    BMODCounter=0;header=Load("B:/CODE.BIN");p=header(U8 *)+32;
    BCap(pass,"main-order",BMODCounter);
    BCap(pass,"source-abs",(BField(p,32,4)-p)&0xFFFFFFFF);
    BCap(pass,"source-export",BExport("data")-p);
    deferred=Load("B:/WAIT.BIN",LDF_JUST_LOAD|LDF_SILENT);d=deferred(U8 *)+32;
    BCap(pass,"unresolved-byte",BField(d,0,1));
    BCap(pass,"pending",BPending("BMRLater"));
    Load("B:/PROV.BIN",LDF_JUST_LOAD);
    BCap(pass,"deferred-byte",BField(d,0,1));
    BCap(pass,"deferred-word",BField(d,1,4));
    BCap(pass,"resolved",BPending("BMRLater"));
    BCap(pass,"heap-pending",BPending("BMRLaterHeap"));
    BCap(pass,"heap-unpatched",BField(d,8,8));
    Load("B:/RES.BIN",LDF_JUST_LOAD);
    BCap(pass,"heap-resolved",BPending("BMRLaterHeap"));
    BCap(pass,"heap-final-word",BField(d,8,8));
    BRemove("BMRCode");BRemove("BMRZeroCode");BRemove("BMRData");BRemove("BMRZeroData");
    BRemove("BMRRel32");BRemove("BMRRel64");BRemove("BMRImm32");BRemove("BMRImm64");
    BRemove("BMRLater");BRemove("BMRLaterHeap");BRemove("entry");BRemove("data");
  }
  BOut("BINDONE\n");
}
'''


def build(executable, destination):
    destination.mkdir(parents=True, exist_ok=True)
    cases = [
        ("CODE", ROOT / "examples/templeos-module.asm", None,
         ["--main=entry", "--main=second", "--main=second"]),
        ("ALL", FIXTURES / "all-records.asm", FIXTURES / "all-records.json", []),
        ("WAIT", FIXTURES / "deferred.asm", FIXTURES / "deferred.json", []),
        ("PROV", FIXTURES / "provider.asm", FIXTURES / "provider.json", []),
        ("RES", FIXTURES / "provider.asm", FIXTURES / "resolve.json", []),
    ]
    writes = []
    modules = []
    for name, source, records, options in cases:
        path = destination / (name + ".BIN")
        command = [str(executable), "bin", str(source), "--format=json",
                   "--output=" + str(path), *options]
        if records:
            command.append("--records=" + str(records))
        report = json.loads(subprocess.check_output(command, cwd=ROOT, text=True))
        raw = path.read_bytes()
        modules.append({"name": name, "sha256": hashlib.sha256(raw).hexdigest(),
                        "hex": raw.hex(), "report": report,
                        "source": source.relative_to(ROOT).as_posix(),
                        "records": records.relative_to(ROOT).as_posix() if records else None,
                        "options": options})
        writes.append(f'BWrite("B:/{name}.BIN","{raw.hex()}");')
    payload = NATIVE + "\n" + "\n".join(writes) + "\nBRun;\n"
    (destination / "ORACLE.HC").write_bytes(payload.encode("ascii"))
    (destination / "modules.json").write_text(json.dumps(modules, indent=2) + "\n")
    print("payload_sha256=" + hashlib.sha256(payload.encode("ascii")).hexdigest())


def record_capture(destination, capture_path, iso_path):
    raw = capture_path.read_bytes()
    lines = raw.decode("ascii").splitlines()
    if not lines or lines[-1] != "BINDONE":
        raise ValueError("native capture is incomplete")
    observed = {0: {}, 1: {}}
    source_checks = []
    for line in lines[:-1]:
        if line.startswith("BN|"):
            _, pass_id, name, value = line.split("|")
            pass_id = int(pass_id)
            if pass_id not in observed or name in observed[pass_id]:
                raise ValueError("duplicate or unknown native observation")
            observed[pass_id][name] = int(value, 16)
        elif line.startswith("SRC|"):
            _, name, encoded = line.split("|")
            source = bytes.fromhex(encoded)
            relative = name[3:-2]
            pinned = (ROOT / "third_party/TempleOS" / relative).read_bytes()
            normalized = source.replace(b"\r\n", b"\n").replace(b"\x1f", b" ")
            if normalized != pinned.replace(b"\r\n", b"\n"):
                raise ValueError("ISO source differs from pinned Git source: " + relative)
            source_checks.append({"path": relative, "expanded_sha256": hashlib.sha256(source).hexdigest(),
                                  "normalized_sha256": hashlib.sha256(normalized).hexdigest(),
                                  "crlf_pairs": source.count(b"\r\n"), "shift_spaces": source.count(b"\x1f")})
        else:
            raise ValueError("unknown native capture record")
    if observed[0] != EXPECTED or observed[1] != EXPECTED or len(source_checks) != 4:
        raise ValueError("native observations differ from independent expectations")
    modules = json.loads((destination / "modules.json").read_text())
    check_modules(modules)
    payload = (destination / "ORACLE.HC").read_bytes()
    metadata = {"fixture_version": 1, "reference_commit": "c26482bb6ad3f80106d28504ec5db3c6a360732c",
                "original_iso_sha256": "3c68ca395d0b64f779f1ffa330e2685798b17862e3c36ae4e349537fd676b40b",
                "boot_iso_sha256": hashlib.sha256(iso_path.read_bytes()).hexdigest(),
                "payload_sha256": hashlib.sha256(payload).hexdigest(), "capture_sha256": hashlib.sha256(raw).hexdigest(),
                "source_checks": source_checks, "expected": {k: format(v,"X") for k,v in EXPECTED.items()},
                "passes": 2, "model_status": "passed", "modules": modules}
    (FIXTURES / "native.hc").write_bytes(payload)
    (FIXTURES / "native.capture").write_bytes(raw)
    (FIXTURES / "native.json").write_text(json.dumps(metadata, indent=2) + "\n")
    print(f"verified {len(EXPECTED)*2} native observations; independent loader model passed")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--holyc", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--capture", type=Path)
    parser.add_argument("--boot-iso", type=Path)
    args = parser.parse_args()
    if args.capture:
        if not args.boot_iso:
            parser.error("--capture requires --boot-iso")
        record_capture(args.output_dir.resolve(), args.capture.resolve(), args.boot_iso.resolve())
    else:
        build(args.holyc.resolve(), args.output_dir.resolve())
