"""Add the capture payload to a verified, read-only TempleOS boot image copy."""
import argparse
import hashlib
from pathlib import Path
import struct

REFERENCE_SHA256 = "3c68ca395d0b64f779f1ffa330e2685798b17862e3c36ae4e349537fd676b40b"


def prepare(iso_path, payload_path, output_path):
    image = bytearray(iso_path.read_bytes())
    if hashlib.sha256(image).hexdigest() != REFERENCE_SHA256:
        raise ValueError("input is not the verified TempleOS final ISO")
    payload = payload_path.read_bytes()
    if not payload or len(payload) > 1024 * 1024:
        raise ValueError("capture payload must be between one byte and 1 MiB")
    # DskISORedSea.HC and KernelA.HH: the El Torito prefix is followed by
    # CRedSeaBoot at block 88. CDirEntry.start serializes exactly 64 bytes.
    boot = 22 * 2048
    if image[boot + 3] != 0x88 or image[boot + 510:boot + 512] != b"\x55\xaa":
        raise ValueError("unexpected RedSea boot record")
    root = struct.unpack_from("<q", image, boot + 24)[0] * 512
    size = struct.unpack_from("<q", image, root + 48)[0]
    if image[root + 2:root + 4] != b".\0" or size <= 0 or root + size > len(image):
        raise ValueError("unexpected root directory")
    slots = [root + index for index in range(0, size, 64)
             if image[root + index:root + index + 64] == bytes(64)]
    if not slots:
        raise ValueError("root directory has no unused entry")
    entry = bytearray(64)
    struct.pack_into("<H", entry, 0, 0x800)  # RS_ATTR_CONTIGUOUS
    entry[2:12] = b"ORACLE.HC\0"
    struct.pack_into("<qq", entry, 40, len(image) // 512, len(payload))
    image[slots[0]:slots[0] + 64] = entry
    image.extend(payload)
    image.extend(bytes((-len(image)) % 2048))
    struct.pack_into("<q", image, boot + 16, len(image) // 512 - 88)
    for sector in (16, 18):
        struct.pack_into("<I", image, sector * 2048 + 80, len(image) // 2048)
        struct.pack_into(">I", image, sector * 2048 + 84, len(image) // 2048)
    # Exclusive creation preserves the original and any existing destination.
    with output_path.open("xb") as output:
        output.write(image)
    print("payload_sha256=" + hashlib.sha256(payload).hexdigest())
    print("boot_iso_sha256=" + hashlib.sha256(image).hexdigest())


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--iso", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--payload", type=Path, default=Path(__file__).resolve().parents[1]
                        / "test/oracle/source-disassembly.hc")
    args = parser.parse_args()
    try:
        prepare(args.iso, args.payload, args.output)
    except (OSError, ValueError) as error:
        parser.exit(1, str(error) + "\n")
