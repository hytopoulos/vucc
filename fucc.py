# Wrapper/Driver for SFCC
# Please make sure qemu-i386 is installed

import os, re

def encrypt(f_in, f_out):
    with open(f_in, "r") as f:
        data = f.read()
    data = re.sub(r"\t", "\x16", data)
    data = bytes([(((x - 0x20) ^ 0xFF) & 0xFF) for x in data.encode()])
    with open(f_out, "wb") as f:
        f.write(data)

# for _, _, files in os.walk("."):
#     for f in files:
#         if f.endswith(".cl"):
#             encrypt(f"./{f}", f"inject/{f}")
#     break # only traverse root

cmd = "qemu-system-i386 -hda msdos.disk -m 64 -L . -hdb fat:rw:. -nographic"
os.system(cmd)
