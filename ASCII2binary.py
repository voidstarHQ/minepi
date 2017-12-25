#!/usr/bin/env python3

import math
import sys

if len(sys.argv) != 2 :
    print('Usage: {} <file> >file.bin'.format(sys.argv[0]), file=sys.stderr)
    sys.exit(1)

input = sys.argv[1]

alphabet = None
with open(input, 'r') as fd:
    alphabet = set(fd.read())
alphabet = ''.join(sorted(list(alphabet - set('\n'))))
bits = math.ceil(math.log2(len(alphabet)))
bits_per_byte = int(8 / bits)
print('>>>', 'len({}) = {}, {} bits --> {} per byte'
      .format(alphabet, len(alphabet), bits, bits_per_byte),
      file=sys.stderr)
##>>> len(acgt) = 4, 2 bits --> 4 per byte
##>>> len(0123456789) = 10, 4 bits --> 2 per byte

buffer_size = bits_per_byte -1
if buffer_size < 1:
    raise Exception('alphabet too large')

with open(sys.argv[1], 'r') as fd:
    with open(1, 'wb') as out:
        while True:
            byte, first = 0, True
            for _ in range(buffer_size + 1):##
                read = fd.read(1)
                if not read:
                    sys.exit()
                if read in "\n":
                    break
                b = alphabet.index(read)
                if first:
                    first = False
                    byte += b
                else:
                    byte = (byte << bits) + b
            try:
                out.write(bytes([byte]))
            except:
                print("Unexpected error:", sys.exc_info(), file=sys.stderr)
                raise
