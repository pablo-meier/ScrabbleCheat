# BINGAD!

<p style="text-align: center; font-weight: bold">The variable-length binary gaddag (protocol (thing))</p>

Due to memory issues with the previous model as well as general naivite (I 
could, and did, invoke "premature optimization is the root of all evil" and
was subsequently was left with memory hell), it became necessary to redesign
the GADDAG representation in my program. What follows is how gaddags are 
represented as binaries:

Each Gaddag node is:

```
8-bit byte: unsigned number N, number of branches (may = 0).
N 8-bit byte + 32-bit word:
    8-bits: ASCII encoding of branch character.
    32-bit word: unsigned int of branch offset, in bytes.
8-bit byte: is_terminator.  0 when not, 255 when it is.
```

The binary itself is just a list of these nodes, next to each other.  You can
navigate it by jumping around the master binary, and the Erlang program just 
passes around nodes, which can be 2-bytes long (the leaves) or (27 * 5) + 2
bytes (a node with all the keys, usually near the root).


The contents of this folder build two programs: the parser itself, and a
binary to test it.


## DEPENDENCIES

I'm using clang to compile, because I'm a hipster. If you don't have it, feel 
free to use gcc or another c compiler of your choice -- it really doesn't matter.

I'm using check to unit test.

Note that the Erlang program is expecting these values in little-endian, so try
to write it that way if you can.  Is there a way you can specify this is the C?

## OUTPUT BINARY

Usage: `gaddag_parser <file1> [file2 ...]`

For every dictionary file it finds, it places another file in that directory
with a ".dict" suffix
