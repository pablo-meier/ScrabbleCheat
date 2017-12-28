Scrabblecheat Server
=====

The brain of all of ScrabbleCheat, using [GADDAGs][2] to generate moves on a
board given a rack. We used [Apache Thrift][3]'s Erlang bindings circa 2011 to
RPC to clients, but maybe I'll bolt on an HTTP server or something.

## Running instructions

I wrote the bulk of this in 2010, when I was less experienced and we had way
less consensus on what was good or not.

To open a shell:

* Ensure you have [`rebar3`][1] and a C compiler installed.
* `make`

### What's happening

#### Building GADDAGs with a C executable

ScrabbleCheat uses a custom binary format for its GADDAGs. I stated why in the
`bin_trie` comments, but we took a 300MB binary structure and made it 30MB. To
build these GADDAGs, we use a C program called Bingad. It lives in `lib/bingad`

So the Makefile first compiles the C program, then uses it to make the
dictionaries. It also has unit tests, but you need to ensure that [Check][4]
is available to be linked.

#### Rebar3 for dependencies

From here, we do the more formal Erlang build process, powered mostly by rebar3.
Bless the authors for improving the tooling.

   [1]: http://www.rebar3.org/
   [2]: https://en.wikipedia.org/wiki/GADDAG
   [3]: https://thrift.apache.org/
   [4]: https://libcheck.github.io/check/
