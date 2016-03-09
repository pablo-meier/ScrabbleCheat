scrabblecheat
=====

An OTP application

### Running instructions

* Make on `/lib/bingad`

* Run `/lib/bingad/bin/gaddag_parser` on the TWL06, SOWPODS, and Words with
Friends dictionaries.

* Move it to the app's priv\_dir.

* `cd lib/thrift && ./rebar compile`

* `rebar3 shell`

### TODO

* Move bingad to c_src?
* make Thrift a plugin?
* Refactor lol

* integration tests
* test on "clean build"

Build
-----

    $ rebar3 compile
