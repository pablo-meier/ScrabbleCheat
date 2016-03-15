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

* make Releases work
* * make Thrift a plugin?
* make env_vars a thing.
* Move bingad to c_src?
* Check in `.dict`s?
* Refactor lol

* integration tests
* test on "clean build"

Build
-----

    $ rebar3 compile
