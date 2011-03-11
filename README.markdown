# ScrabbleCheat #

## Revenge of the Verbally Challenged ##

ScrabbleCheat is a tool I'm developing to finally beat my girlfriend in 
Scrabble.  Developing it in Erlang, mostly for fun.  Use it yourself, or explore
the code!

Originally development was considered in phases: generate words from your rack,
then place the words on the board.  This was using a DAWG algorithm most popular
in other Scrabble AIs such as 
[Maven](http://en.wikipedia.org/wiki/Maven_(Scrabble)).  If you checkout the 
repo at tag WORDSEARCH, you will see this!  Run 'make run' from that commit and 
you get a simple trie-based search to generate words from your rack of tiles.

This was eschewed in favor of a GADDAG-based approach, such as the one used in
[Quackle](http://people.csail.mit.edu/jasonkb/quackle/).  A 
[GADDAG](http://en.wikipedia.org/wiki/GADDAG), for those who
don't know, is a perverted trie that is fat and fast, designed specifically for
Scrabble and games with similar rules.

## Contents ##

ScrabbleCheat has the following contents:

* `Makefile`. Builds the project, targets below.
* `code/`. Contains the project code.  This is for the server (the 'brains,' in
  Erlang) as well as the clients that communicate with it.  Each is a 
  sub-project, and contains their own build/testing logic as appropriate.
* `lib/`. Contains any extraneous support, such as testing macros, dictionary 
  files, Thrift libraries, and the like.
* `TODO`. Small lists of tasks for the project.  This is my PM.

## Requirements ##

ScrabbleCheat and it's various components have a ton.  The subprojects have 
their own, here are the ones I know off the top of my head:

### Global ###

* Thrift -> Apache Thrift is used to build language-agnostic web services 
  automatically, and it's how my AI communicates with any client or
  service who wants to use it to cheat. See lib/ScrabbleCheat.thrift
  for the specification. We build the files dynamically, so you'll 
  need Thrift installed to build the files that communicate with one another.
* Make -> I'm using the GNU version that came with Mac OS X.  Even if we're 
  using other build utilities, Make is a pretty simple no-nonsense way to 
  automate it all.

### Server ###

* Erlang VM, naturally.
* [Rebar](https://github.com/basho/rebar) for builds and tests.

### Curses Client ###

* [Ncurses](http://www.gnu.org/software/ncurses/).
* [Ncurses-ruby](https://github.com/eclubb/ncurses-ruby) (Note that the linked
  implementation has issues on Mac, see [this
  gem](http://rubygems.org/gems/snowleopard-ncurses) for a solution.

### Adobe AIR Client ###

* Ensure you have the Flex SDK tools in your PATH (adt, adl, etc.).

## Milestones ##

Check out these git tags for particular milestones in the project.

* WORDSEARCH -> This is your basic anagram solver.  Plug in letters and it 
  returns a list of words you can generate with those letters.  Use '*' is 
  for a wildcard.

* GADDAG_WITH_CLIENT -> The first "real" "release," a simple ncurses UI
  is provided to communicate to the Erlang backend, which allows you to
  either add moves (such as your opponent's) or query the game for the
  4 highest scoring moves given a rack.  Requires ncurses-ruby 
  package to use the client. 

* WITH_THRIFT_V1 -> The GADDAG_WITH_CLIENT used a serialization scheme 
  that I made up, and was brittle/insecure as a server.  I moved the
  client/server to use Apache Thrift, which while stil a little buggy
  (the server won't start unless manually started from the Erlang 
  shell, -s and -run make it break) allows for better server behaviour,
  as well as a much more maintainable service that's easy to add clients
  for.

----

For now this is just loose code that will hopefully piece together into actual 
usable software; in the meantime, I'm putting it here as well as my local 
machine.  Enjoy, and email about any questions, concerns!

- Paul Meier
02/15/2011
pablo.a.meier@gmail.com  
www.morepaul.com
