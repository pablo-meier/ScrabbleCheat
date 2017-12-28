# ScrabbleCheat

## Revenge of the Verbally Challenged

ScrabbleCheat is code I wrote to beat my at-the-time girlfriend in Scrabble (she
is amazing at Scrabble). Figuring it would be only as hard as other word
algorithms like spell check or word creation from a list of letters, I developed
it in Erlang, a new language I hadn't used much. This started a love affair with
Erlang (which was largely accidental).

**ALMOST ALL OF THIS WAS WRITTEN IN 2010**. I was a very different programmer
then, and it was a pretty different industry.

## Components

Each of these has their own Makefiles and READMEs. You'll need all three to get
things going.

### Server

The main brain is written in Erlang. It's in `code/server`. That said, it
requires the dictionaries to be loaded as a binary, which is created with the
next component.

### Bingad

Which, being almost a component of the server, lives in
`code/server/lib/bingad`.

### Ncurses client

I've halfhearted started a number of clients, the only one I really finished was
an Ncurses one that's wrapped by Ruby. It lives in `clients/curses`.

## Dependencies

While we used packaged tools for code dependencies when necessary, you'll need:

* Erlang (try kerl to manage installations) and Rebar3
* Ruby (try rvm to manage installations) and Bundler
* Clang

I'll write a Dockerfile for this sometime, I'm sure of it.

----

For now this is just loose code that will hopefully piece together into actual 
usable software; in the meantime, I'm putting it here as well as my local 
machine.  Enjoy, and email about any questions, concerns!

Pablo Meier
12/27/2017
pablo.a.meier@gmail.com  
https://morepablo.com
