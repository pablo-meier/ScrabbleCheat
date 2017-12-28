# Curses client!

<p style="font-weight: bold; text-align: center">The one client I actually damn
built.</p>

## Running instructions

Ensure you have Ruby installed. At the time of this writing, I'm using Ruby
2.4.2, but maybe you'll need [RVM][1] to get a version if you don't have it.
Also ensure you've got [Bundler][2] since we're using a for-real outside-world
dependency.

Then

`make`

## What's happening here

I rushed to make this to have something, _anything_ that let my Erlang server
speak to the outside world. I didn't have the chops to make anything on a web
client (it was 2011, it was harder to do...) or felt like writing something like
Qt (for a while I considered ActionScript) but remembered once I'd used ncurses
for a project at school. I hated having to deal with resource management in C,
and wondered if Ruby had a wrapper for it.

Lo and behold, it does! I checked in the generated Thrift code to call the
server.

**It's buggy as all hell** but she'll do in a pinch. We use [ncurses-ruby][3] to
draw the state onto the screen.

   [1]: https://rvm.io/
   [2]: https://bundler.io/
   [3]: https://github.com/eclubb/ncurses-ruby
