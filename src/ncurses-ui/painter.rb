# Copyright (c) 2010 Paul Meier
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.


require 'board.rb'
require 'ncurses.rb'


class Painter

    TITLE_LINE = "Welcome to ScrabbleCheat!"
    AUTHOR_LINE =  "by Paul Meier - www.morepaul.com"
    COMMEMORATE_LINE = "For Sam"


    def initialize
        Ncurses.initscr
        Ncurses.cbreak                  # provide unbuffered input
        Ncurses.noecho                  # don't echo input
        Ncurses.nonl                    # Turn of line-ending processing
        Ncurses.stdscr.intrflush(false) # turn off flush-on-interrupt
        Ncurses.stdscr.keypad(true)     # turn on keypad mode
    end

    def draw_welcome
        Ncurses.stdscr.clear
        draw_preamble 
        present_welcome_options
        Ncurses.stdscr.refresh

        Ncurses.stdscr.getch
        {:state => :quit, :data => nil}
    end


    def present_welcome_options
        query_line = "Would you like to..."
        Ncurses.stdscr.move(Ncurses.LINES / 4, (Ncurses.COLS / 4) - (query_line.length / 2))
        Ncurses.stdscr.addstr(query_line)

        new_game_line = "Start a New Game"
        quit_line = "Quit"

        midpoint_rows = Ncurses.LINES / 2
        midpoint_cols = Ncurses.COLS / 2
        Ncurses.stdscr.move(midpoint_rows - 2, midpoint_cols - (new_game_line.length / 2))
        Ncurses.stdscr.addstr(new_game_line)
        
        Ncurses.stdscr.move(midpoint_rows + 2, midpoint_cols - (quit_line.length / 2))
        Ncurses.stdscr.addstr(quit_line)
     end


    def draw_preamble
        # Draw the title line
        Ncurses.stdscr.move(1, (Ncurses.COLS / 2) - (TITLE_LINE.length / 2))
        Ncurses.stdscr.addstr(TITLE_LINE)

        # Vanity Line
        Ncurses.stdscr.move(Ncurses.LINES - 2, 2)
        Ncurses.stdscr.addstr(AUTHOR_LINE)

        # Disgusting lovey-dovey line
        Ncurses.stdscr.move(Ncurses.LINES - 2, Ncurses.COLS - (COMMEMORATE_LINE.length + 2))
        Ncurses.stdscr.addstr(COMMEMORATE_LINE)
    end

    def close_up
        Ncurses.echo
        Ncurses.nocbreak
        Ncurses.nl
        Ncurses.endwin    
    end
 
 end






