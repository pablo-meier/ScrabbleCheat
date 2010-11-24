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

    include Ncurses
    include Ncurses::Menu
    include Ncurses::Form

    TITLE_LINE = "Welcome to ScrabbleCheat!"
    AUTHOR_LINE =  "by Paul Meier - www.morepaul.com"
    COMMEMORATE_LINE = "For Sam"


    def initialize
        Ncurses.initscr
        Ncurses.start_color
        Ncurses.cbreak                  # provide unbuffered input
        Ncurses.noecho                  # don't echo input
        Ncurses.nonl                    # Turn of line-ending processing
        Ncurses.stdscr.intrflush(false) # turn off flush-on-interrupt
        Ncurses.stdscr.keypad(true)     # turn on keypad mode

        Ncurses.init_pair(1, COLOR_RED, COLOR_BLACK);
    end

   ########################################################################
   #  WELCOME SCREEN 

   def draw_welcome
        Ncurses.stdscr.clear
        draw_preamble

        menuwin = self.build_welcome_menu
        menuwin[:menu].post_menu
        menuwin[:window].wrefresh

        char = nil
        while (char = Ncurses.getch) do
            case char
                when Ncurses::KEY_DOWN
                    Ncurses::Menu.menu_driver(menuwin[:menu], Ncurses::Menu::REQ_DOWN_ITEM)
                when Ncurses::KEY_UP
                    Ncurses::Menu.menu_driver(menuwin[:menu], Ncurses::Menu::REQ_UP_ITEM)
                when Ncurses::KEY_RIGHT
                    break
                else  :do_nothing
            end
            menuwin[:window].wrefresh
        end
        
        item = Ncurses::Menu::current_item(menuwin[:menu])
        case Ncurses::Menu::item_description(item)
            when "new_game"
                Ncurses.stdscr.move(4, 4)
                Ncurses.stdscr.addstr("New game")
                Ncurses.stdscr.refresh()
                sleep(1)
                Ncurses::Menu.free_menu(menuwin[:menu])
                Ncurses.delwin(menuwin[:window])
                self.build_name_entry_form
            when "quit"
                Ncurses.stdscr.move(4, 4)
                Ncurses.stdscr.addstr("Quit")
                Ncurses.stdscr.refresh()
                sleep(1)
                {:state => :quit, :data => "quit"}
        end
    end

    def build_welcome_menu
        numcols = 40
        numrows = 10 

        new_game_item = Ncurses::Menu.new_item("Start a New Game", "new_game")
        quit_item = Ncurses::Menu.new_item("Quit", "quit")
        items = [new_game_item, quit_item]
        welcome_menu = Ncurses::Menu.new_menu(items)
        Ncurses::Menu::menu_opts_off(welcome_menu, Ncurses::Menu::O_SHOWDESC)

        xoffset = (Ncurses.COLS / 2) - (numcols / 2)
        yoffset = (Ncurses.LINES / 2) - (numrows / 2)
        welcome_menu_window = Ncurses.newwin(numrows, numcols, yoffset, xoffset)

        Ncurses.keypad(welcome_menu_window, true)
 
        Ncurses::Menu::set_menu_win(welcome_menu, welcome_menu_window)
        Ncurses::Menu::set_menu_sub(welcome_menu, Ncurses.derwin(welcome_menu_window, items.length * 3, numcols - 4, 3, 4))

        Ncurses::Menu::set_menu_mark(welcome_menu, " * ")
        Ncurses.box(welcome_menu_window, 0, 0)
        print_in_middle(welcome_menu_window, 1, 0, 40, "What would you like to do?", Ncurses.COLOR_PAIR(1))
        Ncurses.mvwaddch(welcome_menu_window, 2, 0, Ncurses::ACS_LTEE)
        Ncurses.mvwhline(welcome_menu_window, 2, 1, Ncurses::ACS_HLINE, 38)
        Ncurses.mvwaddch(welcome_menu_window, 2, 39, Ncurses::ACS_RTEE)
        {:window => welcome_menu_window, :menu => welcome_menu}
    end

    ########################################################################
    #  NAME ENTRY
    def build_name_entry_form
        fields = []
        1.upto 4 do |i|
            field = FIELD.new(1, 10, i * 1, 1, 0, 0)
            field.set_field_back(A_UNDERLINE)
            field.set_field_type(TYPE_ALPHA, 0)
            fields.push(field)
        end
        name_form = FORM.new(fields)
        name_form.user_object = "Name entry"

        rows = []
        cols = []
        name_form.scale_form(rows, cols)

        name_form_win = WINDOW.new(rows[0] + 3, cols[0] + 14, 1, 1)
        name_form_win.keypad(true)

        name_form.set_form_win(name_form_win)
        name_form.set_form_sub(name_form_win.derwin(rows[0], cols[0], 2, 12))
        
        name_form_win.box(0, 0)
        print_in_middle(name_form_win, 1, 0, cols[0] + 14, "Please enter up to 4 players", Ncurses.COLOR_PAIR(1))

        name_form.post_form
        name_form_win.mvaddstr(3, 2, "Player 1:")
        name_form_win.mvaddstr(4, 2, "Player 2:")
        name_form_win.mvaddstr(5, 2, "Player 3:")
        name_form_win.mvaddstr(6, 2, "Player 4:")

        name_form_win.wrefresh
        Ncurses.stdscr.refresh

        char = nil
        while (char = name_form_win.getch) 
            case char
                when KEY_DOWN
                    # Go to next field */
                    name_form.form_driver(REQ_VALIDATION);
                    name_form.form_driver(REQ_NEXT_FIELD);
                    # Go to the end of the present buffer
                    # Leaves nicely at the last character
                    name_form.form_driver(REQ_END_LINE);
                
                when KEY_UP
                    # Go to previous field
                    name_form.form_driver(REQ_VALIDATION);
                    name_form.form_driver(REQ_PREV_FIELD);
                    name_form.form_driver(REQ_END_LINE);
                
                when KEY_LEFT
                    # Go to previous field
                    name_form.form_driver(REQ_PREV_CHAR);
                
                when KEY_RIGHT
                    # Go to previous field
                    name_form.form_driver(REQ_NEXT_CHAR);
                
                when KEY_BACKSPACE
                when KEY_DC
                    name_form.form_driver(REQ_DEL_PREV);
                when KEY_SLEFT
                    break
                else
                    # If this is a normal character, it gets Printed    
                    name_form.form_driver(char);
            end
        end
        {:state => :new_game, :data => ["Paul", "Sam"]}
    end

    ########################################################################
    #  UTILITIES
    def print_in_middle(win, starty, startx, width, string, color)
         if(win == nil)
             win = stdscr;
         end
         x = Array.new
         y = Array.new
         Ncurses.getyx(win, y, x);
         if(startx != 0)
             x[0] = startx;
         end
         if(starty != 0)
             y[0] = starty;
         end
         if(width == 0)
             width = 80;
         end
         length = string.length;
         temp = (width - length)/ 2;
         x[0] = startx + temp.floor;
         win.attron(color);
         win.mvprintw(y[0], x[0], "%s", string);
         win.attroff(color);
         Ncurses.refresh();
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

end


at_exit {
    Ncurses.echo
    Ncurses.nocbreak
    Ncurses.nl
    Ncurses.endwin    
}
