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
        Ncurses.init_pair(2, COLOR_YELLOW, COLOR_BLACK);
        Ncurses.init_pair(3, COLOR_BLUE, COLOR_BLACK);
        Ncurses.init_pair(4, COLOR_CYAN, COLOR_BLACK);
        Ncurses.init_pair(5, COLOR_MAGENTA, COLOR_BLACK);
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
                Ncurses::Menu.free_menu(menuwin[:menu])
                Ncurses.delwin(menuwin[:window])
                Ncurses.stdscr.clear
                self.build_name_entry_form
            when "quit"
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
        self.draw_preamble
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

        width = cols[0] + 50
        height = rows[0] + 9 
        starty = Ncurses.LINES / 2 - (height / 2)
        startx = Ncurses.COLS / 2 - (width / 2)

        name_form_win = WINDOW.new(height, width, starty, startx)
        name_form_win.keypad(true)

        name_form.set_form_win(name_form_win)
        name_form.set_form_sub(name_form_win.derwin(rows[0], cols[0], 4, 30))
        
        name_form_win.box(0, 0)
        str = "Please enter the names of up to 4 players"
        print_in_middle(name_form_win, 2, 10, str.length, str, Ncurses.COLOR_PAIR(1))

        name_form.post_form
        name_form_win.mvaddstr(5, 20, "Player 1:")
        name_form_win.mvaddstr(6, 20, "Player 2:")
        name_form_win.mvaddstr(7, 20, "Player 3:")
        name_form_win.mvaddstr(8, 20, "Player 4:")

        str = "Press ENTER when done"
        print_in_middle(name_form_win, 11, 20, str.length, str, Ncurses.COLOR_PAIR(1))

        name_form_win.wrefresh
        Ncurses.stdscr.refresh

        char = nil
        while (char = name_form_win.getch) 
            case char
                when KEY_DOWN
                    # Go to next field 
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
                    # Go to next field
                    name_form.form_driver(REQ_NEXT_CHAR);
                
                when KEY_BACKSPACE
                when KEY_DC
                    name_form.form_driver(REQ_DEL_PREV);
                when KEY_ENTER
                when ?q
                    break
                else
                    # If this is a normal character, it gets printed
                    name_form.form_driver(char);
            end
        end
        # Process the form data.  Begins with a dirty hack to collect all names,
        # since field_buffer wasn't counting a field that hasn't been exited from.

        name_form.form_driver(REQ_NEXT_FIELD);
        name_form.form_driver(REQ_PREV_FIELD);

        names = []
        names = fields.map { |x| x.field_buffer(0) }

        names = names.map { |x| x.strip }.reject { |x| x.empty? }
        #names << "Names has #{names.length} entries"
        self.debug_println("Exited form with names #{names}")

        name_form.unpost_form
        name_form.free_form
        fields.each { |f| f.free_field }

        {:state => :new_game, :data => names}
    end


    ########################################################################
    #  ACTION CHOOSE
    def draw_action_choose(gamestate)
        board = gamestate[:board]
        scores = gamestate[:scores]
        turn = gamestate[:turn]
        history = gamestate[:history]

        Ncurses.stdscr.clear
        self.draw_preamble
        self.paint_scores(scores, turn)
        self.paint_board(board)
        Ncurses.refresh
        sleep(15)
        self.paint_history(history)

        option = self.present_action_request
        case option
            when :move 
                move = self.get_a_move
                {:state => :play_move, :data => move}
            when :ai
                rack = self.get_a_rack
                {:state => :get_moves, :data => rack}
        end
    end


    def paint_board(board)
        # Set up window
        width = 35  
        height = 20
        board_win = WINDOW.new(height, width, 4, (Ncurses.COLS / 2) - (2 + width))
        print_in_middle(board_win, 1, 13, 10, "-- Board --", Ncurses.COLOR_PAIR(1))
        board_win.box(0,0)

        # Print top edge
        board_win.attron(Ncurses.COLOR_PAIR(2))
        col = 3
        while col < 32
            board_win.mvaddstr(2, col, "_ ")
            col += 2
        end

        # Print the left edge
        row = 3
        while row < 18
            board_win.mvaddstr(row, 2, "|")
            row += 1
        end
        board_win.attroff(Ncurses.COLOR_PAIR(2))


        # Print each tile
        tiles = board.tiles
        1.upto 15 do |row|
            1.upto 15 do |col|
                this_tile = tiles[row][col]
                y = this_tile[:row] + 2
                x = (2 * this_tile[:col]) + 1

                case this_tile[:letter]
                    when :none
                        case this_tile[:bonus]
                            when :none # Yellow
                                board_win.attron(Ncurses.COLOR_PAIR(2))
                                board_win.mvaddstr(y, x, "_")
                                board_win.attroff(Ncurses.COLOR_PAIR(2))
                            when :triple_word_score # Red
                                board_win.attron(Ncurses.COLOR_PAIR(1))
                                board_win.mvaddstr(y, x, "*")
                                board_win.attroff(Ncurses.COLOR_PAIR(1))
                            when :double_word_score # Magenta
                                board_win.attron(Ncurses.COLOR_PAIR(5))
                                board_win.mvaddstr(y, x, "*")
                                board_win.attroff(Ncurses.COLOR_PAIR(5))
                            when :triple_letter_score # Blue
                                board_win.attron(Ncurses.COLOR_PAIR(3))
                                board_win.mvaddstr(y, x, "*")
                                board_win.attroff(Ncurses.COLOR_PAIR(3))
                            when :double_letter_score # Cyan
                                board_win.attron(Ncurses.COLOR_PAIR(4))
                                board_win.mvaddstr(y, x, "*")
                                board_win.attroff(Ncurses.COLOR_PAIR(4))
                        end
                    else
                        char = this_tile[:letter]
                        board_win.mvaddstr(y, x, char)
                end

                board_win.attron(Ncurses.COLOR_PAIR(2))
                board_win.mvaddstr(y, x + 1, "|")
                board_win.attroff(Ncurses.COLOR_PAIR(2))
            end
        end
        board_win.wrefresh
    end


    def paint_scores(scores, turn)
        width = 20
        height = scores.length + 3
        score_win = WINDOW.new(height, width, 4, (Ncurses.COLS / 2) + 2)
        print_in_middle(score_win, 1, 5, 10, "-- Scores --", Ncurses.COLOR_PAIR(1))
        score_win.box(0,0)

        y = 2
        x = 3

        scores.each do |score_listing|
            name = score_listing[0]
            score = score_listing[1].to_s
            my_turn = turn.eql? name

            score_win.mvaddstr(y, x, name)
            score_win.mvaddstr(y, width - score.length - 2, score)

            if my_turn
                score_win.attron(Ncurses.COLOR_PAIR(1))
                score_win.mvaddstr(y, 1, "*")
                score_win.attroff(Ncurses.COLOR_PAIR(1))
            end
            y += 1
        end
        score_win.wrefresh
    end


    def paint_history(history)
        :ok
    end


    def present_option_request
        :ok
    end

    def get_a_move
        :ok
    end

    def get_a_rack
        :ok
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


    def debug_println(str)
        Ncurses.stdscr.move(4, 4)
        Ncurses.stdscr.addstr(str)
        Ncurses.stdscr.refresh()
        sleep(1)
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
