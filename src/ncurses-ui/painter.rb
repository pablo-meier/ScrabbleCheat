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


    CAPITAL_LETTER_PATTERN = /[A-Z]/

    CHARACTER_TAB = 9  # Ascii values for pattern matching
    CHARACTER_CR = 13
    CHARACTER_LF = 10

    WELCOME_MENU_SPEC = {:title => "What would you like to do?",
                         :items => [{:name => "Start a New Game", :retval => "new_game"},
                                    {:name => "Quit", :retval => "quit"}],
                         :draw_at => :center}
                         

    ACTION_CHOICE_MENU_SPEC = {:title => "What would you like to do?",
                               :items => [{:name => "Create and play a move", :retval => "make_move"},
                                          {:name => "Get suggested ScrabbleCheat moves", :retval => "ai"}],
                               :draw_at => {:x => :center, :y => 25}}
 

    def initialize
        @debug = false

        Ncurses.initscr
        Ncurses.start_color
        Ncurses.cbreak                  # provide unbuffered input
        Ncurses.noecho                  # don't echo input
        Ncurses.nonl                    # Turn of line-ending processing
        Ncurses.stdscr.intrflush(false) # turn off flush-on-interrupt
        Ncurses.stdscr.keypad(true)     # turn on keypad mode
#        Ncurses.curs_set(1)             # Remove the actual cursor

        Ncurses.init_pair(1, COLOR_RED, COLOR_BLACK);
        Ncurses.init_pair(2, COLOR_YELLOW, COLOR_BLACK);
        Ncurses.init_pair(3, COLOR_BLUE, COLOR_BLACK);
        Ncurses.init_pair(4, COLOR_CYAN, COLOR_BLACK);
        Ncurses.init_pair(5, COLOR_MAGENTA, COLOR_BLACK);
        Ncurses.init_pair(6, COLOR_WHITE, COLOR_WHITE);

        @colors = {:red => Ncurses.COLOR_PAIR(1),
                   :yellow => Ncurses.COLOR_PAIR(2),  
                   :blue => Ncurses.COLOR_PAIR(3),  
                   :cyan => Ncurses.COLOR_PAIR(4),  
                   :magenta => Ncurses.COLOR_PAIR(5),  
                   :highlight => Ncurses.COLOR_PAIR(6)
                  }
    end


   ########################################################################
   #  WELCOME SCREEN 

   def draw_welcome
        Ncurses.stdscr.clear
        draw_preamble

        menuwin = self.make_menu(WELCOME_MENU_SPEC)

        char = nil
        while (char = Ncurses.getch) do
            case char
                when Ncurses::KEY_DOWN
                    Ncurses::Menu.menu_driver(menuwin[:menu], Ncurses::Menu::REQ_DOWN_ITEM)
                when Ncurses::KEY_UP
                    Ncurses::Menu.menu_driver(menuwin[:menu], Ncurses::Menu::REQ_UP_ITEM)
                when KEY_ENTER, ?1, CHARACTER_CR, CHARACTER_LF
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
        print_in_middle(name_form_win, 2, 10, str.length, str, @colors[:red])

        name_form.post_form
        name_form_win.mvaddstr(5, 20, "Player 1:")
        name_form_win.mvaddstr(6, 20, "Player 2:")
        name_form_win.mvaddstr(7, 20, "Player 3:")
        name_form_win.mvaddstr(8, 20, "Player 4:")

        str = "Press ENTER when done"
        print_in_middle(name_form_win, 11, 20, str.length, str, @colors[:red])

        name_form_win.wrefresh
        Ncurses.stdscr.refresh

        while (char = name_form_win.getch) 
            case char
                when KEY_DOWN, CHARACTER_TAB
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
                
                when KEY_BACKSPACE, KEY_DC
                    name_form.form_driver(REQ_DEL_PREV);
                when KEY_ENTER, ?1, CHARACTER_CR, CHARACTER_LF
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
        boardwin = self.paint_board(board)
        self.paint_history(history)

        option = self.present_action_request
        case option[:state]
            when :move 
                move = self.get_a_move(board, boardwin, option[:window])
                {:state => :play_move, :data => move}
            when :ai
                rack = self.get_a_rack(option[:window])
                {:state => :get_moves, :data => rack}
        end
    end


    def paint_board(board)
        # Set up window
        width = 35  
        height = 20
        board_win = WINDOW.new(height, width, 4, (Ncurses.COLS / 2) - (2 + width))
        paint_board_win(board, board_win)
    end


    def paint_board_win(board, board_win)
        print_in_middle(board_win, 1, 13, 10, "-- Board --", @colors[:red])
        board_win.box(0,0)

        # Print top edge
        board_win.attron(@colors[:yellow])
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
        board_win.attroff(@colors[:yellow])


        # Print each tile
        tiles = board.tiles
        1.upto 15 do |row|
            1.upto 15 do |col|
                draw_tile(tiles[row][col], board_win)
            end
        end
        board_win.wrefresh
        board_win
    end


    def draw_tile(this_tile, board_win)
        y = this_tile[:row] + 2
        x = (2 * this_tile[:col]) + 1

        case this_tile[:letter]
            when :none
                case this_tile[:bonus]
                    when :none 
                        board_win.attron(@colors[:yellow])
                        board_win.mvwaddstr(y, x, "_")
                        board_win.attroff(@colors[:yellow])
                    when :triple_word_score 
                        board_win.attron(@colors[:red])
                        board_win.mvwaddstr(y, x, "*")
                        board_win.attroff(@colors[:red])
                    when :double_word_score
                        board_win.attron(@colors[:magenta])
                        board_win.mvwaddstr(y, x, "*")
                        board_win.attroff(@colors[:magenta])
                    when :triple_letter_score 
                        board_win.attron(@colors[:blue])
                        board_win.mvwaddstr(y, x, "*")
                        board_win.attroff(@colors[:blue])
                    when :double_letter_score 
                        board_win.attron(@colors[:cyan])
                        board_win.mvwaddstr(y, x, "*")
                        board_win.attroff(@colors[:cyan])
                end
            else
                char = this_tile[:letter]
                board_win.mvwaddstr(y, x, char)
        end

        board_win.wattron(@colors[:yellow])
        board_win.mvwaddstr(y, x + 1, "|")
        board_win.wattroff(@colors[:yellow])
    end


    def paint_scores(scores, turn)
        width = 20
        height = scores.length + 3
        score_win = WINDOW.new(height, width, 4, (Ncurses.COLS / 2) + 2)
        print_in_middle(score_win, 1, 5, 10, "-- Scores --", @colors[:red])
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
                score_win.attron(@colors[:red])
                score_win.mvaddstr(y, 1, "*")
                score_win.attroff(@colors[:red])
            end
            y += 1
        end
        score_win.wrefresh
    end


    def paint_history(history)
        :ok
    end


    def present_action_request

        menuwin = self.make_menu(ACTION_CHOICE_MENU_SPEC)
        char = nil
        menuwin[:window].wrefresh
        while (char = Ncurses.getch) do
            case char
                when Ncurses::KEY_DOWN
                    Ncurses::Menu.menu_driver(menuwin[:menu], Ncurses::Menu::REQ_DOWN_ITEM)
                when Ncurses::KEY_UP
                    Ncurses::Menu.menu_driver(menuwin[:menu], Ncurses::Menu::REQ_UP_ITEM)
                when KEY_ENTER, ?1, CHARACTER_CR, CHARACTER_LF
                    break
                else  :do_nothing
            end
            menuwin[:window].wrefresh
        end
        
        item = Ncurses::Menu::current_item(menuwin[:menu])
        retval = Ncurses::Menu::item_description(item)
        menuwin[:items].each { |i| i.free_item }
        menuwin[:menu].unpost_menu
        menuwin[:menu].free_menu
        menuwin[:derwin].delwin
        case retval
            when "make_move"
                {:state => :move, :window => menuwin[:window]}
            when "ai"
                {:state => :ai, :window => menuwin[:window]}
        end
    end


    def get_a_move(board, boardwin, presentation_win)

        title_str = "How to Add a Move From Your Rack"
        presentation_win.attron(@colors[:red])
        presentation_win.mvaddstr(1, 23, title_str)
        presentation_win.attroff(@colors[:red])

        str1 = "Use the arrow keys to move the cursor, and the keyboard to add"
        str2 = "   characters to the board.  Use the SPACE BAR to delete a"
        str3 = "mistaken character.  Press ENTER when finished to add the move" 
        str4 = "                         to the game."

        row = 3
        col = 6

        presentation_win.mvaddstr(row, col, str1)
        presentation_win.mvaddstr(row + 1, col, str2)
        presentation_win.mvaddstr(row + 2, col, str3)
        presentation_win.mvaddstr(row + 3, col, str4)
        presentation_win.box(0,0)
        presentation_win.wrefresh

        tiles = board.tiles
        cursor = {:x => 1, :y => 1}
        move = []   # A move is just a list of tiles.

        boardwin.wmove(1 + 2, (2 * 1) + 1) #  REPLACE WITH GENERAL get_board_coordinates
        boardwin.wrefresh
        while (char = Ncurses.getch) do
            case char
                when KEY_DOWN
                    cursor[:y] += 1 if cursor[:y] < 15
                when KEY_UP
                    cursor[:y] -= 1 if cursor[:y] > 1
                when KEY_RIGHT
                    cursor[:x] += 1 if cursor[:x] < 15
                when KEY_LEFT
                    cursor[:x] -= 1 if cursor[:x] > 1
                when KEY_BACKSPACE, 32 # Space bar, don't know how to char literal a space in Ruby.
                    move = move.reject { |x| x[:row] == cursor[:y] && x[:col] == cursor[:x] } 
                when KEY_ENTER, ?1, CHARACTER_CR, CHARACTER_LF
                    break
                else
                    char = char.chr.to_s.upcase
                    tile = tiles[cursor[:y]][cursor[:x]]
                    if char.match(CAPITAL_LETTER_PATTERN) && tile[:letter] == :none
                        # Lame Ruby, having state and shit, making me make my own copies of things...
                        newtile = {:letter => char, :letter_type => :character,
                                   :bonus => tile[:bonus], :row => tile[:row], :col => tile[:col]}

                        move = move.reject { |x| x[:row] == newtile[:row] && x[:col] == newtile[:col] }
                        move << newtile

                        cursor[:x] += 1 if cursor[:x] < 15
                    end
            end
            paint_board_win(board, boardwin)
            move.each do |x|
                self.draw_tile(x, boardwin)
            end
            boardwin.wmove(cursor[:y] + 2, (2 * cursor[:x]) + 1)
            boardwin.wrefresh
        end
        move
    end



    def get_a_rack(presentation_win)
        presentation_win.wclear

        fields = []
        field = FIELD.new(1, 7, 1, 1, 0, 0)
        field.set_field_back(A_UNDERLINE)
        field.set_field_type(TYPE_ALPHA, 0)
        fields << field
        rack_form = FORM.new(fields)
        rack_form.user_object = "Rack Entry"

        rack_form.set_form_win(presentation_win)
        the_derwin = presentation_win.derwin(3, 33, 3, 33)
        rack_form.set_form_sub(the_derwin)
        
        presentation_win.box(0, 0)
        str = "Please enter the letters in your rack"

        presentation_win.attron(@colors[:red])
        presentation_win.mvaddstr(1, 19, str)
        presentation_win.attroff(@colors[:red])

        str = "Press ENTER when done"
        presentation_win.mvaddstr(6, 27, str)

        rack_form.post_form
        rack_form.set_current_field(field)

        presentation_win.box(0,0)
        presentation_win.wrefresh

        while (char = presentation_win.getch) 
            case char
                when KEY_DOWN
                    rack_form.form_driver(REQ_VALIDATION);
                    rack_form.form_driver(REQ_NEXT_FIELD);
                    rack_form.form_driver(REQ_END_LINE);
                
                when KEY_UP
                    rack_form.form_driver(REQ_VALIDATION);
                    rack_form.form_driver(REQ_PREV_FIELD);
                    rack_form.form_driver(REQ_END_LINE);
                
                when KEY_LEFT
                    rack_form.form_driver(REQ_PREV_CHAR);
                
                when KEY_RIGHT
                    rack_form.form_driver(REQ_NEXT_CHAR);
 
                when KEY_BACKSPACE, KEY_DC
                    rack_form.form_driver(REQ_DEL_PREV);
                when KEY_ENTER, ?1, CHARACTER_CR, CHARACTER_LF
                    break
                else
                    if char < 256 && char.chr.match(/[a-zA-Z]/)
                        rack_form.form_driver(char.chr.upcase[0])
                    end
            end
            presentation_win.wrefresh
        end

        rack_form.form_driver(REQ_NEXT_FIELD);
        rack_form.form_driver(REQ_PREV_FIELD);

        rack = field.field_buffer(0).strip

        rack_form.unpost_form
        rack_form.free_form
        field.free_field 
        the_derwin.delwin
        presentation_win.delwin
        rack
    end


    def draw_move_select_menu(hash)
        gamestate = hash[:gamestate]
        board = gamestate[:board]
        scores = gamestate[:scores]
        turn = gamestate[:turn]

        Ncurses.stdscr.clear
        self.draw_preamble
        self.paint_scores(scores, turn)
        boardwin = self.paint_board(board)

        moves = hash[:moves][0, 4]
        title_str = "Please select a move to play"
        move_menu_spec = {:title => title_str,
                          :draw_at => {:x => :center, :y => 25}}

        move_items = []
        0.upto(moves.length - 1) do |index|
            curr_move = moves[index]
            board_copy = board.deep_copy
            board_copy.place_move(curr_move)
            move_name = get_move_name(curr_move, board_copy)
            access_index = index.to_s
            move_items << {:name => move_name, :retval => access_index}
        end
        move_menu_spec[:items] = move_items
 
        menuwin = self.make_menu(move_menu_spec)
        menu_item_index = 0
        menuwin[:menu].set_current_item(menuwin[:items][menu_item_index])
        index = menuwin[:menu].current_item.item_description.to_i
        curr_move = moves[index][:move]

        @debug = true
 
        curr_move.each { |tile| draw_tile(tile, boardwin) }
        Ncurses.redrawwin(boardwin)
        Ncurses.redrawwin(menuwin[:window])
        Ncurses.wrefresh(boardwin)
        Ncurses.wrefresh(menuwin[:window])
        while (char = Ncurses.getch) do
            Ncurses.redrawwin(boardwin)
            Ncurses.redrawwin(menuwin[:window])
            Ncurses.wrefresh(boardwin)
            Ncurses.wrefresh(menuwin[:window])
            case char
                when Ncurses::KEY_DOWN
                    if menu_item_index < (menuwin[:items].length - 1) 
                        Ncurses::Menu.menu_driver(menuwin[:menu], Ncurses::Menu::REQ_DOWN_ITEM)
                        menu_item_index += 1 
                    end
               when Ncurses::KEY_UP
                    if menu_item_index > 0 
                        Ncurses::Menu.menu_driver(menuwin[:menu], Ncurses::Menu::REQ_UP_ITEM)
                        menu_item_index -= 1 
                    end
                when KEY_ENTER, ?1, CHARACTER_CR, CHARACTER_LF
                    break
                else  :do_nothing
            end
            index = Ncurses::Menu::current_item(menuwin[:menu]).item_description.to_i
            menuwin[:menu].set_current_item(menuwin[:items][menu_item_index])
            curr_move = moves[index][:move]
            boardwin.wclear
            self.paint_board_win(board, boardwin)
            curr_move.each { |tile| draw_tile(tile, boardwin) }
            Ncurses.redrawwin(boardwin)
            Ncurses.redrawwin(menuwin[:window])
            Ncurses.wrefresh(boardwin)
            Ncurses.wrefresh(menuwin[:window])
         end

        @debug = false
        
        index = Ncurses::Menu::current_item(menuwin[:menu]).item_description.to_i
        menuwin[:items].each { |i| i.free_item }
        menuwin[:menu].unpost_menu
        menuwin[:menu].free_menu
        menuwin[:derwin].delwin
        menuwin[:window].delwin
        {:state => :play_move, :data => moves[index][:move] }
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


    # Given a move (a hash), return a name for it's label.  This should
    # be the "primary" word it makes, and the score.
    def get_move_name(movehash, board)
        move = movehash[:move]
        score = movehash[:score]
        orientation = nil

        # Find the orientation: 
        #   If move is two or more tiles, check two tiles of the move and see what plane they are on.
        #   Else, check adjacents.
        if move.length > 1 
            component1, component2 = move[0], move[1]
            if component1[:row] == component2[:row]
                orientation = :horizontal
            else
                orientation = :vertical
            end
        elsif 
            row = move[0][:row] 
            col = move[0][:col]
            if board[row][col - 1][:letter] != :none || board[row][col + 1][:letter] != :none
                orientation = :horizontal
            else
                orientation = :vertical
            end
        end

        # Zoom as far back, trace the word
        row, col = move[0][:row], move[0][:col]
        currtile = board[row][col]
        if orientation == :horizontal
            while currtile[:letter] != :none && col != 0 do
                 col -= 1
                 currtile = board[row][col]
            end
            namestring = ""
            col += 1 if currtile[:letter] == :none
            currtile = board[row][col]

            while currtile[:letter] != :none && col != 16 do
                namestring += currtile[:letter]
                col += 1
                currtile = board[row][col]
            end
            namestring + " -- " + score.to_s
        else
            while currtile[:letter] != :none && row != 0 do
                 row -= 1
                 currtile = board[row][col]
            end
            namestring = ""
            row += 1 if currtile[:letter] == :none
            currtile = board[row][col]

            while currtile[:letter] != :none && row != 16 do
                namestring += currtile[:letter]
                row += 1
                currtile = board[row][col]
            end
            namestring + " -- " + score.to_s
        end
    end


    def debug_println(str)
        if @debug
            Ncurses.stdscr.move(4, 4)
            Ncurses.stdscr.addstr(str)
            Ncurses.stdscr.refresh()
            sleep(4)
        end
    end


    def make_menu(menuspec)
        title = menuspec[:title]
        itemlist = menuspec[:items]
    
        longest = 1
        itemlist.each do |itemhash|
            longest = itemhash[:name].length if itemhash[:name].length > longest
        end
        longest = title.length if title.length > longest
    
        items = itemlist.map do |x|
            name = x[:name]
            if name.length < longest
                diff = longest - name.length
                padding = ""
                1.upto(diff / 2) { padding += " " }
                name = padding + name + padding
            end
            Ncurses::Menu.new_item(name, x[:retval])
        end
        menu = Ncurses::Menu.new_menu(items)
        menu.menu_opts_off(Ncurses::Menu::O_SHOWDESC)
    
        height = menuspec[:height]
        width  = menuspec[:width]
    
        if height.nil?
            height = itemlist.length + 6
        end
    
        if width.nil?
            width = 2 * (longest + 4) # Padding of two for each side
        end
    
        x = nil
        y = nil
        location = menuspec[:draw_at]
        if location.is_a? Hash
            x = location[:x] if location[:x].is_a?(Fixnum)
            x = (Ncurses.COLS / 2) - (width / 2) if location[:x].is_a?(Symbol) && location[:x] == :center

            y = location[:y] if location[:y].is_a?(Fixnum)
            y = (Ncurses.LINES / 2) - (height / 2) if location[:y].is_a?(Symbol) && location[:y] == :center
        elsif location.is_a? Symbol
            if location == :center
                x = (Ncurses.COLS / 2) - (width / 2)
                y = (Ncurses.LINES / 2) - (height / 2)
            end
        end

        menu_win = Ncurses.newwin(height, width, y, x)
        Ncurses.keypad(menu_win, true)
    
        menu.set_menu_win(menu_win)
        derwin_startcol = (width / 2 - longest / 2) - 3
        the_derwin = menu_win.derwin(itemlist.length, width - derwin_startcol, 4, derwin_startcol)
        menu.set_menu_sub(the_derwin)
    
        menu.set_menu_mark(" * ")
    
        menu_win.box(0,0)
    
        color = @colors[:red]
        color = menuspec[:color] if not menuspec[:color].nil?
        menu_win.attron(color);
        col_loc = (width / 2) - (menuspec[:title].length / 2)
        menu_win.mvprintw(1, col_loc, "%s", menuspec[:title])
        menu_win.attroff(color);
    
        menu_win.mvaddch(2, 0, Ncurses::ACS_LTEE)
        menu_win.mvwhline(2, 1, Ncurses::ACS_HLINE, width - 2)
        menu_win.mvaddch(2, width - 1, Ncurses::ACS_RTEE)
    
        Ncurses.refresh
        menu.post_menu
        menu_win.wrefresh
        {:menu => menu, :window => menu_win, :items => items, :derwin => the_derwin}
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
