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

require 'conversationalist.rb'
require 'painter.rb'

require 'socket'
include Socket::Constants
PORT = 6655                     # Hard coding for now, will generalize later.


# Handles state transitions for the client.  Uses Conversationalist for server
# communications, and painter to draw the results.
#
# Use cases are:
#   - A welcome menu (new_game, quit).
#   - An action chooser (play_move, get_scrabblecheat_moves)
#       - Present board, scores, and choice between two options.  Optional
#         history view.
#   - First "action sequence!":  play_move needs a move.  Allow navigation/input 
#     into board until user is satisfied, then "Submit"
#       - On successful submit, display action chooser with resulting board, 
#         scores.
#   - Second "action sequence!": get_scrabblecheat_moves needs a rack.  Allow 
#     user to input a Rack, followed by Submit.
#       - On submit, present vertical scroller with word, score.  Board have
#         move overlayed.  User submits a move, and it is played.  Back to 
#         action chooser with new gamestate.
#

class Client

    def initialize
        socket = TCPSocket.new("localhost", PORT)
        @connection = Conversationalist.new(socket)
        @painter = Painter.new

        @gamestates = []
        @gamestate_index = -1
        @curr_state = {:state => :welcome, :data => nil}

        self.play_game
    end

    def play_game
        loop do
            response = self.show(@curr_state)
            case response[:state]
                when :new_game
                    names = response[:data]
                    gamestate = @connection.create_new_game(names)
                    self.add_gamestate(gamestate)
                    @curr_state = {:state => :action_choose, :data => gamestate}
                when :play_move
                    move = response[:data]
                    new_state = @connection.play_move(@gamestates[@gamestate_index], move)
                    self.add_gamestate(new_state)
                    @curr_state = {:state => :action_choose, :data => new_state}
                when :get_moves
                    rack = response[:data]
                    this_gamestate = @gamestates[@gamestate_index]
                    moves = @connection.get_scrabblecheat_moves(this_gamestate, rack)
                    @curr_state = {:state => :move_choose, :data => {:gamestate => this_gamestate, :moves => moves}}
                when :quit
                    @connection.quit
                    Process.exit
            end
        end
    end


    def add_gamestate(gamestate)
        @gamestates << gamestate
        @gamestate_index += 1
    end



    def show(client_state)
        case client_state[:state]
            # Draws the welcome screen, prompts user for their names.  Returns either
            # {:state => :new_game, :data => [String]}, or 
            # {:state => :quit, :data => nil}
            when :welcome
                @painter.draw_welcome
            when :action_choose
                handle_action_choose(client_state[:data])
            when :move_choose
                handle_move_choose(client_state[:data])
        end
    end

    # Draws the current gamestate (board, scores, turn) and prompts the user
    # to either play a move, or ask for help.  Returns either
    # {:state => :play_move, :data => Move}, or
    # {:state => :get_moves, :data => Rack}
    def handle_action_choose(gamestate)

    end

    # Presents a list of possible moves to the user, allows them to select
    # one to play.  Returns 
    # {:state => :play_move, :data => Move}
    def handle_move_choose(moves)
    end
end


Client.new
