# Like everything in Ruby in this project, a quick hack to get the wheels moving.
# This module handles reading/writing our simple file format for games. We'll just
# do a format like the following:

$:.push('gen-rb')

require_relative 'conversationalist'

class FileParser

    WWF_PATTERN = /words_with_friends/
    SCRABBLE_PATTERN = /scrabble/
    LEXULOUS_PATTERN = /lexulous/

    TWL_PATTERN = /twl06/
    SOWPODS_PATTERN = /sowpods/
    ZYNGA_PATTERN = /zynga/

    BOARD_MATCH = /^Board:/
    DICT_MATCH = /^Dict:/
    GAME_MATCH = /^Game:/
    TURN_MATCH = /^Turn:/
    PLAYERS_MATCH = /^Players:/


    def initialize(connection)
        @connection = connection
    end

    def parse(filepath)
        
        file = File.open(filepath)
        return_gamestate = {:gamename => :words_with_friends, :dict => :zynga }

        while (line = file.gets) do
            if line.match(GAME_MATCH)
                gamename = :scrabble
                if line.match(WWF_PATTERN) then gamename = :words_with_friends end
                if line.match(LEXULOUS_PATTERN) then gamename = :lexulous end
                return_gamestate[:gamename] = gamename

            elsif line.match(DICT_MATCH)
                dict = :twl06
                if line.match(ZYNGA_PATTERN) then dict = :zynga end
                if line.match(SOWPODS_PATTERN) then dict = :sowpods end
                return_gamestate[:dict] = dict

            elsif line.match(PLAYERS_MATCH)
                return_gamestate[:scores] = parse_players(line)

            elsif line.match(TURN_MATCH)
                return_gamestate[:turn] = line.match(/^Turn:(.*)/)[1].strip

            elsif line.match(BOARD_MATCH)
                return_gamestate[:board] = parse_board(file, return_gamestate[:gamename])
            end
        end

        return_gamestate[:history] = []
        return_gamestate
    end


    def parse_players(line)
        namelist = line.match(/^Players:(.*)/)[1].strip
        pairs = namelist.split(/,/)
        pairs = pairs.map do |pair|
                    arr = pair.split(/\//)
                    arr = arr.map { |x| x.strip }
                    arr[1] = arr[1].to_i
                    arr
                end
        pairs
    end


    def parse_board(file, gamename)
        gameinfo = @connection.game_info(gamename)
        board = gameinfo[:board_template]

        1.upto(15) do |row|
            tilechars = file.gets.split(" ")
            1.upto(15) do |col|
                this_char = tilechars[col - 1]
                if is_upcase(this_char) || is_downcase(this_char)
                    boardtile = board[row][col]
                    boardtile[:letter] = this_char
                    if is_upcase(this_char)
                        boardtile[:letter_type] = :character
                    elsif is_downcase(this_char)
                        boardtile[:letter_type] = :wildcard
                    end
                    board[row][col] = boardtile
                else
                    # probably '_', do nothing
                end
            end
        end
        board
    end


    def is_upcase(str)
        ascii = str[0]
        ascii >= 65 && ascii <= 90
    end

    def is_downcase(str)
        ascii = str[0]
        ascii >= 97 && ascii <= 122
    end
end


## Testing!
#
# filename = ARGV[0]
# fp = FileParser.new(Conversationalist.new)
# gamestate = fp.parse(filename)
# 
# 
# puts "Game is " + gamestate[:gamename].to_s
# puts "Dict is " + gamestate[:dict].to_s
# puts "It is " + gamestate[:turn] + "'s turn."
# puts "The Scores are:"
# gamestate[:scores].each do |arr|
#     puts "  " + arr[0].to_s + ", with " + arr[1].to_s + " points."
# end
# 
# puts "The board:"
# 
# board = gamestate[:board].to_list
# i = 0
# 
# board.each do |tile|
#     if tile[:letter] == :none
#         print " _ "
#     else
#         print " " + tile[:letter].to_s + " "
#     end
#     
#     i += 1
#     if i % 15 == 0
#         print "\n"
#         i = 0
#     end
# end
