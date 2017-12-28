#
# Autogenerated by Thrift Compiler (0.10.0)
#
# DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
#

require 'thrift'
require 'scrabble_cheat_types'

module ScrabbleCheat
  class Client
    include ::Thrift::Client

    def new_game(players, game_name, dict)
      send_new_game(players, game_name, dict)
      return recv_new_game()
    end

    def send_new_game(players, game_name, dict)
      send_message('new_game', New_game_args, :players => players, :game_name => game_name, :dict => dict)
    end

    def recv_new_game()
      result = receive_message(New_game_result)
      return result.success unless result.success.nil?
      raise result.what unless result.what.nil?
      raise ::Thrift::ApplicationException.new(::Thrift::ApplicationException::MISSING_RESULT, 'new_game failed: unknown result')
    end

    def game_info(game_name)
      send_game_info(game_name)
      return recv_game_info()
    end

    def send_game_info(game_name)
      send_message('game_info', Game_info_args, :game_name => game_name)
    end

    def recv_game_info()
      result = receive_message(Game_info_result)
      return result.success unless result.success.nil?
      raise result.msg unless result.msg.nil?
      raise ::Thrift::ApplicationException.new(::Thrift::ApplicationException::MISSING_RESULT, 'game_info failed: unknown result')
    end

    def play_move(tiles, gamestate)
      send_play_move(tiles, gamestate)
      return recv_play_move()
    end

    def send_play_move(tiles, gamestate)
      send_message('play_move', Play_move_args, :tiles => tiles, :gamestate => gamestate)
    end

    def recv_play_move()
      result = receive_message(Play_move_result)
      return result.success unless result.success.nil?
      raise result.msg unless result.msg.nil?
      raise ::Thrift::ApplicationException.new(::Thrift::ApplicationException::MISSING_RESULT, 'play_move failed: unknown result')
    end

    def pass_turn(gamestate)
      send_pass_turn(gamestate)
      return recv_pass_turn()
    end

    def send_pass_turn(gamestate)
      send_message('pass_turn', Pass_turn_args, :gamestate => gamestate)
    end

    def recv_pass_turn()
      result = receive_message(Pass_turn_result)
      return result.success unless result.success.nil?
      raise result.msg unless result.msg.nil?
      raise ::Thrift::ApplicationException.new(::Thrift::ApplicationException::MISSING_RESULT, 'pass_turn failed: unknown result')
    end

    def get_scrabblecheat_suggestions(rack, board, game_name, dict)
      send_get_scrabblecheat_suggestions(rack, board, game_name, dict)
      return recv_get_scrabblecheat_suggestions()
    end

    def send_get_scrabblecheat_suggestions(rack, board, game_name, dict)
      send_message('get_scrabblecheat_suggestions', Get_scrabblecheat_suggestions_args, :rack => rack, :board => board, :game_name => game_name, :dict => dict)
    end

    def recv_get_scrabblecheat_suggestions()
      result = receive_message(Get_scrabblecheat_suggestions_result)
      return result.success unless result.success.nil?
      raise result.msg unless result.msg.nil?
      raise ::Thrift::ApplicationException.new(::Thrift::ApplicationException::MISSING_RESULT, 'get_scrabblecheat_suggestions failed: unknown result')
    end

    def quit()
      send_quit()
    end

    def send_quit()
      send_oneway_message('quit', Quit_args)
    end
  end

  class Processor
    include ::Thrift::Processor

    def process_new_game(seqid, iprot, oprot)
      args = read_args(iprot, New_game_args)
      result = New_game_result.new()
      begin
        result.success = @handler.new_game(args.players, args.game_name, args.dict)
      rescue ::BadArgsException => what
        result.what = what
      end
      write_result(result, oprot, 'new_game', seqid)
    end

    def process_game_info(seqid, iprot, oprot)
      args = read_args(iprot, Game_info_args)
      result = Game_info_result.new()
      begin
        result.success = @handler.game_info(args.game_name)
      rescue ::BadArgsException => msg
        result.msg = msg
      end
      write_result(result, oprot, 'game_info', seqid)
    end

    def process_play_move(seqid, iprot, oprot)
      args = read_args(iprot, Play_move_args)
      result = Play_move_result.new()
      begin
        result.success = @handler.play_move(args.tiles, args.gamestate)
      rescue ::BadArgsException => msg
        result.msg = msg
      end
      write_result(result, oprot, 'play_move', seqid)
    end

    def process_pass_turn(seqid, iprot, oprot)
      args = read_args(iprot, Pass_turn_args)
      result = Pass_turn_result.new()
      begin
        result.success = @handler.pass_turn(args.gamestate)
      rescue ::BadArgsException => msg
        result.msg = msg
      end
      write_result(result, oprot, 'pass_turn', seqid)
    end

    def process_get_scrabblecheat_suggestions(seqid, iprot, oprot)
      args = read_args(iprot, Get_scrabblecheat_suggestions_args)
      result = Get_scrabblecheat_suggestions_result.new()
      begin
        result.success = @handler.get_scrabblecheat_suggestions(args.rack, args.board, args.game_name, args.dict)
      rescue ::BadArgsException => msg
        result.msg = msg
      end
      write_result(result, oprot, 'get_scrabblecheat_suggestions', seqid)
    end

    def process_quit(seqid, iprot, oprot)
      args = read_args(iprot, Quit_args)
      @handler.quit()
      return
    end

  end

  # HELPER FUNCTIONS AND STRUCTURES

  class New_game_args
    include ::Thrift::Struct, ::Thrift::Struct_Union
    PLAYERS = 1
    GAME_NAME = 2
    DICT = 3

    FIELDS = {
      PLAYERS => {:type => ::Thrift::Types::LIST, :name => 'players', :element => {:type => ::Thrift::Types::STRING}},
      GAME_NAME => {:type => ::Thrift::Types::I32, :name => 'game_name', :enum_class => ::GameName},
      DICT => {:type => ::Thrift::Types::I32, :name => 'dict', :enum_class => ::Dictionary}
    }

    def struct_fields; FIELDS; end

    def validate
      unless @game_name.nil? || ::GameName::VALID_VALUES.include?(@game_name)
        raise ::Thrift::ProtocolException.new(::Thrift::ProtocolException::UNKNOWN, 'Invalid value of field game_name!')
      end
      unless @dict.nil? || ::Dictionary::VALID_VALUES.include?(@dict)
        raise ::Thrift::ProtocolException.new(::Thrift::ProtocolException::UNKNOWN, 'Invalid value of field dict!')
      end
    end

    ::Thrift::Struct.generate_accessors self
  end

  class New_game_result
    include ::Thrift::Struct, ::Thrift::Struct_Union
    SUCCESS = 0
    WHAT = 1

    FIELDS = {
      SUCCESS => {:type => ::Thrift::Types::STRUCT, :name => 'success', :class => ::Gamestate},
      WHAT => {:type => ::Thrift::Types::STRUCT, :name => 'what', :class => ::BadArgsException}
    }

    def struct_fields; FIELDS; end

    def validate
    end

    ::Thrift::Struct.generate_accessors self
  end

  class Game_info_args
    include ::Thrift::Struct, ::Thrift::Struct_Union
    GAME_NAME = 1

    FIELDS = {
      GAME_NAME => {:type => ::Thrift::Types::I32, :name => 'game_name', :enum_class => ::GameName}
    }

    def struct_fields; FIELDS; end

    def validate
      unless @game_name.nil? || ::GameName::VALID_VALUES.include?(@game_name)
        raise ::Thrift::ProtocolException.new(::Thrift::ProtocolException::UNKNOWN, 'Invalid value of field game_name!')
      end
    end

    ::Thrift::Struct.generate_accessors self
  end

  class Game_info_result
    include ::Thrift::Struct, ::Thrift::Struct_Union
    SUCCESS = 0
    MSG = 1

    FIELDS = {
      SUCCESS => {:type => ::Thrift::Types::STRUCT, :name => 'success', :class => ::GameInfo},
      MSG => {:type => ::Thrift::Types::STRUCT, :name => 'msg', :class => ::BadArgsException}
    }

    def struct_fields; FIELDS; end

    def validate
    end

    ::Thrift::Struct.generate_accessors self
  end

  class Play_move_args
    include ::Thrift::Struct, ::Thrift::Struct_Union
    TILES = 1
    GAMESTATE = 2

    FIELDS = {
      TILES => {:type => ::Thrift::Types::LIST, :name => 'tiles', :element => {:type => ::Thrift::Types::STRUCT, :class => ::Tile}},
      GAMESTATE => {:type => ::Thrift::Types::STRUCT, :name => 'gamestate', :class => ::Gamestate}
    }

    def struct_fields; FIELDS; end

    def validate
    end

    ::Thrift::Struct.generate_accessors self
  end

  class Play_move_result
    include ::Thrift::Struct, ::Thrift::Struct_Union
    SUCCESS = 0
    MSG = 1

    FIELDS = {
      SUCCESS => {:type => ::Thrift::Types::STRUCT, :name => 'success', :class => ::Gamestate},
      MSG => {:type => ::Thrift::Types::STRUCT, :name => 'msg', :class => ::BadArgsException}
    }

    def struct_fields; FIELDS; end

    def validate
    end

    ::Thrift::Struct.generate_accessors self
  end

  class Pass_turn_args
    include ::Thrift::Struct, ::Thrift::Struct_Union
    GAMESTATE = 1

    FIELDS = {
      GAMESTATE => {:type => ::Thrift::Types::STRUCT, :name => 'gamestate', :class => ::Gamestate}
    }

    def struct_fields; FIELDS; end

    def validate
    end

    ::Thrift::Struct.generate_accessors self
  end

  class Pass_turn_result
    include ::Thrift::Struct, ::Thrift::Struct_Union
    SUCCESS = 0
    MSG = 1

    FIELDS = {
      SUCCESS => {:type => ::Thrift::Types::STRUCT, :name => 'success', :class => ::Gamestate},
      MSG => {:type => ::Thrift::Types::STRUCT, :name => 'msg', :class => ::BadArgsException}
    }

    def struct_fields; FIELDS; end

    def validate
    end

    ::Thrift::Struct.generate_accessors self
  end

  class Get_scrabblecheat_suggestions_args
    include ::Thrift::Struct, ::Thrift::Struct_Union
    RACK = 1
    BOARD = 2
    GAME_NAME = 3
    DICT = 4

    FIELDS = {
      RACK => {:type => ::Thrift::Types::STRING, :name => 'rack'},
      BOARD => {:type => ::Thrift::Types::LIST, :name => 'board', :element => {:type => ::Thrift::Types::STRUCT, :class => ::Tile}},
      GAME_NAME => {:type => ::Thrift::Types::I32, :name => 'game_name', :enum_class => ::GameName},
      DICT => {:type => ::Thrift::Types::I32, :name => 'dict', :enum_class => ::Dictionary}
    }

    def struct_fields; FIELDS; end

    def validate
      unless @game_name.nil? || ::GameName::VALID_VALUES.include?(@game_name)
        raise ::Thrift::ProtocolException.new(::Thrift::ProtocolException::UNKNOWN, 'Invalid value of field game_name!')
      end
      unless @dict.nil? || ::Dictionary::VALID_VALUES.include?(@dict)
        raise ::Thrift::ProtocolException.new(::Thrift::ProtocolException::UNKNOWN, 'Invalid value of field dict!')
      end
    end

    ::Thrift::Struct.generate_accessors self
  end

  class Get_scrabblecheat_suggestions_result
    include ::Thrift::Struct, ::Thrift::Struct_Union
    SUCCESS = 0
    MSG = 1

    FIELDS = {
      SUCCESS => {:type => ::Thrift::Types::LIST, :name => 'success', :element => {:type => ::Thrift::Types::STRUCT, :class => ::Move}},
      MSG => {:type => ::Thrift::Types::STRUCT, :name => 'msg', :class => ::BadArgsException}
    }

    def struct_fields; FIELDS; end

    def validate
    end

    ::Thrift::Struct.generate_accessors self
  end

  class Quit_args
    include ::Thrift::Struct, ::Thrift::Struct_Union

    FIELDS = {

    }

    def struct_fields; FIELDS; end

    def validate
    end

    ::Thrift::Struct.generate_accessors self
  end

  class Quit_result
    include ::Thrift::Struct, ::Thrift::Struct_Union

    FIELDS = {

    }

    def struct_fields; FIELDS; end

    def validate
    end

    ::Thrift::Struct.generate_accessors self
  end

end
