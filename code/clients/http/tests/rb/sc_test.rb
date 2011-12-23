
$:.unshift 'lib/'


require 'thrift'
require 'scrabble_cheat'
require 'test/unit'


class TestHttpServer < Test::Unit::TestCase

	def test_new_game
		transport = Thrift::HTTPClientTransport.new("http://127.0.0.1:9090")
		protocol = Thrift::BinaryProtocol.new(transport)
		client = ScrabbleCheat::Client.new(protocol)

		transport.open()

		gs = client.new_game(["Paul", "Sam"], GameName::SCRABBLE, Dictionary::TWL06)

		assert_equal("Paul", gs.player_turn)

		gs.scores.each do |k,v|
			assert_equal(0, v)
			is_good_name = k.eql?("Paul") || k.eql?("Sam")
			assert(is_good_name)
		end

		transport.close()
	end
end
