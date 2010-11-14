
require 'serialization.rb'
require 'test/unit'


class SerializationTest < Test::Unit::TestCase

    def test_tile_deserialization
        testtile1 = "--t0104"
        result = Serialization.deserialize(:tile, testtile1)
        assert_equal({:letter => :none, :letter_type => :none, :bonus => :triple_letter_score, :row => 1, :col => 4}, result)

        testtile2 = "WTn1114"
        result2 = Serialization.deserialize(:tile, testtile2)
        assert_equal({:letter => "T", :letter_type => :wildcard, :bonus => :none, :row => 11, :col => 14}, result2)

        testtile3 = "CRT0606"
        result3 = Serialization.deserialize(:tile, testtile3)
        assert_equal({:letter => "R", :letter_type => :character, :bonus => :triple_word_score, :row => 6, :col => 6}, result3)
    end


    def test_scores_deserialization
        scorestr1 = "Paul$100|Sam$50|"
        assert_equal({"Paul" => 100, "Sam" => 50}, Serialization.deserialize(:scores, scorestr1))

        scorestr2 = "Sam$50|Paul$100|Robert$4|Brett$60|"
        assert_equal({"Sam" => 50, "Paul" => 100, "Robert" => 4, "Brett" => 60}, Serialization.deserialize(:scores, scorestr2))
    end


    def test_board_deserialization
        assert(true)
    end
    
    def test_history_deserialization
        history_str = "Paul$CAn0707|CBn0708|CLn0709|CEn0710|$100|" +
                      "Sam$CPn0607|CAn0608|CUn0609|CLn0610|$20|" +
                      "Paul$CPn0509|CWn0609|CNn0709|CTn0809|$200|" +
                      "Sam$CHn1001|CEn1101|CLn1201|CPn1301$15|"
        
        firstmove = {:name => "Paul", :move => [tile("CAn0707"),tile("CBn0708"),tile("CLn0709"),tile("CEn0710")], :score => 100}
        secondmove = {:name => "Sam", :move => [tile("CPn0607"),tile("CAn0608"),tile("CUn0609"),tile("CLn0610")], :score => 20}
        thirdmove = {:name => "Paul", :move => [tile("CPn0509"),tile("CWn0609"),tile("CNn0709"),tile("CTn0809")], :score => 200}
        fourthmove = {:name => "Sam", :move => [tile("CHn1001"),tile("CEn1101"),tile("CLn1201"),tile("CPn1301")], :score => 15}

        result = []
        result << firstmove
        result << secondmove
        result << thirdmove
        result << fourthmove

        assert_equal(result, Serialization.deserialize(:history, history_str))
    end

    # lame, but makes the above test case possible...
    def tile(str)
        Serialization.deserialize(:tile, str)
    end

end
