
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

        empty_board_str = "--T0101|--n0102|--n0103|--d0104|--n0105|--n0106|--n0107|--T0108|--n0109|--n0110|--n0111|--d0112|--n0113|" +
                          "--n0114|--T0115|--n0201|--D0202|--n0203|--n0204|--n0205|--t0206|--n0207|--n0208|--n0209|--t0210|--n0211|" + 
                          "--n0212|--n0213|--D0214|--n0215|--n0301|--n0302|--D0303|--n0304|--n0305|--n0306|--d0307|--n0308|--d0309|" + 
                          "--n0310|--n0311|--n0312|--D0313|--n0314|--n0315|--d0401|--n0402|--n0403|--D0404|--n0405|--n0406|--n0407|" + 
                          "--d0408|--n0409|--n0410|--n0411|--D0412|--n0413|--n0414|--d0415|--n0501|--n0502|--n0503|--n0504|--D0505|" +
                          "--n0506|--n0507|--n0508|--n0509|--n0510|--D0511|--n0512|--n0513|--n0514|--n0515|--n0601|--t0602|--n0603|" + 
                          "--n0604|--n0605|--t0606|--n0607|--n0608|--n0609|--t0610|--n0611|--n0612|--n0613|--t0614|--n0615|--n0701|" +
                          "--n0702|--d0703|--n0704|--n0705|--n0706|--d0707|--n0708|--d0709|--n0710|--n0711|--n0712|--d0713|--n0714|" +
                          "--n0715|--T0801|--n0802|--n0803|--d0804|--n0805|--n0806|--n0807|--D0808|--n0809|--n0810|--n0811|--d0812|" + 
                          "--n0813|--n0814|--T0815|--n0901|--n0902|--d0903|--n0904|--n0905|--n0906|--d0907|--n0908|--d0909|--n0910|" + 
                          "--n0911|--n0912|--d0913|--n0914|--n0915|--n1001|--t1002|--n1003|--n1004|--n1005|--t1006|--n1007|--n1008|" + 
                          "--n1009|--t1010|--n1011|--n1012|--n1013|--t1014|--n1015|--n1101|--n1102|--n1103|--n1104|--D1105|--n1106|" +
                          "--n1107|--n1108|--n1109|--n1110|--D1111|--n1112|--n1113|--n1114|--n1115|--d1201|--n1202|--n1203|--D1204|" +
                          "--n1205|--n1206|--n1207|--d1208|--n1209|--n1210|--n1211|--D1212|--n1213|--n1214|--d1215|--n1301|--n1302|" +
                          "--D1303|--n1304|--n1305|--n1306|--d1307|--n1308|--d1309|--n1310|--n1311|--n1312|--D1313|--n1314|--n1315|" +
                          "--n1401|--D1402|--n1403|--n1404|--n1405|--t1406|--n1407|--n1408|--n1409|--t1410|--n1411|--n1412|--n1413|" +
                          "--D1414|--n1415|--T1501|--n1502|--n1503|--d1504|--n1505|--n1506|--n1507|--T1508|--n1509|--n1510|--n1511|" +
                          "--d1512|--n1513|--n1514|--T1515|"
        board = Serialization.deserialize(:board, empty_board_str)

        assert_equal({:letter => :none, :letter_type => :none, :bonus => :double_letter_score, :row => 1, :col => 4}, board[1][4])
        assert_equal({:letter => :none, :letter_type => :none, :bonus => :triple_word_score, :row => 8, :col => 1}, board[8][1])
        assert_equal({:letter => :none, :letter_type => :none, :bonus => :none, :row => 8, :col => 2}, board[8][2])
        assert_equal({:letter => :none, :letter_type => :none, :bonus => :triple_word_score, :row => 15, :col => 15}, board[15][15])

        with_moves_board = "--T0101|--n0102|--n0103|--d0104|--n0105|--n0106|--n0107|--T0108|--n0109|--n0110|--n0111|--d0112|--n0113|" +
                           "--n0114|--T0115|--n0201|--D0202|--n0203|--n0204|--n0205|--t0206|--n0207|--n0208|--n0209|--t0210|--n0211|" + 
                           "--n0212|--n0213|--D0214|--n0215|--n0301|--n0302|--D0303|--n0304|--n0305|--n0306|--d0307|--n0308|--d0309|" + 
                           "--n0310|--n0311|--n0312|--D0313|--n0314|--n0315|--d0401|--n0402|--n0403|--D0404|--n0405|--n0406|--n0407|" + 
                           "--d0408|--n0409|--n0410|--n0411|--D0412|--n0413|--n0414|--d0415|--n0501|--n0502|--n0503|--n0504|--D0505|" +
                           "--n0506|--n0507|--n0508|--n0509|--n0510|--D0511|--n0512|--n0513|--n0514|--n0515|--n0601|--t0602|--n0603|" + 
                           "--n0604|--n0605|--t0606|CAn0607|--n0608|--n0609|--t0610|--n0611|--n0612|--n0613|--t0614|--n0615|--n0701|" +
                           "--n0702|--d0703|WBn0704|--n0705|--n0706|--d0707|--n0708|--d0709|--n0710|--n0711|--n0712|--d0713|--n0714|" +
                           "--n0715|--T0801|--n0802|--n0803|--d0804|--n0805|--n0806|--n0807|CDD0808|--n0809|--n0810|--n0811|--d0812|" + 
                           "--n0813|--n0814|--T0815|--n0901|--n0902|--d0903|--n0904|--n0905|--n0906|--d0907|--n0908|--d0909|--n0910|" + 
                           "--n0911|--n0912|--d0913|--n0914|--n0915|--n1001|--t1002|--n1003|--n1004|--n1005|--t1006|--n1007|--n1008|" + 
                           "--n1009|--t1010|--n1011|--n1012|--n1013|--t1014|--n1015|--n1101|--n1102|--n1103|--n1104|--D1105|--n1106|" +
                           "--n1107|--n1108|--n1109|--n1110|--D1111|--n1112|--n1113|--n1114|--n1115|--d1201|--n1202|--n1203|--D1204|" +
                           "--n1205|--n1206|--n1207|--d1208|--n1209|--n1210|--n1211|--D1212|--n1213|--n1214|--d1215|--n1301|--n1302|" +
                           "--D1303|--n1304|--n1305|--n1306|--d1307|--n1308|--d1309|--n1310|--n1311|--n1312|--D1313|--n1314|--n1315|" +
                           "--n1401|--D1402|--n1403|--n1404|--n1405|--t1406|--n1407|--n1408|--n1409|--t1410|--n1411|--n1412|--n1413|" +
                           "--D1414|--n1415|--T1501|--n1502|--n1503|--d1504|--n1505|--n1506|--n1507|--T1508|--n1509|--n1510|--n1511|" +
                           "--d1512|--n1513|--n1514|CGT1515|"
        board = Serialization.deserialize(:board, with_moves_board)

        assert_equal({:letter => "A", :letter_type => :character, :bonus => :none, :row => 6, :col => 7}, board[6][7])
        assert_equal({:letter => "B", :letter_type => :wildcard, :bonus => :none, :row => 7, :col => 4}, board[7][4])
        assert_equal({:letter => "D", :letter_type => :character, :bonus => :double_word_score, :row => 8, :col => 8}, board[8][8])
        assert_equal({:letter => "G", :letter_type => :character, :bonus => :triple_word_score, :row => 15, :col => 15}, board[15][15])


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
