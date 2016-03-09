-module(game_parser).

-export([parse_game/1, 
         parse_game_body/2,
         new_board/0]).

-include("gameinfo.hrl").

-import(board, [place_bonus_on_board/4]).

-define(BOARD_LENGTH, 15).
-define(BOARD_HEIGHT, ?BOARD_LENGTH).

-define(MAPPING_PATTERN, "^(.+)\\s*-\\s*(\\d+)").

-define(TRIPLE_WORD_SCORE, "TW").
-define(DOUBLE_WORD_SCORE, "DW").
-define(TRIPLE_LETTER_SCORE, "TL").
-define(DOUBLE_LETTER_SCORE, "DL").
-define(NO_BONUS, "N").


%% parse_game :: gamename() -> gameinfo()
%%
%% where gamename = scrabble | lexulous | words_with_friends
%%       gameinfo = {gameinfo, board(), String * Number map, String * Number map, int()}
%%
%% Parses files related to game-specific information, such as a board, the 
%% point distribution of letters, and the letter distribution.
parse_game(GameName) ->
    GameInfoDir = lists:concat([code:priv_dir(scrabblecheat), "/games/", GameName, '/']),
    parse_game_body(GameName, GameInfoDir).


%% Separated out to aid in testing, with our unit tests. Provide a hard-coded 
%% path to the games directory, rather than a dynamically-generated path to the 
%% priv directory.
parse_game_body(GameName, GameInfoDir) ->
    Board = parse_board(GameInfoDir ++ "board.txt"),
    {ok, MapPattern} = re:compile(?MAPPING_PATTERN),
    LetterDist = parse_mapping(GameInfoDir ++ "letterdist.txt", MapPattern),
    ScoreDist = parse_mapping(GameInfoDir ++ "points.txt", MapPattern),
    RackSize = parse_number_in_file(GameInfoDir ++ "rack_size.txt"),
    BingoBonuses = parse_bonuses(GameInfoDir ++ "bingo_bonuses.txt", MapPattern),
    Dicts = parse_dicts(GameInfoDir ++ "allowed_dicts.txt"),
    #gameinfo{name = GameName,
              board = Board, 
              letterdist = LetterDist, 
              scoredist = ScoreDist, 
              racksize = RackSize,
              bingo_bonuses = BingoBonuses,
              allowed_dicts = Dicts}.


parse_number_in_file(Filename) ->
    NumberAsString = split_into_lines(Filename),
    list_to_integer(hd(NumberAsString)).


parse_board(Filename) ->
    Lines = split_into_lines(Filename),
    Split = lists:map(fun(Y) -> re:split(Y, "\\s") end, Lines),
    AsStrings = inner_map(fun binary_to_list/1, Split),
    Filtered = inner_filter(fun (X) -> length(X) > 0 end, AsStrings),
    Bonuses = inner_map(fun string_to_bonus/1, Filtered),
    AsTiles = make_tiles(Bonuses, 1, []),
    make_fixed_array(lists:map(fun (X) -> make_fixed_array(X) end, AsTiles)).

make_fixed_array(X) -> array:fix(array:from_list(X)).

%% Call map on each list in a list of lists.
inner_map(Fun, Outer) -> lists:map(fun (X) -> lists:map(Fun, X) end, Outer).
inner_filter(Fun, Outer) -> lists:map(fun (X) -> lists:filter(Fun, X) end, Outer).


make_tiles([], _, Accum) -> 
    lists:reverse(Accum);
make_tiles([H|T], Row, Accum) ->
    FilledIn = fill_row(H, Row, 1, []),
    make_tiles(T, Row + 1, [FilledIn|Accum]).

fill_row([], _, _, Accum) ->
    lists:reverse(Accum);
fill_row([Bonus|Rst], Row, Col, Accum) ->
    NewTile = tile:new_tile(none, Bonus, Row, Col),
    fill_row(Rst, Row, Col + 1, [NewTile|Accum]).


string_to_bonus(?NO_BONUS) -> none;
string_to_bonus(?TRIPLE_WORD_SCORE) -> triple_word_score;
string_to_bonus(?DOUBLE_WORD_SCORE) -> double_word_score;
string_to_bonus(?TRIPLE_LETTER_SCORE) -> triple_letter_score;
string_to_bonus(?DOUBLE_LETTER_SCORE) -> double_letter_score.



%% Parses a file of the form Char - Value into a dict.
parse_mapping(Filename, MapPattern) ->
    parse_mapping_common(Filename, MapPattern, fun hd/1, fun list_to_integer/1).


%% Very similar to parse_mapping, but we accept a number on the left-hand side rather than
%% a char..
parse_bonuses(Filename, MapPattern) ->
    parse_mapping_common(Filename, MapPattern, fun list_to_integer/1, fun list_to_integer/1).


%% what all mapping parsers use.
parse_mapping_common(Filename, MapPattern, TransformKey, TransformValue) ->
    Lines = split_into_lines(Filename),
    Mapped = lists:map(fun (Line) ->
                           {match, Matches} = re:run(Line, MapPattern, [{capture, all_but_first, list}]),
                           LHS = string:strip(hd(Matches)),
                           RHS = string:strip(hd(tl(Matches))),

                           Key = TransformKey(LHS),
                           Value = TransformValue(RHS),
                           {Key, Value}
                        end, Lines),
    Empty = dict:new(),
    lists:foldl(fun ({Key,Value},Dict) -> dict:store(Key,Value,Dict) end, Empty, Mapped).


%% parse_dicts :: String -> [atom()]
%%
%% Each atom() in the result can only be one that refers to a proper 
%% dictionary, such as 'twl06', 'sowpods', or 'zynga'.
parse_dicts(Filename) ->
    Lines = split_into_lines(Filename),
    lists:map(fun list_to_atom/1, Lines).


split_into_lines(Filename) ->
    case file:open(Filename, read) of
        {ok, FileHandle} ->
            line_looper(FileHandle, []);
        {error, enoent} ->
            throw({file_not_found, Filename})
    end.

line_looper(FileHandle, Lst) ->
    Line = io:get_line(FileHandle, ''),
    case Line of
        eof -> lists:reverse(Lst);
        _Else -> 
            Stripped = string:strip(Line, right, $\n),
            line_looper(FileHandle, [Stripped|Lst])
    end.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%   OBSOLETE CODE - DELETE SOON
%%
%% This code creates a new Scrabble board by default.  In theory, we'd like to do
%% this by simply calling GameInfo#gameinfo.board on a Scrabble GameInfo, but 
%% almost all test rely on this function, and they're called with the gameinfo files
%% in the OTP apps priv/ directory.  We should delete this code as soon as a 
%% resolution is found, wherein we either test on the OTP app running the server
%% (preferred) or hack it, via a debug build that searches for the files in a
%% hard-coded path (lame).


%% new_board :: () -> Board
%%
%% Generates a new, blank Scrabble board.  This includes all the bonuses, but
%% no tiles in place.


-define(TRIPLE_WORD_SCORE_LOCATIONS, 
    [{1,1},{1,8},{1,15},{8,1},{8,15},{15,1},{15,8},{15,15}]).
-define(TRIPLE_LETTER_SCORE_LOCATIONS, 
    [{2,6},{2,10},{6,2},{6,6},{6,10},{6,14},{10,2},{10,6},{10,10},{10,14},{14,6},{14,10}]).
-define(DOUBLE_WORD_SCORE_LOCATIONS, 
    [{2,2},{2,14},{3,3},{3,13},{4,4},{4,12},{5,5},{5,11},{8,8},{11,5},{11,11},{12,4},{12,12},{13,3},
    {13,13},{14,2},{14,14}]).
-define(DOUBLE_LETTER_SCORE_LOCATIONS, 
    [{1,4},{1,12},{3,7},{3,9},{4,1},{4,8},{4,15},{7,3},{7,7},{7,9},{7,13},{8,4},{8,12},
    {9,3},{9,7},{9,9},{9,13},{12,1},{12,8},{12,15},{13,7},{13,9},{15,4},{15,12}]).


new_board() ->
    Empty = empty_board_template(),
    lists:foldl(fun ({BonusType, Lst}, Acc) -> mass_inject(Acc, BonusType, Lst) end,
                Empty,
                [{triple_word_score, ?TRIPLE_WORD_SCORE_LOCATIONS}, {double_word_score, ?DOUBLE_WORD_SCORE_LOCATIONS},
                {triple_letter_score, ?TRIPLE_LETTER_SCORE_LOCATIONS}, {double_letter_score, ?DOUBLE_LETTER_SCORE_LOCATIONS}]).

mass_inject(Board, Type, Locations) ->
    lists:foldl( fun (Loc, Acc) ->
                     {Row, Col} = Loc,
                     place_bonus_on_board(Row, Col, Type, Acc)
                 end, Board, Locations).


%% empty_board_template :: () -> Array<Array<Tile>>
empty_board_template() ->
    ListOfRows = make_board_rows(0, []),
    ArrayOfRows = array:from_list(ListOfRows),
    array:fix(ArrayOfRows).


%% make_board_rows :: Int * [Array<Tile>] -> [Array<Tile>]
make_board_rows(?BOARD_HEIGHT, Accum) -> lists:reverse(Accum);
make_board_rows(Index, Accum) ->
    ArrayRow = array:from_list(make_board_columns(Index, 0, [])),
    Fixed = array:fix(ArrayRow),
    make_board_rows(1 + Index, [Fixed|Accum]).

make_board_columns(_, ?BOARD_HEIGHT, Accum) -> lists:reverse(Accum);
make_board_columns(RowNumber, ColNumber, Accum) ->
    make_board_columns(RowNumber, ColNumber + 1, [tile:new_tile(none, none, RowNumber + 1, ColNumber + 1)|Accum]).


