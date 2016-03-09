-ifndef(_scrabbleCheat_types_included).
-define(_scrabbleCheat_types_included, yeah).

-define(scrabbleCheat_LetterType_CHARACTER, 0).
-define(scrabbleCheat_LetterType_WILDCARD, 1).
-define(scrabbleCheat_LetterType_EMPTY, 2).

-define(scrabbleCheat_Bonus_TRIPLE_WORD_SCORE, 0).
-define(scrabbleCheat_Bonus_DOUBLE_WORD_SCORE, 1).
-define(scrabbleCheat_Bonus_TRIPLE_LETTER_SCORE, 2).
-define(scrabbleCheat_Bonus_DOUBLE_LETTER_SCORE, 3).
-define(scrabbleCheat_Bonus_NONE, 4).

-define(scrabbleCheat_GameName_SCRABBLE, 0).
-define(scrabbleCheat_GameName_WORDS_WITH_FRIENDS, 1).
-define(scrabbleCheat_GameName_LEXULOUS, 2).

-define(scrabbleCheat_Dictionary_TWL06, 0).
-define(scrabbleCheat_Dictionary_SOWPODS, 1).
-define(scrabbleCheat_Dictionary_ZYNGA, 2).

%% struct 'tile'

-record('tile', {'row' :: integer(),
                 'col' :: integer(),
                 'type' :: integer(),
                 'letter' :: string() | binary(),
                 'bonus' :: integer()}).
-type 'tile'() :: #'tile'{}.

%% struct 'move'

-record('move', {'move' :: list(),
                 'score' :: integer()}).
-type 'move'() :: #'move'{}.

%% struct 'turn'

-record('turn', {'move' :: 'move'(),
                 'player' :: string() | binary()}).
-type 'turn'() :: #'turn'{}.

%% struct 'gameInfo'

-record('gameInfo', {'name' :: integer(),
                     'rack_size' :: integer(),
                     'bingo_bonus' :: dict:dict(),
                     'letter_distribution' :: dict:dict(),
                     'score_distribution' :: dict:dict(),
                     'allowed_dictionaries' :: list(),
                     'board_template' :: list()}).
-type 'gameInfo'() :: #'gameInfo'{}.

%% struct 'gamestate'

-record('gamestate', {'board' :: list(),
                      'scores' :: dict:dict(),
                      'player_turn' :: string() | binary(),
                      'turn_order' :: list(),
                      'history' :: list(),
                      'game_name' :: integer(),
                      'dict' :: integer()}).
-type 'gamestate'() :: #'gamestate'{}.

%% struct 'badArgsException'

-record('badArgsException', {'reprimand' :: string() | binary()}).
-type 'badArgsException'() :: #'badArgsException'{}.

-endif.
