

/**
 * Thrift specification for the Client/Server of ScrabbleCheat.  We send 
 * Gamestates, which contain everything needed for the game at a given point 
 * in time.
 */

namespace erl ScrabbleCheat
namespace as3 com.morepaul.ScrabbleCheat


/** 
 * Tells us what kind of letter is in the tile, as this affects scoring.
 * If the tile is empty, we state it here.
 */
enum LetterType {
    CHARACTER,
    WILDCARD,
    EMPTY 
}



/**
 * Tells us what bonuses take place in a tile, if any.
 */
enum Bonus {
    TRIPLE_WORD_SCORE,
    DOUBLE_WORD_SCORE,
    TRIPLE_LETTER_SCORE,
    DOUBLE_LETTER_SCORE,
    NONE
}


/**
 * Represents a game the server can 'play'
 */
enum GameName {
    SCRABBLE,
    WORDS_WITH_FRIENDS,
    LEXULOUS
}


/**
 * Represents a game the server can 'play'
 */
enum Dictionary {
    TWL06,
    SOWPODS,
    ZYNGA 
}

/**
 * Basic datatype for a tile on the board.  'row' and 'col' are 1-indexed.
 * If the tile is empty the string is empty and LetterType is EMPTY.
 */
struct Tile {
    1: byte row,
    2: byte col,
    3: LetterType type,
    4: string letter,
    5: Bonus bonus,
}


// The board is just a list of tiles.
typedef list<Tile> Board



struct Move {
    1: list<Tile> move,
    2: i16 score,
}

struct Turn {
    1: Move move,
    2: string player,
}

struct GameInfo {
    1: GameName name,
    2: byte rack_size
    3: map<byte, i32> bingo_bonus,
    4: map<string, byte> letter_distribution,
    5: map<string, byte> score_distribution,
    6: list<Dictionary> allowed_dictionaries,
    7: Board board_template
}


/**
 * The primary data structure.  This allows the server to do any of its 
 * operations, and a client to display the game as needed to the client.
 */
struct Gamestate {
    1: Board board,
    2: map<string, i16> scores,
    3: string player_turn,
    4: list<string> turn_order,
    5: list<Turn> history,
    6: GameName game_name,
    7: Dictionary dict
}


/**
 * Called when a client asks for moves with a bad rack -- empty, containing 
 * bad characters, too many chars, etc.
 */ 
exception BadRackException {
    1: string reprimand,
}

/**
 * Called when a client asks for moves with a bad board -- Invalid state, wrong 
 * size, etc.
 */
exception BadBoardException {
    1: string reprimand,
}

exception BadMoveException { 1: string reprimand, }
exception BadGamestateException { 1: string reprimand, }
exception BadNamelistException { 1: string reprimand, }

service ScrabbleCheat {

    /**
     * Initiates a new game with the server.  This creates a new empty gamestate
     * with the specified players, game, and dictionary.
     */
    Gamestate new_game(1: list<string> players, 2: GameName game_name, 3: Dictionary dict)
                throws(1: BadNamelistException boo),


    /**
     * Returns the game info given the game's name. 
     */
    GameInfo game_info(1: GameName game_name),


    /**
     * Plays the specified move on the board, for the player.
     */
    Gamestate play_move(1: list<Tile> tiles, 2: Gamestate gamestate)
                 throws(1: BadMoveException boo, 2: BadGamestateException urns),

    /**
     * Allows a player to "pass" on their turn, if they can't/won't make a move.
     */
    Gamestate pass_turn(1: Gamestate gamestate)
                 throws(1: BadGamestateException msg),

    /**
     * The good stuff.  Get a list of moves given a rack, board.
     */
    list<Move> get_scrabblecheat_suggestions(1: string rack, 
                                             2: Board board, 
                                             3: GameName game_name,
                                             4: Dictionary dict) 
                                     throws (1: BadRackException boo, 
                                             2: BadBoardException urns),

    /**
     * Tells the server we're done here.
     */
    oneway void quit()
}
