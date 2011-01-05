

/**
 * Thrift specification for the Client/Server of ScrabbleCheat.  We send Gamestates, which
 * contain everything needed for the game at a given point in time.
 */

namespace erl ScrabbleCheat
namespace rb  ScrabbleCheat

typedef i32 int


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
 * Basic datatype for a tile on the board.  'row' and 'col' are 1-indexed.
 * If the tile is empty the string is empty and LetterType is EMPTY.
 */
struct Tile {
    1: int row,
    2: int col,
    3: LetterType type,
    4: string letter,
    5: Bonus bonus,
}


// The board is just a list of tiles.
typedef list<Tile> Board



struct Move {
    1: list<Tile> move,
    2: int score,
}

struct Turn {
    1: Move move,
    2: string player,
}


/**
 * The primary data structure.  This allows the server to do any of its operations, and a client to
 * display the game as needed to the client.
 */
struct Gamestate {
    1: Board board,
    2: map<string, int> scores,
    3: string player_turn,
    4: list<string> turn_order,
    5: list<Turn> history,
}


/**
 * Called when a client asks for moves with a bad rack -- empty, containing bad characters, too many chars, etc.
 */ 
exception BadRackException {
    1: string reprimand,
}

/**
 * Called when a client asks for moves with a bad board -- Invalid state, wrong size, etc.
 */
exception BadBoardException {
    1: string reprimand,
}

exception BadMoveException { 1: string reprimand, }
exception BadGamestateException { 1: string reprimand, }
exception BadNamelistException { 1: string reprimand, }

service ScrabbleCheat {

    /**
     * Initiates a new game with the server.  This creates a new empty gamestate with the specified players.
     */
    Gamestate new_game(1: list<string> players)
                throws(1: BadNamelistException boo),


    /**
     * Plays the specified move on the board, for the player.
     */
    Gamestate play_move(1: list<Tile> tiles, 2: Gamestate gamestate)
                 throws(1: BadMoveException boo, 2: BadGamestateException urns),


    /**
     * The good stuff.  Get a list of moves given a rack and a board.
     */
    list<Move> get_scrabblecheat_suggestions(1: string rack, 2: Board board) 
                                     throws (1: BadRackException boo, 2: BadBoardException urns),

    /**
     * Tells the server we're done here.
     */
    oneway void quit()
}
