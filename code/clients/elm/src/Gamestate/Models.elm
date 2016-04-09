module Gamestate.Models (..) where

import Dict exposing (Dict)

type Dictionary = 
    TWL06
    | SOWPODS
    | Zynga

type GameName =
    Scrabble
    | Lexulous
    | WordsWithFriends

type alias Move =
  { move : List Tile
  , score : Int
  }

type alias Turn =
  { move : Move
  , player : String
  }

{-| Represents a Bonus Tile in Scrabble. Often they are different colors, and rather treasured!
-}
type Bonus =
    DoubleLetterScore
    | TripleLetterScore
    | DoubleWordScore
    | TripleWordScore

{-| Represents the "type" of letter this is. This is almost always Character, which is your
  standard letter, but Wildcards can be any letter in the alphabet, but themselves are
  worth 0 points.
-}
type LetterType =
    Character
    | Wildcard

{-| The toplevel Tile datatype. Keeps track of its own row and column, as well as whether or not
  it's got a letter (and which Type) via letterType and bonus. Can arguably cleaned up by having
  a Location type for (row,col) and an, idk, "occupancy" type for letterType/value since the
  two are coupled.
-}
type alias Tile = {
    row : Int,
    col : Int,
    letterType : Maybe LetterType,
    value : Maybe String,
    bonus : Maybe Bonus
}


type alias Board = List Tile

type alias GamestateId = Int

type alias GameInfo = 
  { game : GameName
  , rackSize : Int
  , bingoBonus : Int
  , letterDistribution : Dict String Int
  , scoreDistribution : Dict String Int
  , allowedDicts : List Dictionary
  , boardTemplate : Board
  }

type alias Gamestate =
  { id : GamestateId
  , board : Board
  , scores : Dict String Int
  , playerTurn : String
  , turnOrder : List String
  , history : List Turn
  , game : GameName
  , dictionary : Dictionary
  }

new : Gamestate
new =
  { id = 0
  , board = sampleBoard
  , scores = Dict.fromList [("Pablo", 10), ("Sapo", 10)]
  , playerTurn = "Pablo"
  , turnOrder = ["Pablo", "Sapo"]
  , history = []
  , game = Scrabble
  , dictionary = TWL06
  }
  

sampleBoard : Board
sampleBoard = [
      {row = 1, col = 1, letterType = Nothing, value = Nothing, bonus = Just TripleWordScore },
      {row = 1, col = 2, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 1, col = 3, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 1, col = 4, letterType = Nothing, value = Nothing, bonus = Just DoubleLetterScore },
      {row = 1, col = 5, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 1, col = 6, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 1, col = 7, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 1, col = 8, letterType = Nothing, value = Nothing, bonus = Just TripleWordScore },
      {row = 1, col = 9, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 1, col = 10, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 1, col = 11, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 1, col = 12, letterType = Nothing, value = Nothing, bonus = Just DoubleLetterScore },
      {row = 1, col = 13, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 1, col = 14, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 1, col = 15, letterType = Nothing, value = Nothing, bonus = Just TripleWordScore },
      
      {row = 2, col = 1, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 2, col = 2, letterType = Nothing, value = Nothing, bonus = Just DoubleWordScore },
      {row = 2, col = 3, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 2, col = 4, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 2, col = 5, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 2, col = 6, letterType = Nothing, value = Nothing, bonus = Just TripleLetterScore },
      {row = 2, col = 7, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 2, col = 8, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 2, col = 9, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 2, col = 10, letterType = Nothing, value = Nothing, bonus = Just TripleLetterScore },
      {row = 2, col = 11, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 2, col = 12, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 2, col = 13, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 2, col = 14, letterType = Nothing, value = Nothing, bonus = Just DoubleWordScore },
      {row = 2, col = 15, letterType = Nothing, value = Nothing, bonus = Nothing },

      {row = 3, col = 1, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 3, col = 2, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 3, col = 3, letterType = Nothing, value = Nothing, bonus = Just DoubleWordScore },
      {row = 3, col = 4, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 3, col = 5, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 3, col = 6, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 3, col = 7, letterType = Nothing, value = Nothing, bonus = Just DoubleLetterScore },
      {row = 3, col = 8, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 3, col = 9, letterType = Nothing, value = Nothing, bonus = Just DoubleLetterScore },
      {row = 3, col = 10, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 3, col = 11, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 3, col = 12, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 3, col = 13, letterType = Nothing, value = Nothing, bonus = Just DoubleWordScore },
      {row = 3, col = 14, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 3, col = 15, letterType = Nothing, value = Nothing, bonus = Nothing },

      {row = 4, col = 1, letterType = Nothing, value = Nothing, bonus = Just DoubleLetterScore },
      {row = 4, col = 2, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 4, col = 3, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 4, col = 4, letterType = Nothing, value = Nothing, bonus = Just DoubleWordScore },
      {row = 4, col = 5, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 4, col = 6, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 4, col = 7, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 4, col = 8, letterType = Nothing, value = Nothing, bonus = Just DoubleLetterScore },
      {row = 4, col = 9, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 4, col = 10, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 4, col = 11, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 4, col = 12, letterType = Nothing, value = Nothing, bonus = Just DoubleWordScore },
      {row = 4, col = 13, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 4, col = 14, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 4, col = 15, letterType = Nothing, value = Nothing, bonus = Just DoubleLetterScore },

      {row = 5, col = 1, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 5, col = 2, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 5, col = 3, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 5, col = 4, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 5, col = 5, letterType = Nothing, value = Nothing, bonus = Just DoubleWordScore },
      {row = 5, col = 6, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 5, col = 7, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 5, col = 8, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 5, col = 9, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 5, col = 10, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 5, col = 11, letterType = Nothing, value = Nothing, bonus = Just DoubleWordScore },
      {row = 5, col = 12, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 5, col = 13, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 5, col = 14, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 5, col = 15, letterType = Nothing, value = Nothing, bonus = Nothing },

      {row = 6, col = 1, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 6, col = 2, letterType = Nothing, value = Nothing, bonus = Just TripleLetterScore },
      {row = 6, col = 3, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 6, col = 4, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 6, col = 5, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 6, col = 6, letterType = Nothing, value = Nothing, bonus = Just TripleLetterScore },
      {row = 6, col = 7, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 6, col = 8, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 6, col = 9, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 6, col = 10, letterType = Nothing, value = Nothing, bonus = Just TripleLetterScore },
      {row = 6, col = 11, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 6, col = 12, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 6, col = 13, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 6, col = 14, letterType = Nothing, value = Nothing, bonus = Just TripleLetterScore },
      {row = 6, col = 15, letterType = Nothing, value = Nothing, bonus = Nothing },

      {row = 7, col = 1, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 7, col = 2, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 7, col = 3, letterType = Nothing, value = Nothing, bonus = Just DoubleLetterScore },
      {row = 7, col = 4, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 7, col = 5, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 7, col = 6, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 7, col = 7, letterType = Nothing, value = Nothing, bonus = Just DoubleLetterScore },
      {row = 7, col = 8, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 7, col = 9, letterType = Nothing, value = Nothing, bonus = Just DoubleLetterScore },
      {row = 7, col = 10, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 7, col = 11, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 7, col = 12, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 7, col = 13, letterType = Nothing, value = Nothing, bonus = Just DoubleLetterScore },
      {row = 7, col = 14, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 7, col = 15, letterType = Nothing, value = Nothing, bonus = Nothing },

      {row = 8, col = 1, letterType = Nothing, value = Nothing, bonus = Just TripleWordScore },
      {row = 8, col = 2, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 8, col = 3, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 8, col = 4, letterType = Nothing, value = Nothing, bonus = Just DoubleLetterScore },
      {row = 8, col = 5, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 8, col = 6, letterType = Just Character, value = Just "C", bonus = Nothing },
      {row = 8, col = 7, letterType = Just Character, value = Just "A", bonus = Nothing },
      {row = 8, col = 8, letterType = Just Wildcard, value = Just "R", bonus = Just DoubleWordScore },
      {row = 8, col = 9, letterType = Just Character, value = Just "E", bonus = Nothing },
      {row = 8, col = 10, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 8, col = 11, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 8, col = 12, letterType = Nothing, value = Nothing, bonus = Just DoubleLetterScore },
      {row = 8, col = 13, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 8, col = 14, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 8, col = 15, letterType = Nothing, value = Nothing, bonus = Just TripleWordScore },

      {row = 9, col = 1, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 9, col = 2, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 9, col = 3, letterType = Nothing, value = Nothing, bonus = Just DoubleLetterScore },
      {row = 9, col = 4, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 9, col = 5, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 9, col = 6, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 9, col = 7, letterType = Just Character, value = Just "B", bonus = Just DoubleLetterScore },
      {row = 9, col = 8, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 9, col = 9, letterType = Nothing, value = Nothing, bonus = Just DoubleLetterScore },
      {row = 9, col = 10, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 9, col = 11, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 9, col = 12, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 9, col = 13, letterType = Nothing, value = Nothing, bonus = Just DoubleLetterScore },
      {row = 9, col = 14, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 9, col = 15, letterType = Nothing, value = Nothing, bonus = Nothing },

      {row = 10, col = 1, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 10, col = 2, letterType = Nothing, value = Nothing, bonus = Just TripleLetterScore },
      {row = 10, col = 3, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 10, col = 4, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 10, col = 5, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 10, col = 6, letterType = Nothing, value = Nothing, bonus = Just TripleLetterScore },
      {row = 10, col = 7, letterType = Just Character, value = Just "L", bonus = Nothing },
      {row = 10, col = 8, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 10, col = 9, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 10, col = 10, letterType = Nothing, value = Nothing, bonus = Just TripleLetterScore },
      {row = 10, col = 11, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 10, col = 12, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 10, col = 13, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 10, col = 14, letterType = Nothing, value = Nothing, bonus = Just TripleLetterScore },
      {row = 10, col = 15, letterType = Nothing, value = Nothing, bonus = Nothing },

      {row = 11, col = 1, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 11, col = 2, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 11, col = 3, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 11, col = 4, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 11, col = 5, letterType = Nothing, value = Nothing, bonus = Just DoubleWordScore },
      {row = 11, col = 6, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 11, col = 7, letterType = Just Character, value = Just "E", bonus = Nothing },
      {row = 11, col = 8, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 11, col = 9, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 11, col = 10, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 11, col = 11, letterType = Nothing, value = Nothing, bonus = Just DoubleWordScore },
      {row = 11, col = 12, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 11, col = 13, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 11, col = 14, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 11, col = 15, letterType = Nothing, value = Nothing, bonus = Nothing },

      {row = 12, col = 1, letterType = Nothing, value = Nothing, bonus = Just DoubleLetterScore },
      {row = 12, col = 2, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 12, col = 3, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 12, col = 4, letterType = Nothing, value = Nothing, bonus = Just DoubleWordScore },
      {row = 12, col = 5, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 12, col = 6, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 12, col = 7, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 12, col = 8, letterType = Nothing, value = Nothing, bonus = Just DoubleLetterScore },
      {row = 12, col = 9, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 12, col = 10, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 12, col = 11, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 12, col = 12, letterType = Nothing, value = Nothing, bonus = Just DoubleWordScore },
      {row = 12, col = 13, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 12, col = 14, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 12, col = 15, letterType = Nothing, value = Nothing, bonus = Just DoubleLetterScore },

      {row = 13, col = 1, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 13, col = 2, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 13, col = 3, letterType = Nothing, value = Nothing, bonus = Just DoubleWordScore },
      {row = 13, col = 4, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 13, col = 5, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 13, col = 6, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 13, col = 7, letterType = Nothing, value = Nothing, bonus = Just DoubleLetterScore },
      {row = 13, col = 8, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 13, col = 9, letterType = Nothing, value = Nothing, bonus = Just DoubleLetterScore },
      {row = 13, col = 10, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 13, col = 11, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 13, col = 12, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 13, col = 13, letterType = Nothing, value = Nothing, bonus = Just DoubleWordScore },
      {row = 13, col = 14, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 13, col = 15, letterType = Nothing, value = Nothing, bonus = Nothing },

      {row = 14, col = 1, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 14, col = 2, letterType = Nothing, value = Nothing, bonus = Just DoubleWordScore },
      {row = 14, col = 3, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 14, col = 4, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 14, col = 5, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 14, col = 6, letterType = Nothing, value = Nothing, bonus = Just TripleLetterScore },
      {row = 14, col = 7, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 14, col = 8, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 14, col = 9, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 14, col = 10, letterType = Nothing, value = Nothing, bonus = Just TripleLetterScore },
      {row = 14, col = 11, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 14, col = 12, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 14, col = 13, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 14, col = 14, letterType = Nothing, value = Nothing, bonus = Just DoubleWordScore },
      {row = 14, col = 15, letterType = Nothing, value = Nothing, bonus = Nothing },

      {row = 15, col = 1, letterType = Nothing, value = Nothing, bonus = Just TripleWordScore },
      {row = 15, col = 2, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 15, col = 3, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 15, col = 4, letterType = Nothing, value = Nothing, bonus = Just DoubleLetterScore },
      {row = 15, col = 5, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 15, col = 6, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 15, col = 7, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 15, col = 8, letterType = Nothing, value = Nothing, bonus = Just TripleWordScore },
      {row = 15, col = 9, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 15, col = 10, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 15, col = 11, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 15, col = 12, letterType = Nothing, value = Nothing, bonus = Just DoubleLetterScore },
      {row = 15, col = 13, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 15, col = 14, letterType = Nothing, value = Nothing, bonus = Nothing },
      {row = 15, col = 15, letterType = Nothing, value = Nothing, bonus = Just TripleWordScore }
  ]

scrabbleScoreDistribution : Dict String Int
scrabbleScoreDistribution =
    let
      dist = [
          ("A", 1),
          ("B", 3),
          ("C", 3),
          ("D", 2),
          ("E", 1),
          ("F", 4),
          ("G", 2),
          ("H", 4),
          ("I", 1),
          ("J", 8),
          ("K", 5),
          ("L", 1),
          ("M", 3),
          ("N", 1),
          ("O", 1),
          ("P", 3),
          ("Q", 10),
          ("R", 1),
          ("S", 1),
          ("T", 1),
          ("U", 1),
          ("V", 4),
          ("W", 4),
          ("X", 8),
          ("Y", 4),
          ("Z", 10)
      ]
    in
       List.foldl (\x accum -> Dict.insert (fst x) (snd x) accum) Dict.empty dist
