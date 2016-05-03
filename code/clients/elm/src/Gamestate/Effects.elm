module Gamestate.Effects (..) where

import Dict exposing (Dict)
import String
import Effects exposing (Effects)
import Http
import Json.Decode as Decode exposing ((:=), Decoder)
import Json.Encode as Encode
import Result
import Task
import Gamestate.Models exposing (..)
import Gamestate.Actions exposing (..)

domain : String
domain =
    "http://localhost"

portNumber : String
portNumber =
  toString 8080

baseUrl : String
baseUrl =
  domain ++ ":" ++ portNumber

fetchAllUrl : String
fetchAllUrl =
  baseUrl ++ "/games"

createUrl : String
createUrl =
  baseUrl ++ "/games"

deleteUrl : GamestateId -> String
deleteUrl gamestateId =
  baseUrl ++ "/games/" ++ (toString gamestateId)

saveUrl : GamestateId -> String
saveUrl gamestateId =
  baseUrl ++ "/games/" ++ (toString gamestateId)



fetchAll : Effects Action
fetchAll =
  Http.get collectionDecoder fetchAllUrl
    |> Task.toResult
    |> Task.map FetchAllDone
    |> Effects.task

collectionDecoder : Decode.Decoder (List Gamestate)
collectionDecoder =
    Decode.list memberDecoder

memberDecoder : Decode.Decoder Gamestate
memberDecoder =
    Decode.object8
      Gamestate
        ("id" := Decode.int)
        ("board" := (Decode.list tileDecoder))
        ("scores" := (Decode.dict Decode.int))
        ("player_turn" := Decode.string)
        ("turn_order" := (Decode.list Decode.string))
        ("history" := (Decode.list turnDecoder))
        ("game_name" := gameNameDecoder)
        ("dict" := dictionaryDecoder)
        


tileDecoder : Decoder Tile
tileDecoder =
  Decode.object5
    Tile
      ("row" := Decode.int)
      ("col" := Decode.int)
      ("type" := letterTypeDecoder)
      ("letter" := valueDecoder)
      ("bonus" := bonusDecoder)


turnDecoder : Decoder Turn
turnDecoder =
  Decode.object2
    Turn
      ("move" := moveDecoder)
      ("player" := Decode.string)


moveDecoder : Decoder Move
moveDecoder =
  Decode.object2
    Move
      ("move" := Decode.list tileDecoder)
      ("score" := Decode.int)


create : Gamestate -> Effects Action
create gamestate =
  let
    body =
      memberEncoded gamestate
        |> Encode.encode 0
        |> Http.string
    
    config =
      { verb = "POST"
      , headers = [("Content-Type", "application/json")]
      , url = createUrl
      , body = body
      }
  in
    Http.send Http.defaultSettings config
      |> Http.fromJson memberDecoder
      |> Task.toResult
      |> Task.map CreateGamestateDone
      |> Effects.task


memberEncoded : Gamestate -> Encode.Value
memberEncoded gamestate =
  let
    list = [
      ("id", Encode.int gamestate.id)
      ,("dict", encodeDictionary gamestate.dictionary)
      ,("game_name", encodeGameName gamestate.game)
      ,("player_turn", Encode.string gamestate.playerTurn)
      ,("turn_order", Encode.list (List.map Encode.string gamestate.turnOrder))
      ,("history", Encode.list (List.map encodeTurn gamestate.history))
      ,("scores", Dict.toList gamestate.scores
                    |> List.map (\x -> (fst x, Encode.int (snd x)))
                    |> Encode.object)
      ,("board", Encode.list (List.map encodeTile gamestate.board))
    ]
  in
    list |> Encode.object


encodeTurn : Turn -> Encode.Value
encodeTurn turn =
  let
    list = [
      ("player", Encode.string turn.player)
      ,("move", encodeMove turn.move)
    ]
  in
     list |> Encode.object


encodeMove : Move -> Encode.Value
encodeMove move =
  let
    list = [
      ("score", Encode.int move.score)
      ,("move", Encode.list (List.map encodeTile move.move))
    ]
  in
    list |> Encode.object

encodeTile : Tile -> Encode.Value
encodeTile tile =
  let
    list = [
      ("row", Encode.int tile.row)
      ,("col", Encode.int tile.col)
      ,("letter", encodeLetterValue tile.value)
      ,("type", encodeLetterType tile.letterType)
      ,("bonus", encodeBonus tile.bonus)
    ]
  in
    list |> Encode.object

encodeLetterValue : Maybe String -> Encode.Value
encodeLetterValue letterValue =
  Maybe.withDefault "" letterValue
    |> Encode.string

encodeLetterType : Maybe LetterType -> Encode.Value
encodeLetterType letterType =
  let
    intCode = letterTypeCode letterType
  in Encode.int intCode

encodeBonus : Maybe Bonus -> Encode.Value
encodeBonus bonus =
  let
    intCode = bonusCode bonus
  in Encode.int intCode

encodeDictionary : Dictionary -> Encode.Value
encodeDictionary dict =
  let
    intCode = dictCode dict
  in Encode.int intCode


encodeGameName : GameName -> Encode.Value
encodeGameName game =
  let
    intCode = gameCode game
  in Encode.int intCode


delete : GamestateId -> Effects Action
delete gamestateId =
  deleteTask gamestateId
    |> Task.toResult
    |> Task.map (DeleteGamestateDone gamestateId)
    |> Effects.task

save : Gamestate -> Effects Action
save gamestate =
  saveTask gamestate
    |> Task.toResult
    |> Task.map SaveDone
    |> Effects.task


deleteTask : GamestateId -> Task.Task Http.Error ()
deleteTask gamestateId =
  let
    config = {
        verb = "DELETE"
        , headers = [("Content-Type", "application/json")]
        , url = deleteUrl gamestateId 
        , body = Http.empty
    }
  in
    Http.send Http.defaultSettings config
      |> Http.fromJson (Decode.succeed ())


saveTask : Gamestate -> Task.Task Http.Error Gamestate
saveTask player =
  let
    body = memberEncoded player
             |> Encode.encode 0
             |> Http.string
    config = {
        verb = "PATCH"
        , headers = [("Content-Type", "application/json")]
        , url = saveUrl player.id
        , body = body
    }
  in
    Http.send Http.defaultSettings config
      |> Http.fromJson memberDecoder


{- THE FOLLOWING MAPPINGS COME FROM THRIFT -}

letterTypeDecoder : Decoder (Maybe LetterType)
letterTypeDecoder =
  Decode.customDecoder Decode.int intToLetterType


valueDecoder : Decoder (Maybe String)
valueDecoder =
  let
    emptyMeansMaybe x =
      if String.isEmpty x then
        Result.Ok Nothing
      else 
        Result.Ok (Just x)
  in
    Decode.customDecoder Decode.string emptyMeansMaybe


bonusDecoder : Decoder (Maybe Bonus)
bonusDecoder =
  Decode.customDecoder Decode.int intToBonus


dictionaryDecoder : Decoder Dictionary
dictionaryDecoder =
  let
    intToDictionary x = 
      case x of
        0 -> Result.Ok TWL06
        1 -> Result.Ok SOWPODS
        2 -> Result.Ok Zynga
        _ -> Result.Err "Invalid Dictionary Code."
  in
    Decode.customDecoder Decode.int intToDictionary


gameNameDecoder : Decoder GameName
gameNameDecoder =
  let
    intToGameName x = 
      case x of
        0 -> Result.Ok Scrabble
        1 -> Result.Ok WordsWithFriends
        2 -> Result.Ok Lexulous
        _ -> Result.Err "Invalid GameName value"
  in
    Decode.customDecoder Decode.int intToGameName


intToLetterType : Int -> Result String (Maybe LetterType)
intToLetterType letterType =
  case letterType of
    0 -> Result.Ok (Just Character)
    1 -> Result.Ok (Just Wildcard)
    2 -> Result.Ok Nothing
    _ -> Result.Err "Invalid LetterType"


letterTypeCode : Maybe LetterType -> Int
letterTypeCode letterType =
  case letterType of
    Just Character -> 0
    Just Wildcard -> 1
    Nothing -> 2


bonusCode : Maybe Bonus -> Int
bonusCode bonus =
  case bonus of
    Just DoubleLetterScore -> 3
    Just TripleLetterScore -> 2
    Just DoubleWordScore -> 1
    Just TripleWordScore -> 0
    Nothing -> 4


intToBonus : Int -> Result String (Maybe Bonus)
intToBonus  bonus =
  case bonus of
    3 -> Result.Ok (Just DoubleLetterScore)
    2 -> Result.Ok (Just TripleLetterScore)
    1 -> Result.Ok (Just DoubleWordScore)
    0 -> Result.Ok (Just TripleWordScore)
    4 -> Result.Ok (Nothing)
    _ -> Result.Err "Invalid Bonus Type"


dictCode : Dictionary -> Int
dictCode dict =
  case dict of
    TWL06 -> 0
    SOWPODS -> 1
    Zynga -> 2


gameCode : GameName -> Int
gameCode game =
  case game of
    Scrabble -> 0
    WordsWithFriends -> 1
    Lexulous -> 2
