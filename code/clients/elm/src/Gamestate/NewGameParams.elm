module Gamestate.NewGameParams (..) where

import Debug
import Html exposing (..)
import String
import Effects exposing (Effects)
import Signal
import Json.Decode
import Json.Encode as Encode
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Gamestate.Actions exposing (..)
import Gamestate.Models exposing (..)
import Gamestate.Effects
import Task
import Http

defaultModel : NewGameParamsModel
defaultModel =
  {
    players = ["", "", "", ""]
    , game = Scrabble
    , dict = TWL06
  }

update : NewGameParamsUpdateAction -> NewGameParamsModel -> (NewGameParamsModel, Effects Action)
update action model =
  case action of
    NewGameNoOp ->
      (model, Effects.none)
    EditPlayerName index str ->
      let
        oldPlayers = model.players
        newPlayers = List.concat [(List.take index oldPlayers), [str], (List.drop (index + 1) oldPlayers)]
      in
        ({ model | players = newPlayers }, Effects.none)
    SetGame game -> ({ model | game = game }, Effects.none)
    SetDictionary dict -> ({ model | dict = dict }, Effects.none)
    SubmitParams model ->
      Debug.log "Called SubmitParams" (defaultModel, save model)


view : Signal.Address Action -> NewGameParamsModel -> Html.Html
view address model =
  let
    input1 = input [ id "Player1", type' "text", changeHandler 0 address model ] []
    input2 = input [ id "Player2", type' "text", changeHandler 1 address model ] []
    input3 = input [ id "Player3", type' "text", changeHandler 2 address model ] []
    input4 = input [ id "Player4", type' "text", changeHandler 3 address model ] []
  in
    Html.form []
      [ 
        h1 [] [ text "Who's Playing?" ]
        , label [ for "Player1" ] [ text "Player 1:" ]
        , input1
        , label [ for "Player2" ] [ text "Player 2:" ]
        , input2
        , label [ for "Player3" ] [ text "Player 3:" ]
        , input3
        , label [ for "Player4" ] [ text "Player 4:" ]
        , input4
        , select [ name "game", gameHandler address model ] [
            option [ value "Scrabble" ] [ text "Scrabble" ]
            , option [ value "Words With Friends" ] [ text "Words With Friends" ]
            , option [ value "Lexulous" ] [ text "Lexulous" ]
          ]
        , select [ name "dict", dictHandler address model ] [
            option [ value "TWL06" ] [ text "TWL06" ]
            , option [ value "SOWPODS" ] [ text "SOWPODS" ]
            , option [ value "Words With Friends" ] [ text "Words With Friends" ]
          ]
        , button [ class "btn", submitHandler address model ] [ text "Create!" ]
      ]



{- Takes and index and sets and Action to change the Model to reflect the current player name -}
changeHandler : Int -> Signal.Address Action -> NewGameParamsModel -> Attribute
changeHandler index address model =
  on "keyup" targetValue (\str -> 
      EditPlayerName index str
      |> UpdateGamestateParams 
      |> Signal.message address)


gameHandler : Signal.Address Action -> NewGameParamsModel -> Attribute
gameHandler address model =
  dropDownHandler (intToGame address model)


dictHandler : Signal.Address Action -> NewGameParamsModel -> Attribute
dictHandler address model =
  dropDownHandler (intToDict address model)


intToGame : Signal.Address Action -> NewGameParamsModel -> Int -> Signal.Message
intToGame address model key =
  let
    value = case key of
              1 -> SetGame WordsWithFriends
              2 -> SetGame Lexulous
              _ -> SetGame Scrabble
  in
    Signal.message address (UpdateGamestateParams value)

intToDict : Signal.Address Action -> NewGameParamsModel -> Int -> Signal.Message
intToDict address model key =
  let
    value = case key of
              1 -> SetDictionary SOWPODS
              2 -> SetDictionary Zynga
              _ -> SetDictionary TWL06
  in
    Signal.message address (UpdateGamestateParams value)


dropDownHandler : (Int -> Signal.Message) -> Attribute
dropDownHandler fun = 
  on "change" (Json.Decode.at ["target", "selectedIndex"] Json.Decode.int) fun


{- 
  Send the changeHandler to the "local" address?
  Build out the proper submitHandler that goes back to app-level?
-}
submitHandler : Signal.Address Action -> NewGameParamsModel -> Attribute
submitHandler address model =
  onClick address (UpdateGamestateParams (SubmitParams model))


save : NewGameParamsModel -> Effects Action
save model =
  saveTask model
    |> Task.toResult
    |> Task.map SaveDone
    |> Effects.task

saveTask : NewGameParamsModel -> Task.Task Http.Error Gamestate
saveTask model =
  let
    body = newGamestateEncode model
             |> Encode.encode 0
             |> Http.string
    config = {
        verb = "POST"
        , headers = [("Content-Type", "application/json")]
        , url = newGameUrl
        , body = body
    }
  in
    Http.send Http.defaultSettings config
      |> Http.fromJson Gamestate.Effects.memberDecoder


newGameUrl : String
newGameUrl =
  "http://localhost:8080/games"


newGamestateEncode : NewGameParamsModel -> Encode.Value
newGamestateEncode model =
  let
    names = model.players
              |> List.filter (\x -> not (String.isEmpty x))
              |> List.map Encode.string
    list = [("names", Encode.list names)
           ,("game_name", Encode.int (Gamestate.Effects.gameCode model.game))
           ,("dict", Encode.int (Gamestate.Effects.dictCode model.dict))]
  in
    list |> Encode.object

