module Gamestate.NewGameParams (..) where

import Html exposing (..)
import Signal
import Debug
import Json.Decode
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Gamestate.Actions exposing (..)
import Gamestate.Models exposing (..)
import Actions
import Models


type alias ViewModel = 
  { 
    players : List String
  , game : GameName
  , dict : Dictionary
  , toplevelAddress : Signal.Address Actions.Action
  , toplevelModel : Models.AppModel
  }

type ViewAction = 
  NoOp
  | EditPlayerName Int String
  | SetGame GameName
  | SetDictionary Dictionary
  | SubmitParams


inbox : Signal.Mailbox ViewAction
inbox =
  Signal.mailbox NoOp


defaultModel : Signal.Address Actions.Action -> Models.AppModel -> ViewModel
defaultModel toplevelAddress toplevelModel =
  {
    players = ["", "", "", ""]
    , game = Scrabble
    , dict = TWL06
    , toplevelAddress = toplevelAddress
    , toplevelModel = toplevelModel
  }

update : ViewAction -> ViewModel -> ViewModel
update action model =
  case action of
    NoOp ->
      model
    EditPlayerName index str ->
      let
        oldPlayers = model.players
        newPlayers = List.concat [(List.take index oldPlayers), [str], (List.drop (index + 1) oldPlayers)]
      in
        Debug.log "New players:" { model | players = newPlayers }
    SetGame game -> Debug.log "New game" { model | game = game }
    SetDictionary dict -> Debug.log "New dict" { model | dict = dict }
    SubmitParams ->
      model


view : Signal.Address Actions.Action -> Models.AppModel -> Html.Html
view topLevelAddress toplevelModel  =
  viewLocal inbox.address (defaultModel topLevelAddress toplevelModel)


viewLocal : Signal.Address ViewAction -> ViewModel -> Html.Html
viewLocal address model =
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
        , div [ class "button" ] [ text "Create!" ]
      ]



{- Takes and index and sets and Action to change the Model to reflect the current player name -}
changeHandler : Int -> Signal.Address ViewAction -> ViewModel -> Attribute
changeHandler index address model =
  on "change" targetValue (\str -> Signal.message address (EditPlayerName index str))


gameHandler : Signal.Address ViewAction -> ViewModel -> Attribute
gameHandler address model =
  dropDownHandler (intToGame address model)


dictHandler : Signal.Address ViewAction -> ViewModel -> Attribute
dictHandler address model =
  dropDownHandler (intToDict address model)


intToGame : Signal.Address ViewAction -> ViewModel -> Int -> Signal.Message
intToGame address model key =
  let
    value = case key of
              1 -> SetGame WordsWithFriends
              2 -> SetGame Lexulous
              _ -> SetGame Scrabble
  in
    Signal.message address value

intToDict : Signal.Address ViewAction -> ViewModel -> Int -> Signal.Message
intToDict address model key =
  let
    value = case key of
              1 -> SetDictionary SOWPODS
              2 -> SetDictionary Zynga
              _ -> SetDictionary TWL06
  in
    Signal.message address value


dropDownHandler : (Int -> Signal.Message) -> Attribute
dropDownHandler fun = 
  on "change" (Json.Decode.at ["target", "selectedIndex"] Json.Decode.int) fun


{- 
  Send the changeHandler to the "local" address?
  Build out the proper submitHandler that goes back to app-level?
-}
