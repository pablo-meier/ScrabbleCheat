module Gamestate.List (..) where

import Dict
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Gamestate.Actions exposing (..)
import Gamestate.Models exposing (Gamestate)


type alias ViewModel = 
  { games : List Gamestate
  }


view : Signal.Address Action -> ViewModel -> Html.Html
view address model =
  div
    []
    [ nav address model
    , list address model
    ]

nav : Signal.Address Action -> ViewModel -> Html.Html
nav address model =
  div
    [ class "clearfix mb2 white bg-black" ]
    [ div [ class "left p2" ] [ text "Games" ]
    , div [ class "right p1" ] [ addBtn address model ]
    ]


list : Signal.Address Action -> ViewModel -> Html.Html
list address model =
  div
    []
    [ table
        [ class "table-light" ]
        [ thead
            []
            [ tr
                []
                [ th [] [ text "Id" ]
                , th [] [ text "Players/Scores" ]
                , th [] [ text "Actions" ]
                ]
            ]
        , tbody [] (List.map (gameRow address model) model.games)
        ]
    ]


gameRow : Signal.Address Action -> ViewModel -> Gamestate -> Html.Html
gameRow address model gamestate =
  tr
    []
    [ td [] [ text (toString gamestate.id) ]
    , td [] [ summaryCard gamestate ]
    , td
        []
        [ deleteBtn address gamestate ]
    ]

summaryCard : Gamestate -> Html.Html
summaryCard gs =
  table [] (List.map (scoreToTableRow gs.playerTurn) (Dict.toList gs.scores))

scoreToTableRow : String -> (String, Int) -> Html.Html
scoreToTableRow current pair =
  tr [] [
    td [] [ playerLabel (fst pair) current ]
    , td [] [ text (pair |> snd |> toString) ]
  ]

playerLabel : String -> String -> Html.Html
playerLabel player current =
  if player == current then span [] [ i [class "fa fa-star"] [], text player] else text player


addBtn : Signal.Address Action -> ViewModel -> Html.Html
addBtn address model =
  button
    [ class "btn", onClick address CreateGamestateGatherParams ]
    [ i [ class "fa fa-plus-circle mr1" ] []
    , text "New Game"
    ]

deleteBtn : Signal.Address Action -> Gamestate -> Html.Html
deleteBtn address gamestate =
  button
    [class "btn regular mr1"
    ,onClick address (DeleteGamestateIntent gamestate)]
    [ i [ class "fa fa-trash mr1" ] []
    , text "Delete"]
