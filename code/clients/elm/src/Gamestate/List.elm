module Gamestate.List (..) where

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
    , td [] [ text "Placeholder" ]
    , td
        []
        [ deleteBtn address gamestate ]
    ]

addBtn : Signal.Address Action -> ViewModel -> Html.Html
addBtn address model =
  button
    [ class "btn", onClick address CreateGamestate ]
    [ i [ class "fa fa-plus-circle mr1" ] []
    , text "Add Game"
    ]

deleteBtn : Signal.Address Action -> Gamestate -> Html.Html
deleteBtn address gamestate =
  button
    [class "btn regular mr1"
    ,onClick address (DeleteGamestateIntent gamestate)]
    [ i [ class "fa fa-trash mr1" ] []
    , text "Delete"]
