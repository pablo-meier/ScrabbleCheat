module View (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import String
import Actions exposing (..)
import Models exposing (..)
import Routing
import Gamestate.List
import Gamestate.NewGameParams
import Gamestate.Models exposing (GamestateId)
import Players.List
import Players.Edit
import Players.Models exposing (PlayerId)

view : Signal.Address Action -> AppModel -> Html
view address model =
  div
    []
    [ flash address model
    , page address model
    ]


page : Signal.Address Action -> AppModel -> Html.Html
page address model =
  case model.routing.route of
    Routing.PlayersRoute ->
      playersPage address model

    Routing.PlayerEditRoute playerId ->
      playerEditPage address model playerId

    Routing.GamestatesRoute ->
      gamestatePage address model

    Routing.GamestateParamsRoute ->
      gamestateParamPage address model

    Routing.NotFoundRoute ->
      notFoundView


playersPage : Signal.Address Action -> AppModel -> Html
playersPage address model =
  let
    viewModel = 
      { players = model.players
      }
  in
    Players.List.view (Signal.forwardTo address PlayersAction) viewModel


gamestatePage : Signal.Address Action -> AppModel -> Html
gamestatePage address model =
  let
    viewModel = { games = model.games }
  in
    Gamestate.List.view (Signal.forwardTo address GamestateAction) viewModel


gamestateParamPage : Signal.Address Action -> AppModel -> Html
gamestateParamPage address model =
  let
    viewModel = model.newGameParams
  in
    Gamestate.NewGameParams.view (Signal.forwardTo address GamestateAction) viewModel


playerEditPage : Signal.Address Action -> AppModel -> PlayerId -> Html
playerEditPage address model playerId =
  let
    maybePlayer = 
      model.players
        |> List.filter (\player -> player.id == playerId)
        |> List.head
  in
   case maybePlayer of
     Just player ->
       let
         viewModel = { player = player
                     }
       in
          Players.Edit.view (Signal.forwardTo address PlayersAction) viewModel

     Nothing ->
       notFoundView


notFoundView : Html.Html
notFoundView = 
  div
    []
    [ text "Not Found" ]


flash : Signal.Address Action -> AppModel -> Html
flash address model =
  if String.isEmpty model.errorMessage then
     span [] []
  else 
    div
      [ class "bold center p2 mb2 white bg-red rounded" ]
      [ text model.errorMessage ]
