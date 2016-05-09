module Update (..) where

import Models exposing (..)
import Actions exposing (..)
import Effects exposing (Effects)
import Mailboxes exposing (..)
import Routing
import Players.Update
import Gamestate.Update

update : Action -> AppModel -> ( AppModel, Effects Action )
update action model =
  case action of
    PlayersAction subAction ->
      let
        updateModel =
          { players = model.players
          , showErrorAddress = Signal.forwardTo actionsMailbox.address ShowError
          , deleteConfirmationAddress = askDeleteConfirmationMailbox.address
          }
        (updatedPlayers, fx) = Players.Update.update subAction updateModel
      in
        ( {model | players = updatedPlayers }, Effects.map PlayersAction fx )

    GamestateAction subAction ->
      let
        updateModel =
          {gamestates = model.games
          , newGameParams = model.newGameParams
          , showErrorAddress = Signal.forwardTo actionsMailbox.address ShowError
          , deleteConfirmationAddress = askDeleteConfirmationMailbox.address
          }
        (updatedGames, updatedGameParams, fx) = Gamestate.Update.update subAction updateModel
      in
        ({model | games = updatedGames, newGameParams = updatedGameParams}, Effects.map GamestateAction fx)

    RoutingAction subAction ->
      let
        (updatedRouting, fx) =
          Routing.update subAction model.routing
      in
         ( {model | routing = updatedRouting }, Effects.map RoutingAction fx )

    ShowError message ->
      ({model | errorMessage = message}, Effects.none)

    NoOp ->
      (model, Effects.none)
