module Players.Update (..) where

import Effects exposing (Effects)
import Players.Actions exposing (..)
import Players.Models exposing (..)
import Players.Effects exposing (..)
import Hop.Navigate exposing (navigateTo)
import Task

type alias UpdateModel =
    { players : List Player
    , showErrorAddress : Signal.Address String
    , deleteConfirmationAddress : Signal.Address (PlayerId, String)
    }

update : Action -> UpdateModel -> (List Player, Effects Action)
update action model =
  case action of
    HopAction _ ->
      (model.players, Effects.none)

    CreatePlayer ->
      (model.players, create new)

    CreatePlayerDone result ->
      case result of
        Ok player ->
          let updatedCollection = player :: model.players
              fx = Task.succeed (EditPlayer player.id) |> Effects.task
          in (updatedCollection, fx)

        Err error ->
          let message = toString error
              fx = Signal.send model.showErrorAddress message
                     |> Effects.task
                     |> Effects.map TaskDone
          in (model.players, fx)

    EditPlayer id ->
      let
        path = "/players/" ++ (toString id) ++ "/edit"
      in
        hopTo path model

    ChangeLevel playerId howMuch ->
      let
        fxForPlayer player = 
          if player.id /= playerId then
             Effects.none
          else
            let updatedPlayer = {player | level = player.level + howMuch}
            in if updatedPlayer.level > 0 then
              save updatedPlayer
            else 
              Effects.none

        fx = List.map fxForPlayer model.players
               |> Effects.batch
      in
        (model.players, fx)

    ChangeName playerId newName ->
      let
        fxForPlayer player =
          if player.id /= playerId then
            Effects.none
          else
            let
              updatedPlayer = {player | name = newName}
            in
              save updatedPlayer

        fx = List.map fxForPlayer model.players
               |> Effects.batch
      in
        (model.players, fx)

    SaveDone result ->
      case result of
        Ok player ->
          let
            updatedPlayer existing = if existing.id == player.id then player else existing
            updatedCollection = List.map updatedPlayer model.players
          in
            (updatedCollection, Effects.none)

        Err error ->
          let
            message = toString error
            fx = Signal.send model.showErrorAddress message
                   |> Effects.task
                   |> Effects.map TaskDone
          in
            (model.players, fx)

    ListPlayers ->
      let
        path = "/players"
      in
        hopTo path model

    DeletePlayerIntent player ->
      let msg = "Are you sure you want to delete " ++ player.name ++ "?"
          fx = Signal.send model.deleteConfirmationAddress (player.id, msg)
                 |> Effects.task
                 |> Effects.map TaskDone
      in
        (model.players, fx)

    DeletePlayer playerId ->
      (model.players, delete playerId)

    DeletePlayerDone playerId result ->
      case result of
        Ok _ ->
          let
            notDeleted player = player.id /= playerId
            updatedCollection = List.filter notDeleted model.players
          in
            (updatedCollection, Effects.none)

        Err error ->
          let
            message = toString error
            fx = Signal.send model.showErrorAddress message
                   |> Effects.task
                   |> Effects.map TaskDone
          in
            (model.players, fx)

    FetchAllDone result ->
      case result of
        Ok players ->
          (players, Effects.none)
        Err error ->
          let
            errorMessage = toString error
            fx = Signal.send model.showErrorAddress errorMessage
              |> Effects.task
              |> Effects.map TaskDone
          in
            (model.players, fx)
    
    TaskDone () ->
      (model.players, Effects.none)

    NoOp ->
      (model.players, Effects.none)


hopTo : String -> UpdateModel -> (List Player, Effects Action)
hopTo loc model =
  (model.players, Effects.map HopAction (navigateTo loc))
