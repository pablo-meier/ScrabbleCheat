module Gamestate.Update (..) where

import Effects exposing (Effects)
import Gamestate.Actions exposing (..)
import Gamestate.Models exposing (..)
import Gamestate.Effects exposing (..)
import Gamestate.NewGameParams
import Hop.Navigate exposing (navigateTo)
import Task

type alias UpdateModel =
    { gamestates : List Gamestate
    , newGameParams : NewGameParamsModel
    , showErrorAddress : Signal.Address String
    , deleteConfirmationAddress : Signal.Address (GamestateId, String)
    }

update : Action -> UpdateModel -> (List Gamestate, NewGameParamsModel, Effects Action)
update action model =
  case action of
    HopAction _ ->
      (model.gamestates, model.newGameParams, Effects.none)

    CreateGamestateGatherParams ->
      hopTo "/games/new" model

    CreateGamestate ->
      (model.gamestates, model.newGameParams, create new)

    UpdateGamestateParams action ->
      let (updatedNewGameParams, fx) = Gamestate.NewGameParams.update action model.newGameParams
      in (model.gamestates, updatedNewGameParams, fx)

    CreateGamestateDone result ->
      case result of
        Ok gamestate ->
          let updatedCollection = gamestate :: model.gamestates
          in (updatedCollection, model.newGameParams, Effects.none)

        Err error ->
          let message = toString error
              fx = Signal.send model.showErrorAddress message
                     |> Effects.task
                     |> Effects.map TaskDone
          in (model.gamestates, model.newGameParams, fx)

    SaveDone result ->
      case result of
        Ok gamestate ->
          let
            updatedGamestate existing = if existing.id == gamestate.id then gamestate else existing
            updatedCollection = List.map updatedGamestate model.gamestates
          in
            (updatedCollection, model.newGameParams, Effects.none)

        Err error ->
          let
            message = toString error
            fx = Signal.send model.showErrorAddress message
                   |> Effects.task
                   |> Effects.map TaskDone
          in
            (model.gamestates, model.newGameParams, fx)

    ListGamestates ->
      hopTo "/gamestates" model

    DeleteGamestateIntent gamestate ->
      let msg = "Are you sure you want to delete this game?"
          fx = Signal.send model.deleteConfirmationAddress (gamestate.id, msg)
                 |> Effects.task
                 |> Effects.map TaskDone
      in
        (model.gamestates, model.newGameParams, fx)

    DeleteGamestate gamestateId ->
      (model.gamestates, model.newGameParams, delete gamestateId)

    DeleteGamestateDone gamestateId result ->
      case result of
        Ok _ ->
          let
            notDeleted gamestate = gamestate.id /= gamestateId
            updatedCollection = List.filter notDeleted model.gamestates
          in
            (updatedCollection, model.newGameParams, Effects.none)

        Err error ->
          let
            message = toString error
            fx = Signal.send model.showErrorAddress message
                   |> Effects.task
                   |> Effects.map TaskDone
          in
            (model.gamestates, model.newGameParams, fx)

    FetchAllDone result ->
      case result of
        Ok gamestates ->
          (gamestates, model.newGameParams, Effects.none)
        Err error ->
          let
            errorMessage = toString error
            fx = Signal.send model.showErrorAddress errorMessage
              |> Effects.task
              |> Effects.map TaskDone
          in
            (model.gamestates, model.newGameParams, fx)
    
    TaskDone () ->
      (model.gamestates, model.newGameParams, Effects.none)

    NoOp ->
      (model.gamestates, model.newGameParams, Effects.none)


hopTo : String -> UpdateModel -> (List Gamestate, NewGameParamsModel, Effects Action)
hopTo loc model =
  (model.gamestates, model.newGameParams, Effects.map HopAction (navigateTo loc))
