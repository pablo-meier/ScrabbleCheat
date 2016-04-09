module Gamestate.Update (..) where

import Effects exposing (Effects)
import Gamestate.Actions exposing (..)
import Gamestate.Models exposing (..)
import Gamestate.Effects exposing (..)
import Hop.Navigate exposing (navigateTo)
import Task

type alias UpdateModel =
    { gamestates : List Gamestate
    , showErrorAddress : Signal.Address String
    , deleteConfirmationAddress : Signal.Address (GamestateId, String)
    }

update : Action -> UpdateModel -> (List Gamestate, Effects Action)
update action model =
  case action of
    HopAction _ ->
      (model.gamestates, Effects.none)

    CreateGamestate ->
      (model.gamestates, create new)

    CreateGamestateDone result ->
      case result of
        Ok gamestate ->
          let updatedCollection = gamestate :: model.gamestates
          in (updatedCollection, Effects.none)

        Err error ->
          let message = toString error
              fx = Signal.send model.showErrorAddress message
                     |> Effects.task
                     |> Effects.map TaskDone
          in (model.gamestates, fx)

    SaveDone result ->
      case result of
        Ok gamestate ->
          let
            updatedGamestate existing = if existing.id == gamestate.id then gamestate else existing
            updatedCollection = List.map updatedGamestate model.gamestates
          in
            (updatedCollection, Effects.none)

        Err error ->
          let
            message = toString error
            fx = Signal.send model.showErrorAddress message
                   |> Effects.task
                   |> Effects.map TaskDone
          in
            (model.gamestates, fx)

    ListGamestates ->
      hopTo "/gamestates" model

    DeleteGamestateIntent gamestate ->
      let msg = "Are you sure you want to delete this game?"
          fx = Signal.send model.deleteConfirmationAddress (gamestate.id, msg)
                 |> Effects.task
                 |> Effects.map TaskDone
      in
        (model.gamestates, fx)

    DeleteGamestate gamestateId ->
      (model.gamestates, delete gamestateId)

    DeleteGamestateDone gamestateId result ->
      case result of
        Ok _ ->
          let
            notDeleted gamestate = gamestate.id /= gamestateId
            updatedCollection = List.filter notDeleted model.gamestates
          in
            (updatedCollection, Effects.none)

        Err error ->
          let
            message = toString error
            fx = Signal.send model.showErrorAddress message
                   |> Effects.task
                   |> Effects.map TaskDone
          in
            (model.gamestates, fx)

    FetchAllDone result ->
      case result of
        Ok gamestates ->
          (gamestates, Effects.none)
        Err error ->
          let
            errorMessage = toString error
            fx = Signal.send model.showErrorAddress errorMessage
              |> Effects.task
              |> Effects.map TaskDone
          in
            (model.gamestates, fx)
    
    TaskDone () ->
      (model.gamestates, Effects.none)

    NoOp ->
      (model.gamestates, Effects.none)


hopTo : String -> UpdateModel -> (List Gamestate, Effects Action)
hopTo loc model =
  (model.gamestates, Effects.map HopAction (navigateTo loc))
