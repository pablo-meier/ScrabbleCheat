module Main (..) where

import Html exposing (..)
import Effects exposing (Effects, Never)
import Task
import StartApp
import Actions exposing (..)
import Models exposing (..)
import Update exposing (..)
import View exposing (..)
import Players.Effects
import Players.Actions
import Gamestate.Effects
import Mailboxes exposing (..)
import Routing

-- Start App
init : (AppModel, Effects Action)
init =
    let
      fxs = [ Effects.map GamestateAction Gamestate.Effects.fetchAll ]
      fx = Effects.batch fxs
    in
      (Models.initialModel, fx)

app : StartApp.App AppModel
app =
    StartApp.start
      {
          init = init
          , inputs = [ 
              routerSignal
              , actionsMailbox.signal
              , getDeleteConfirmationSignal 
          ]
          , update = update
          , view = view
      }

main : Signal.Signal Html
main = app.html

routerSignal : Signal Action
routerSignal =
  Signal.map RoutingAction Routing.signal

port runner : Signal (Task.Task Never ())
port runner =
    app.tasks

port routeRunTask : Task.Task () ()
port routeRunTask =
  Routing.run

port askDeleteConfirmation : Signal (Int, String)
port askDeleteConfirmation = askDeleteConfirmationMailbox.signal

port getDeleteConfirmation : Signal Int

getDeleteConfirmationSignal : Signal Actions.Action
getDeleteConfirmationSignal =
  let toAction id = id
                      |> Players.Actions.DeletePlayer
                      |> PlayersAction
  in
    Signal.map toAction getDeleteConfirmation
