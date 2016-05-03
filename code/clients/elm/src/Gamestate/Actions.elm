module Gamestate.Actions(..) where

import Http
import Gamestate.Models exposing (Gamestate, GamestateId)
import Hop

type Action
  = NoOp
  | HopAction ()

  | CreateGamestateGatherParams
  | CreateGamestate
  | CreateGamestateDone (Result Http.Error Gamestate)

  | SaveDone (Result Http.Error Gamestate)

  | ListGamestates

  | DeleteGamestateIntent Gamestate
  | DeleteGamestate GamestateId
  | DeleteGamestateDone GamestateId (Result Http.Error ())

  | FetchAllDone (Result Http.Error (List Gamestate))
  | TaskDone ()
