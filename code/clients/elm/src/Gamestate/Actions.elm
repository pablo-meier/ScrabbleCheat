module Gamestate.Actions(..) where

import Http
import Gamestate.Models exposing (Gamestate, GamestateId, GameName, Dictionary)
import Hop

type NewGameParamsUpdateAction =
  NewGameNoOp
  | EditPlayerName Int String
  | SetGame GameName
  | SetDictionary Dictionary
  | SubmitParams Gamestate.Models.NewGameParamsModel


type Action
  = NoOp
  | HopAction ()

  | CreateGamestateGatherParams
  | UpdateGamestateParams NewGameParamsUpdateAction
  | CreateGamestate
  | CreateGamestateDone (Result Http.Error Gamestate)

  | SaveDone (Result Http.Error Gamestate)

  | ListGamestates

  | DeleteGamestateIntent Gamestate
  | DeleteGamestate GamestateId
  | DeleteGamestateDone GamestateId (Result Http.Error ())

  | FetchAllDone (Result Http.Error (List Gamestate))
  | TaskDone ()
