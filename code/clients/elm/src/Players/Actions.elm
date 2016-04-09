module Players.Actions(..) where

import Http
import Players.Models exposing (PlayerId, Player)
import Hop

type Action
  = NoOp
  | HopAction ()
  | CreatePlayer
  | CreatePlayerDone (Result Http.Error Player)
  | EditPlayer PlayerId
  | ChangeLevel PlayerId Int
  | ChangeName PlayerId String
  | SaveDone (Result Http.Error Player)
  | ListPlayers
  | DeletePlayerIntent Player
  | DeletePlayer PlayerId
  | DeletePlayerDone PlayerId (Result Http.Error ())
  | FetchAllDone (Result Http.Error (List Player))
  | TaskDone ()
