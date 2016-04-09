module Models (..) where

import Gamestate.Models exposing (Gamestate)
import Players.Models exposing (Player)
import Routing

type alias AppModel =
  { games : List Gamestate
  , players : List Player
  , routing : Routing.Model
  , errorMessage : String
  }

initialModel : AppModel
initialModel =
  { games = []
  , players = []
  , routing = Routing.initialModel
  , errorMessage = ""
  }
