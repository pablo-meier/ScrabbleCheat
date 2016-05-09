module Models (..) where

import Gamestate.Models exposing (Gamestate, NewGameParamsModel)
import Gamestate.NewGameParams
import Players.Models exposing (Player)
import Routing

type alias AppModel =
  { games : List Gamestate
  , newGameParams : NewGameParamsModel
  , players : List Player
  , routing : Routing.Model
  , errorMessage : String
  }

initialModel : AppModel
initialModel =
  { games = []
  , newGameParams = Gamestate.NewGameParams.defaultModel
  , players = []
  , routing = Routing.initialModel
  , errorMessage = ""
  }
