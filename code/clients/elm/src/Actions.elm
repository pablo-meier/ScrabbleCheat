module Actions(..) where

import Players.Actions
import Gamestate.Actions
import Routing

type Action
  = NoOp
  | RoutingAction Routing.Action
  | PlayersAction Players.Actions.Action
  | GamestateAction Gamestate.Actions.Action
  | ShowError String
