module Routing(..) where

import Task exposing (Task)
import Effects exposing (Effects, Never)
import Hop
import Hop.Types exposing (Location, PathMatcher, Router, newLocation)
import Hop.Navigate exposing (navigateTo)
import Hop.Matchers exposing (match1, match2, match3, int)
import Players.Models exposing (PlayerId)

{-
  Available routes for the app.
  NotFoundRoute is necessary for unrecognized routes.
-}

type Route
  = PlayersRoute
  | GamestatesRoute
  | PlayerEditRoute PlayerId
  | NotFoundRoute

type Action
  = HopAction ()
  | ApplyRoute (Route, Location)
  | NavigateTo String

type alias Model =
  { location : Location
  , route : Route
  }

initialModel : Model
initialModel =
  { location = newLocation
  , route = GamestatesRoute
  }


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NavigateTo path ->
      (model, Effects.map HopAction (navigateTo path))

    ApplyRoute (route, location) ->
      ({ model | route = route, location = location }, Effects.none)

    HopAction () ->
      (model, Effects.none)


{- Actual routes -}

indexMatcher : PathMatcher Route
indexMatcher =
  match1 GamestatesRoute "/"


gamestatesMatcher : PathMatcher Route
gamestatesMatcher =
  match1 GamestatesRoute "/games"


playersMatcher : PathMatcher Route
playersMatcher =
  match1 PlayersRoute "/players"


playerEditMatch : PathMatcher Route
playerEditMatch =
  match3 PlayerEditRoute "/players/" int "/edit"


matchers : List (PathMatcher Route)
matchers =
  [ indexMatcher
  , playersMatcher
  , playerEditMatch
  ]

{- Hop router -}
router : Router Route
router =
  Hop.new
    { matchers = matchers
    , notFound = NotFoundRoute
    }

run : Task () ()
run = router.run

signal : Signal Action
signal =
  Signal.map ApplyRoute router.signal
