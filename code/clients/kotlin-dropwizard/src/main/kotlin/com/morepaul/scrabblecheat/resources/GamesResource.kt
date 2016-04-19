package com.morepaul.scrabblecheat.resources

import com.codahale.metrics.annotation.Timed
import com.google.common.collect.Lists
import com.morepaul.scrabblecheat.ScrabbleCheatWebappApplication
import com.morepaul.scrabblecheat.api.*
import com.morepaul.scrabblecheat.external.thriftgenerated.*
import com.morepaul.scrabblecheat.external.thriftgenerated.Dictionary
import com.morepaul.scrabblecheat.jdbi.GamestateDAO
import java.util.*
import javax.validation.Valid
import javax.validation.constraints.NotNull
import javax.ws.rs.*
import javax.ws.rs.core.MediaType

// FIXME: Handle errors.
/**
 * Basic toplevel games resource.
 */
@Path("/games")
@Produces(MediaType.APPLICATION_JSON)
class GamesResource(
        val scClient :ScrabbleCheat.Client,
        val dao : GamestateDAO) {


    @Timed
    @GET
    fun listGames() : List<ApiGamestate> {
        return dao.getGamestates()
    }

    @Timed
    @GET
    @Path("/{game_id}")
    fun getGame(@PathParam("game_id") gameId : Int) : ApiGamestate {
        return dao.getGamestateById(gameId)
    }


    // FIXME UUIDs not ints
    @Timed
    @POST
    fun createGame(@NotNull @Valid params : CreateGameParameters) : ApiGamestate {
        val gs = scClient.newGame(
                params.names,
                GameName.findByValue(params.game),
                Dictionary.findByValue(params.dict))

        val newId = dao.getHighestId() + 1
        val newGs = ApiGamestate.fromGamestate(gs, newId)
        dao.insertGamestate(newId, newGs)

        return newGs
    }

    @Timed
    @POST
    @Path("/{game_id}/play_move")
    fun playMove(@PathParam("game_id") gameId : Int, params : PlayMoveParams) : ApiGamestate {
        val gs = dao.getGamestateById(gameId)
        val thrifted = ApiGamestate.toGamestate(gs)

        var mappedTiles : ArrayList<Tile> = ArrayList()
        for (t in params.tiles) {
            mappedTiles.add(ApiTile.toTile(t))
        }
        val played = scClient.playMove(mappedTiles, thrifted)

        return ApiGamestate.fromGamestate(played, gs.id)
    }


    @Timed
    @POST
    @Path("/{game_id}/pass")
    fun passTurn(@PathParam("game_id") gameId : Int) : ApiGamestate {
        val gs = dao.getGamestateById(gameId)
        val thrifted = ApiGamestate.toGamestate(gs)
        val played = scClient.passTurn(thrifted)
        return ApiGamestate.fromGamestate(played, gs.id)
    }


    @Timed
    @GET
    @Path("/{game_id}/suggestions")
    fun getSuggestions(
            @PathParam("game_id") gameId : Int,
            @QueryParam("rack") rack : String
    ) : List<ApiMove> {
        val gs = dao.getGamestateById(gameId)
        val thrifted = ApiGamestate.toGamestate(gs)
        val rawMoves = scClient.getScrabblecheatSuggestions(
                rack,
                thrifted.board,
                thrifted.gameName,
                thrifted.dict)

        var mappedMoves : ArrayList<ApiMove> = ArrayList()
        for (m in rawMoves) {
            mappedMoves.add(ApiMove.fromMove(m))
        }
        return mappedMoves
    }
}
