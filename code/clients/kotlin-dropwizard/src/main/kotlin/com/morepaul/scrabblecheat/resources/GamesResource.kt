package com.morepaul.scrabblecheat.resources

import com.codahale.metrics.annotation.Timed
import com.google.common.collect.Lists
import com.morepaul.scrabblecheat.api.ApiGamestate
import com.morepaul.scrabblecheat.api.CreateGameParameters
import com.morepaul.scrabblecheat.external.thriftgenerated.Dictionary
import com.morepaul.scrabblecheat.external.thriftgenerated.GameName
import com.morepaul.scrabblecheat.external.thriftgenerated.ScrabbleCheat
import javax.validation.Valid
import javax.validation.constraints.NotNull
import javax.ws.rs.*
import javax.ws.rs.core.MediaType

/**
 * Basic toplevel games resource.
 */
@Path("/games")
@Produces(MediaType.APPLICATION_JSON)
class GamesResource(val scClient :ScrabbleCheat.Client) {


    @Timed
    @GET
    @Path("/{game_id}")
    fun getGame(@PathParam("game_id") gameId : Int) : ApiGamestate {
        val gs = scClient.newGame(Lists.newArrayList("Pablo", "Sapo"), GameName.SCRABBLE, Dictionary.TWL06)
        return ApiGamestate.fromGamestate(gs, gameId)
    }

    @Timed
    @POST
    fun createGame(@NotNull @Valid params : CreateGameParameters) : ApiGamestate {
        val gs = scClient.newGame(
                params.names,
                GameName.findByValue(params.game),
                Dictionary.findByValue(params.dict))

        return ApiGamestate.fromGamestate(gs, 1)
    }
}
