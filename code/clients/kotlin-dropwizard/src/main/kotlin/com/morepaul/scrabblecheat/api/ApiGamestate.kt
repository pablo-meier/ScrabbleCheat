package com.morepaul.scrabblecheat.api

import com.fasterxml.jackson.annotation.JsonProperty
import com.morepaul.scrabblecheat.external.thriftgenerated.Gamestate
import java.util.*

/**
 * The thrift-generated Gamestate isn't obviously Jackson-able, so this will just
 * be a silly wrapper class that with a factory method.
 */
data class ApiGamestate(
        @JsonProperty("id") val id : Int,
        @JsonProperty("board") val board : List<ApiTile>,
        @JsonProperty("dict") val dictionary : Int,
        @JsonProperty("game_name") val gameName : Int,
        @JsonProperty("history") val history : List<ApiTurn>,
        @JsonProperty("player_turn") val turn : String,
        @JsonProperty("turn_order") val turnOrder : List<String>,
        @JsonProperty("scores") val scores : Map<String, Short>
) {
    companion object {
        @JvmStatic
        fun fromGamestate(g : Gamestate, id : Int) : ApiGamestate {
            var mappedBoard : ArrayList<ApiTile> = ArrayList()
            for (t in g.board) {
                mappedBoard.add(ApiTile.fromTile(t))
            }

            var mappedHistory : ArrayList<ApiTurn> = ArrayList()
            for (t in g.history) {
                mappedHistory.add(ApiTurn.fromTurn(t))
            }


            return ApiGamestate(
                    id,
                    mappedBoard,
                    g.dict.value,
                    g.gameName.value,
                    mappedHistory,
                    g.playerTurn,
                    g.turnOrder,
                    g.scores)
        }
    }
}
