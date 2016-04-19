package com.morepaul.scrabblecheat.api

import com.fasterxml.jackson.annotation.JsonProperty
import com.morepaul.scrabblecheat.ScrabbleCheatWebappApplication
import com.morepaul.scrabblecheat.external.thriftgenerated.*
import com.morepaul.scrabblecheat.external.thriftgenerated.Dictionary
import org.skife.jdbi.v2.StatementContext
import org.skife.jdbi.v2.tweak.ResultSetMapper
import java.sql.ResultSet
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

        @JvmStatic
        fun toGamestate(g : ApiGamestate) : Gamestate {
            var mappedBoard : ArrayList<Tile> = ArrayList()
            for (t in g.board) {
                mappedBoard.add(ApiTile.toTile(t))
            }

            var mappedHistory : ArrayList<Turn> = ArrayList()
            for (t in g.history) {
                mappedHistory.add(ApiTurn.toTurn(t))
            }

           return Gamestate(
                   mappedBoard,
                   g.scores,
                   g.turn,
                   g.turnOrder,
                   mappedHistory,
                   GameName.findByValue(g.gameName),
                   Dictionary.findByValue(g.dictionary))
        }

        class ApiGamestateMapper : ResultSetMapper<ApiGamestate> {
            override fun map(index: Int, r: ResultSet?, ctx: StatementContext?): ApiGamestate? {
                val asString : String = r!!.getString("gamestate");
                val mapper = ScrabbleCheatWebappApplication.objectMapper;
                val gs : ApiGamestate? = mapper.readValue(asString, ApiGamestate::class.java)
                return gs
            }
        }
    }
}
