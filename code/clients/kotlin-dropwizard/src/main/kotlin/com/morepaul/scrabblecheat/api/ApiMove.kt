package com.morepaul.scrabblecheat.api

import com.fasterxml.jackson.annotation.JsonProperty
import com.morepaul.scrabblecheat.external.thriftgenerated.Move
import com.morepaul.scrabblecheat.external.thriftgenerated.Tile
import java.util.*

data class ApiMove(
        @JsonProperty("score") val score : Short,
        @JsonProperty("move") val move : List<ApiTile>
) {
    companion object {
        @JvmStatic
        fun fromMove(m : Move) : ApiMove {
            var tileList : ArrayList<ApiTile> = ArrayList()
            for (t in m.move) {
                tileList.add(ApiTile.fromTile(t))
            }

            return ApiMove(m.score, tileList)
        }

        @JvmStatic
        fun toMove(m : ApiMove) : Move {
            var tileList : ArrayList<Tile> = ArrayList()
            for (t in m.move) {
                tileList.add(ApiTile.toTile(t))
            }
            return Move(tileList, m.score)
        }
    }
}
