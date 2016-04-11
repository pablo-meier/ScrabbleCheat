package com.morepaul.scrabblecheat.api

import com.fasterxml.jackson.annotation.JsonProperty
import com.morepaul.scrabblecheat.external.thriftgenerated.Turn

data class ApiTurn(
        @JsonProperty("player") val player : String,
        @JsonProperty("move") val move : ApiMove
) {
    companion object {
        @JvmStatic
        fun fromTurn(t : Turn) : ApiTurn {
            return ApiTurn(t.player, ApiMove.fromMove(t.move))
        }
    }
}
