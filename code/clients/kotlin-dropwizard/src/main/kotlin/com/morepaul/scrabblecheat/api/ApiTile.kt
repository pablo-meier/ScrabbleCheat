package com.morepaul.scrabblecheat.api

import com.fasterxml.jackson.annotation.JsonProperty
import com.morepaul.scrabblecheat.external.thriftgenerated.Bonus
import com.morepaul.scrabblecheat.external.thriftgenerated.LetterType
import com.morepaul.scrabblecheat.external.thriftgenerated.Tile

data class ApiTile (
        @JsonProperty("row") val row : Byte,
        @JsonProperty("col") val col : Byte,
        @JsonProperty("letter") val value : String,
        @JsonProperty("bonus") val bonus : Int,
        @JsonProperty("type") val letterType : Int
) {
    companion object {
        @JvmStatic
        fun fromTile(t : Tile) : ApiTile {
            return ApiTile(t.row, t.col, t.letter, t.bonus.value, t.type.value)
        }

        @JvmStatic
        fun toTile(t : ApiTile) : Tile {
            return Tile(t.row, t.col, LetterType.findByValue(t.letterType), t.value, Bonus.findByValue(t.bonus))
        }
    }
}
