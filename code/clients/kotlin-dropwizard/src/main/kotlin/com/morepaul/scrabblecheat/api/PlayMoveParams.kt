package com.morepaul.scrabblecheat.api

import com.fasterxml.jackson.annotation.JsonProperty

data class PlayMoveParams(@JsonProperty("tiles") val tiles : List<ApiTile>)
