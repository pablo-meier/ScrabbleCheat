package com.morepaul.scrabblecheat.api

import com.fasterxml.jackson.annotation.JsonProperty

/**
 * For a POST request to create a new game.
 */
data class CreateGameParameters(
        @JsonProperty("names") val names: List<String>,
        @JsonProperty("game_name") val game: Int,
        @JsonProperty("dict") val dict: Int)
