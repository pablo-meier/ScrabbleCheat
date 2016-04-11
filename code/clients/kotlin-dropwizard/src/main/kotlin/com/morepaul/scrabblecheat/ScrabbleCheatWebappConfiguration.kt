package com.morepaul.scrabblecheat

import com.fasterxml.jackson.annotation.JsonProperty
import io.dropwizard.Configuration

public class ScrabbleCheatWebappConfiguration() : Configuration() {
    @JsonProperty("template")
    public var template: String=""

    @JsonProperty("defaultName")
    public var defaultName: String="Stranger"
}
