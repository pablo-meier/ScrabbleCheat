package com.morepaul.scrabblecheat

import com.fasterxml.jackson.annotation.JsonProperty
import io.dropwizard.Configuration
import io.dropwizard.db.DataSourceFactory
import javax.validation.Valid
import javax.validation.constraints.NotNull

public class ScrabbleCheatWebappConfiguration() : Configuration() {

    @Valid
    @NotNull
    @JsonProperty("database")
    public var dataSourceFactory : DataSourceFactory = DataSourceFactory()


}
