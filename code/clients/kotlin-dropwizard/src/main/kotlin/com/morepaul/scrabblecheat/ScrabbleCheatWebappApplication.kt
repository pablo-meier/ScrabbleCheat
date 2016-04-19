package com.morepaul.scrabblecheat

import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.databind.DeserializationFeature
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.SerializationFeature
import com.fasterxml.jackson.datatype.guava.GuavaModule
import com.fasterxml.jackson.module.kotlin.KotlinModule
import com.morepaul.scrabblecheat.core.ManagedThriftTransport
import com.morepaul.scrabblecheat.external.thriftgenerated.ScrabbleCheat
import com.morepaul.scrabblecheat.jdbi.GamestateDAO
import com.morepaul.scrabblecheat.resources.GamesResource
import io.dropwizard.Application
import io.dropwizard.db.PooledDataSourceFactory
import io.dropwizard.jdbi.DBIFactory
import io.dropwizard.migrations.MigrationsBundle
import io.dropwizard.setup.Bootstrap
import io.dropwizard.setup.Environment
import org.apache.thrift.protocol.TBinaryProtocol
import org.apache.thrift.protocol.TProtocol
import org.apache.thrift.transport.TSocket
import org.apache.thrift.transport.TTransport
import org.skife.jdbi.v2.DBI

class ScrabbleCheatWebappApplication() : Application<ScrabbleCheatWebappConfiguration>() {

    val SCRABBLECHEAT_PORT = 8888;

    companion object {
        @JvmStatic
        var objectMapper : ObjectMapper = ObjectMapper();

        @JvmStatic
        fun objectMapper() : ObjectMapper {
            return objectMapper;
        }

        @JvmStatic
        fun objectMapper(o : ObjectMapper) {
            objectMapper = o;
        }

        @JvmStatic
        fun main(args: Array<String>) {
            ScrabbleCheatWebappApplication().run(*args)
        }

        @JvmStatic
        fun configureObjectMapper(mapper : ObjectMapper) : ObjectMapper {
            mapper.registerModule(KotlinModule())
                    .registerModule(GuavaModule())
                    .configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false)
                    .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, true)
                    .setSerializationInclusion(JsonInclude.Include.NON_ABSENT);

            return mapper
        }
    }

    override fun initialize(bootstrap: Bootstrap<ScrabbleCheatWebappConfiguration>) {
        bootstrap.addBundle(object : MigrationsBundle<ScrabbleCheatWebappConfiguration>() {
            override fun getDataSourceFactory(configuration: ScrabbleCheatWebappConfiguration?): PooledDataSourceFactory? {
                return configuration?.dataSourceFactory
            }
        })
    }

    override fun run(config: ScrabbleCheatWebappConfiguration, env: Environment) {
        objectMapper(configureObjectMapper(env.objectMapper))

        val factory : DBIFactory = DBIFactory()
        val jdbi : DBI = factory.build(env, config.dataSourceFactory, "postgresql")
        val dao : GamestateDAO = jdbi.onDemand(GamestateDAO::class.java)

        val gamesResource = GamesResource(provideScClient(env), dao)

        env.jersey().register(gamesResource)
    }



    /*
     * Until I can get DI going, this will be a poor man's Factory method.
     */
    fun provideScClient(env : Environment) : ScrabbleCheat.Client {
        val transport : TTransport = TSocket("localhost", SCRABBLECHEAT_PORT)
        env.lifecycle().manage(ManagedThriftTransport(transport))
        val protocol : TProtocol = TBinaryProtocol(transport)
        return ScrabbleCheat.Client(protocol)
    }
}
