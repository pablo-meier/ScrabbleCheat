package com.morepaul.scrabblecheat

import com.fasterxml.jackson.module.kotlin.KotlinModule
import com.morepaul.scrabblecheat.core.ManagedThriftTransport
import com.morepaul.scrabblecheat.external.thriftgenerated.ScrabbleCheat
import com.morepaul.scrabblecheat.resources.GamesResource
import io.dropwizard.Application
import io.dropwizard.setup.Bootstrap
import io.dropwizard.setup.Environment
import org.apache.thrift.protocol.TBinaryProtocol
import org.apache.thrift.protocol.TProtocol
import org.apache.thrift.transport.TSocket
import org.apache.thrift.transport.TTransport

class ScrabbleCheatWebappApplication() : Application<ScrabbleCheatWebappConfiguration>() {

    val SCRABBLECHEAT_PORT = 8888;

    companion object {
        @JvmStatic
        fun main(args: Array<String>) {
            ScrabbleCheatWebappApplication().run(*args)
        }
    }

    override fun initialize(bootstrap: Bootstrap<ScrabbleCheatWebappConfiguration>) {
        // Don't do anything
    }

    override fun run(config: ScrabbleCheatWebappConfiguration, env: Environment) {
        env.objectMapper.registerModule(KotlinModule())

        val gamesResource = GamesResource(provideScClient(env))

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
