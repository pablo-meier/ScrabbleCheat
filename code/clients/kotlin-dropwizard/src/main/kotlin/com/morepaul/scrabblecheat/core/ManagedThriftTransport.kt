package com.morepaul.scrabblecheat.core

import io.dropwizard.lifecycle.Managed
import org.apache.thrift.transport.TTransport

/**
 * The "simple" transport needs to be opened and closed with the app, so
 * we'll wrap it in a Managed instance.
 */
class ManagedThriftTransport(val transport : TTransport) : Managed {
    override fun start() {
        transport.open()
    }

    override fun stop() {
        transport.close()
    }
}
