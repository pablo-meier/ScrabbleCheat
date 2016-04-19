package com.morepaul.scrabblecheat.jdbi

import com.morepaul.scrabblecheat.api.ApiGamestate
import org.skife.jdbi.v2.sqlobject.Bind
import org.skife.jdbi.v2.sqlobject.SqlQuery
import org.skife.jdbi.v2.sqlobject.SqlUpdate
import org.skife.jdbi.v2.sqlobject.customizers.RegisterMapper

/**
 * DAO for the Gamestates/Games resource
 */
@RegisterMapper(ApiGamestate.Companion.ApiGamestateMapper::class)
abstract class GamestateDAO {

    @SqlQuery("SELECT gamestate FROM sc.gamestates WHERE id = :id")
    abstract fun getGamestateById(@Bind("id") id : Int) : ApiGamestate

    @SqlQuery("SELECT gamestate FROM sc.gamestates ORDER BY updated_at LIMIT 50")
    abstract fun getGamestates() : List<ApiGamestate>

    @SqlUpdate("INSERT INTO sc.gamestates (id, gamestate, created_at, updated_at) "
    + "VALUES(:id, :gs, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)")
    abstract fun insertGamestate(
            @Bind("id") id : Int,
            @BindJson("gs") gs : ApiGamestate) : Int


    // FIXME THIS IS BAD
    @SqlQuery("SELECT COALESCE(MAX(id), 0) FROM sc.gamestates")
    abstract fun getHighestId() : Int
}
