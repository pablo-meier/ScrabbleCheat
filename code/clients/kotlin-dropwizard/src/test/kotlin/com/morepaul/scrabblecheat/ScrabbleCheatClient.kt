package com.morepaul.scrabblecheat

import com.morepaul.scrabblecheat.api.ApiGamestate
import com.morepaul.scrabblecheat.api.ApiMove
import com.morepaul.scrabblecheat.api.CreateGameParameters
import com.morepaul.scrabblecheat.api.PlayMoveParams
import retrofit2.Call
import retrofit2.http.*

/**
 * Client to be Retrofit-ed for integration tests.
 */
interface ScrabbleCheatClient {

    @GET("games")
    fun listGames() : Call<List<ApiGamestate>>

    @GET("games/{gameId}")
    fun getGame(@Path("gameId") gameId : Int) : Call<ApiGamestate>

    @POST("games")
    fun createGame(@Body params : CreateGameParameters) : Call<ApiGamestate>

    @POST("games/{gameId}/play_move")
    fun playMove(@Path("gameId") gameId : Int, @Body params : PlayMoveParams) : Call<ApiGamestate>

    @POST("games/{gameId}/pass")
    fun pass(@Path("gameId") gameId : Int) : Call<ApiGamestate>

    @POST("games/{gameId}/suggestions")
    fun suggestions(@Path("gameId") gameId : Int, @Query("rack") rack : String) : Call<List<ApiMove>>
}