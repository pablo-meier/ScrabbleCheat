package com.morepaul.scrabblecheat.api

import io.dropwizard.testing.FixtureHelpers.fixture
import org.assertj.core.api.Assertions.assertThat
import com.fasterxml.jackson.databind.ObjectMapper
import com.google.common.collect.ImmutableMap
import com.google.common.collect.Lists
import com.morepaul.scrabblecheat.ScrabbleCheatWebappApplication
import org.junit.Test

/**
 * Ahhh painnnnnn
 */
class ApiGamestateTest {

    val MAPPER : ObjectMapper = ScrabbleCheatWebappApplication.configureObjectMapper(ObjectMapper())

    @Test
    fun deserializesBasicCase() {
        val expected = basicGamestate()
        val valueRead : ApiGamestate = MAPPER.readValue(fixture("fixtures/gamestate1.json"), ApiGamestate::class.java)
        assertThat(valueRead).isEqualTo(expected)
    }


    @Test
    fun serializeBasicCase() {
        val basic = basicGamestate()
        val expected = MAPPER.writeValueAsString(
                MAPPER.readValue(fixture("fixtures/gamestate1.json"), ApiGamestate::class.java))
        assertThat(MAPPER.writeValueAsString(basic)).isEqualTo(expected)
    }

    private fun basicGamestate() : ApiGamestate {
        return ApiGamestate(
                1,                    // id
                board(),
                0,                    // TWL06 dict
                0,                    // Scrabble
                Lists.newArrayList(), // Empty history
                "pablo",              // my turn
                Lists.newArrayList("pablo", "sapo"),
                ImmutableMap.Builder<String, Short>()
                        .put("pablo", 0)
                        .put("sapo", 0)
                        .build()
        )
    }


    fun board() : List<ApiTile> {
       return Lists.newArrayList(
               ApiTile( 1,  1, "", 0, 2),
               ApiTile( 1,  2, "", 4, 2),
               ApiTile( 1,  3, "", 4, 2),
               ApiTile( 1,  4, "", 3, 2),
               ApiTile( 1,  5, "", 4, 2),
               ApiTile( 1,  6, "", 4, 2),
               ApiTile( 1,  7, "", 4, 2),
               ApiTile( 1,  8, "", 0, 2),
               ApiTile( 1,  9, "", 4, 2),
               ApiTile( 1, 10, "", 4, 2),
               ApiTile( 1, 11, "", 4, 2),
               ApiTile( 1, 12, "", 3, 2),
               ApiTile( 1, 13, "", 4, 2),
               ApiTile( 1, 14, "", 4, 2),
               ApiTile( 1, 15, "", 0, 2),

               ApiTile( 2,  1, "", 4, 2),
               ApiTile( 2,  2, "", 1, 2),
               ApiTile( 2,  3, "", 4, 2),
               ApiTile( 2,  4, "", 4, 2),
               ApiTile( 2,  5, "", 4, 2),
               ApiTile( 2,  6, "", 2, 2),
               ApiTile( 2,  7, "", 4, 2),
               ApiTile( 2,  8, "", 4, 2),
               ApiTile( 2,  9, "", 4, 2),
               ApiTile( 2, 10, "", 2, 2),
               ApiTile( 2, 11, "", 4, 2),
               ApiTile( 2, 12, "", 4, 2),
               ApiTile( 2, 13, "", 4, 2),
               ApiTile( 2, 14, "", 1, 2),
               ApiTile( 2, 15, "", 4, 2),

               ApiTile( 3,  1, "", 4, 2),
               ApiTile( 3,  2, "", 4, 2),
               ApiTile( 3,  3, "", 1, 2),
               ApiTile( 3,  4, "", 4, 2),
               ApiTile( 3,  5, "", 4, 2),
               ApiTile( 3,  6, "", 4, 2),
               ApiTile( 3,  7, "", 3, 2),
               ApiTile( 3,  8, "", 4, 2),
               ApiTile( 3,  9, "", 3, 2),
               ApiTile( 3, 10, "", 4, 2),
               ApiTile( 3, 11, "", 4, 2),
               ApiTile( 3, 12, "", 4, 2),
               ApiTile( 3, 13, "", 1, 2),
               ApiTile( 3, 14, "", 4, 2),
               ApiTile( 3, 15, "", 4, 2),

               ApiTile( 4,  1, "", 3, 2),
               ApiTile( 4,  2, "", 4, 2),
               ApiTile( 4,  3, "", 4, 2),
               ApiTile( 4,  4, "", 1, 2),
               ApiTile( 4,  5, "", 4, 2),
               ApiTile( 4,  6, "", 4, 2),
               ApiTile( 4,  7, "", 4, 2),
               ApiTile( 4,  8, "", 3, 2),
               ApiTile( 4,  9, "", 4, 2),
               ApiTile( 4, 10, "", 4, 2),
               ApiTile( 4, 11, "", 4, 2),
               ApiTile( 4, 12, "", 1, 2),
               ApiTile( 4, 13, "", 4, 2),
               ApiTile( 4, 14, "", 4, 2),
               ApiTile( 4, 15, "", 3, 2),

               ApiTile( 5,  1, "", 4, 2),
               ApiTile( 5,  2, "", 4, 2),
               ApiTile( 5,  3, "", 4, 2),
               ApiTile( 5,  4, "", 4, 2),
               ApiTile( 5,  5, "", 1, 2),
               ApiTile( 5,  6, "", 4, 2),
               ApiTile( 5,  7, "", 4, 2),
               ApiTile( 5,  8, "", 4, 2),
               ApiTile( 5,  9, "", 4, 2),
               ApiTile( 5, 10, "", 4, 2),
               ApiTile( 5, 11, "", 1, 2),
               ApiTile( 5, 12, "", 4, 2),
               ApiTile( 5, 13, "", 4, 2),
               ApiTile( 5, 14, "", 4, 2),
               ApiTile( 5, 15, "", 4, 2),

               ApiTile( 6,  1, "", 4, 2),
               ApiTile( 6,  2, "", 2, 2),
               ApiTile( 6,  3, "", 4, 2),
               ApiTile( 6,  4, "", 4, 2),
               ApiTile( 6,  5, "", 4, 2),
               ApiTile( 6,  6, "", 2, 2),
               ApiTile( 6,  7, "", 4, 2),
               ApiTile( 6,  8, "", 4, 2),
               ApiTile( 6,  9, "", 4, 2),
               ApiTile( 6, 10, "", 2, 2),
               ApiTile( 6, 11, "", 4, 2),
               ApiTile( 6, 12, "", 4, 2),
               ApiTile( 6, 13, "", 4, 2),
               ApiTile( 6, 14, "", 2, 2),
               ApiTile( 6, 15, "", 4, 2),

               ApiTile( 7,  1, "", 4, 2),
               ApiTile( 7,  2, "", 4, 2),
               ApiTile( 7,  3, "", 3, 2),
               ApiTile( 7,  4, "", 4, 2),
               ApiTile( 7,  5, "", 4, 2),
               ApiTile( 7,  6, "", 4, 2),
               ApiTile( 7,  7, "", 3, 2),
               ApiTile( 7,  8, "", 4, 2),
               ApiTile( 7,  9, "", 3, 2),
               ApiTile( 7, 10, "", 4, 2),
               ApiTile( 7, 11, "", 4, 2),
               ApiTile( 7, 12, "", 4, 2),
               ApiTile( 7, 13, "", 3, 2),
               ApiTile( 7, 14, "", 4, 2),
               ApiTile( 7, 15, "", 4, 2),

               ApiTile( 8,  1, "", 0, 2),
               ApiTile( 8,  2, "", 4, 2),
               ApiTile( 8,  3, "", 4, 2),
               ApiTile( 8,  4, "", 3, 2),
               ApiTile( 8,  5, "", 4, 2),
               ApiTile( 8,  6, "", 4, 2),
               ApiTile( 8,  7, "", 4, 2),
               ApiTile( 8,  8, "", 1, 2),
               ApiTile( 8,  9, "", 4, 2),
               ApiTile( 8, 10, "", 4, 2),
               ApiTile( 8, 11, "", 4, 2),
               ApiTile( 8, 12, "", 3, 2),
               ApiTile( 8, 13, "", 4, 2),
               ApiTile( 8, 14, "", 4, 2),
               ApiTile( 8, 15, "", 0, 2),

               ApiTile( 9,  1, "", 4, 2),
               ApiTile( 9,  2, "", 4, 2),
               ApiTile( 9,  3, "", 3, 2),
               ApiTile( 9,  4, "", 4, 2),
               ApiTile( 9,  5, "", 4, 2),
               ApiTile( 9,  6, "", 4, 2),
               ApiTile( 9,  7, "", 3, 2),
               ApiTile( 9,  8, "", 4, 2),
               ApiTile( 9,  9, "", 3, 2),
               ApiTile( 9, 10, "", 4, 2),
               ApiTile( 9, 11, "", 4, 2),
               ApiTile( 9, 12, "", 4, 2),
               ApiTile( 9, 13, "", 3, 2),
               ApiTile( 9, 14, "", 4, 2),
               ApiTile( 9, 15, "", 4, 2),

               ApiTile(10,  1, "", 4, 2),
               ApiTile(10,  2, "", 2, 2),
               ApiTile(10,  3, "", 4, 2),
               ApiTile(10,  4, "", 4, 2),
               ApiTile(10,  5, "", 4, 2),
               ApiTile(10,  6, "", 2, 2),
               ApiTile(10,  7, "", 4, 2),
               ApiTile(10,  8, "", 4, 2),
               ApiTile(10,  9, "", 4, 2),
               ApiTile(10, 10, "", 2, 2),
               ApiTile(10, 11, "", 4, 2),
               ApiTile(10, 12, "", 4, 2),
               ApiTile(10, 13, "", 4, 2),
               ApiTile(10, 14, "", 2, 2),
               ApiTile(10, 15, "", 4, 2),

               ApiTile(11,  1, "", 4, 2),
               ApiTile(11,  2, "", 4, 2),
               ApiTile(11,  3, "", 4, 2),
               ApiTile(11,  4, "", 4, 2),
               ApiTile(11,  5, "", 1, 2),
               ApiTile(11,  6, "", 4, 2),
               ApiTile(11,  7, "", 4, 2),
               ApiTile(11,  8, "", 4, 2),
               ApiTile(11,  9, "", 4, 2),
               ApiTile(11, 10, "", 4, 2),
               ApiTile(11, 11, "", 1, 2),
               ApiTile(11, 12, "", 4, 2),
               ApiTile(11, 13, "", 4, 2),
               ApiTile(11, 14, "", 4, 2),
               ApiTile(11, 15, "", 4, 2),

               ApiTile(12,  1, "", 3, 2),
               ApiTile(12,  2, "", 4, 2),
               ApiTile(12,  3, "", 4, 2),
               ApiTile(12,  4, "", 1, 2),
               ApiTile(12,  5, "", 4, 2),
               ApiTile(12,  6, "", 4, 2),
               ApiTile(12,  7, "", 4, 2),
               ApiTile(12,  8, "", 3, 2),
               ApiTile(12,  9, "", 4, 2),
               ApiTile(12, 10, "", 4, 2),
               ApiTile(12, 11, "", 4, 2),
               ApiTile(12, 12, "", 1, 2),
               ApiTile(12, 13, "", 4, 2),
               ApiTile(12, 14, "", 4, 2),
               ApiTile(12, 15, "", 3, 2),

               ApiTile(13,  1, "", 4, 2),
               ApiTile(13,  2, "", 4, 2),
               ApiTile(13,  3, "", 1, 2),
               ApiTile(13,  4, "", 4, 2),
               ApiTile(13,  5, "", 4, 2),
               ApiTile(13,  6, "", 4, 2),
               ApiTile(13,  7, "", 3, 2),
               ApiTile(13,  8, "", 4, 2),
               ApiTile(13,  9, "", 3, 2),
               ApiTile(13, 10, "", 4, 2),
               ApiTile(13, 11, "", 4, 2),
               ApiTile(13, 12, "", 4, 2),
               ApiTile(13, 13, "", 1, 2),
               ApiTile(13, 14, "", 4, 2),
               ApiTile(13, 15, "", 4, 2),

               ApiTile(14,  1, "", 4, 2),
               ApiTile(14,  2, "", 1, 2),
               ApiTile(14,  3, "", 4, 2),
               ApiTile(14,  4, "", 4, 2),
               ApiTile(14,  5, "", 4, 2),
               ApiTile(14,  6, "", 2, 2),
               ApiTile(14,  7, "", 4, 2),
               ApiTile(14,  8, "", 4, 2),
               ApiTile(14,  9, "", 4, 2),
               ApiTile(14, 10, "", 2, 2),
               ApiTile(14, 11, "", 4, 2),
               ApiTile(14, 12, "", 4, 2),
               ApiTile(14, 13, "", 4, 2),
               ApiTile(14, 14, "", 1, 2),
               ApiTile(14, 15, "", 4, 2),

               ApiTile(15,  1, "", 0, 2),
               ApiTile(15,  2, "", 4, 2),
               ApiTile(15,  3, "", 4, 2),
               ApiTile(15,  4, "", 3, 2),
               ApiTile(15,  5, "", 4, 2),
               ApiTile(15,  6, "", 4, 2),
               ApiTile(15,  7, "", 4, 2),
               ApiTile(15,  8, "", 0, 2),
               ApiTile(15,  9, "", 4, 2),
               ApiTile(15, 10, "", 4, 2),
               ApiTile(15, 11, "", 4, 2),
               ApiTile(15, 12, "", 3, 2),
               ApiTile(15, 13, "", 4, 2),
               ApiTile(15, 14, "", 4, 2),
               ApiTile(15, 15, "", 0, 2)
       )
    }
}
