package com.morepaul.scrabblecheat.jdbi;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.morepaul.scrabblecheat.ScrabbleCheatWebappApplication;
import org.postgresql.util.PGobject;
import org.skife.jdbi.v2.SQLStatement;
import org.skife.jdbi.v2.sqlobject.Binder;
import org.skife.jdbi.v2.sqlobject.BinderFactory;
import org.skife.jdbi.v2.sqlobject.BindingAnnotation;

import java.lang.annotation.*;
import java.sql.SQLException;

/**
 * Makes it possible to bind gamestate JSON to something Postgres can store in
 * JSONB. Thanks to this blog post for initial inspiration:
 *
 * http://blog.anorakgirl.co.uk/2016/01/using-jdbi-with-postgres-json-data/
 */
@BindingAnnotation(BindJson.JsonBinderFactory.class)
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.PARAMETER})
public @interface BindJson {
    String value();

    class JsonBinderFactory implements BinderFactory {
        static ObjectMapper MAPPER = ScrabbleCheatWebappApplication.objectMapper();

        @Override
        public Binder build(Annotation annotation) {
            return new Binder<BindJson, Object>() {
                @Override
                public void bind(SQLStatement q, BindJson bind, Object object) {
                    try {
                        PGobject data = new PGobject();
                        data.setType("jsonb");
                        String asJson = MAPPER.writeValueAsString(object);
                        data.setValue(asJson);
                        q.bind(bind.value(), data);
                    } catch (SQLException|JsonProcessingException e) {
                        throw new IllegalStateException("Error binding JSON: ", e);
                    }
                }
            };
        }
    }
}
