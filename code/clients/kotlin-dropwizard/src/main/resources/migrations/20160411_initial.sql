CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE SCHEMA sc;

CREATE TABLE sc.gamestates (
    id int PRIMARY KEY NOT NULL,
    gamestate jsonb NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL
);

