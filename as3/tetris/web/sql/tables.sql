DROP SCHEMA tetris CASCADE;

CREATE SCHEMA tetris;

CREATE SEQUENCE tetris.id_user_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;

CREATE SEQUENCE tetris.id_highscore_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;

CREATE TABLE tetris.users (
    id_user int DEFAULT NEXTVAL('tetris.id_user_seq') PRIMARY KEY,
    username varchar NOT NULL,
    password varchar NOT NULL UNIQUE
);

CREATE TABLE tetris.highscore (
    id_highscore int DEFAULT NEXTVAL('tetris.id_highscore_seq') PRIMARY KEY,
    id_user int REFERENCES tetris.users(id_user),
    score int NOT NULL
);
