DROP SCHEMA tetris CASCADE;

CREATE SCHEMA tetris;

CREATE SEQUENCE tetris.id_user_seq
  START WITH 1
  INCREMENT BY 1
  NO MAXVALUE
  NO MINVALUE
  CACHE 1;

CREATE TABLE tetris.users (
  id_user int DEFAULT nextval('tetris.id_user_seq') PRIMARY KEY,
  username varchar NOT NULL,
  password varchar NOT NULL UNIQUE
);
