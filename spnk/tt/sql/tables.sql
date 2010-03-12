DROP TABLE texts;
DROP SEQUENCE id_text_seq;
DROP TABLE ttlog;
DROP SEQUENCE id_ttlog_seq;
DROP TABLE highscore;
DROP SEQUENCE id_highscore_seq;
DROP TABLE log;
DROP SEQUENCE id_log_seq;

CREATE SEQUENCE id_text_seq;
CREATE TABLE texts (
    id_text int DEFAULT NEXTVAL('id_text_seq') PRIMARY KEY,
    name varchar,
    text varchar NOT NULL
);

CREATE SEQUENCE id_ttlog_seq;
CREATE TABLE ttlog (
    id int DEFAULT NEXTVAL('id_ttlog_seq') PRIMARY KEY,
    date_added timestamp NOT NULL,
    ip varchar(32),
    speed real NOT NULL,
    mistakes int NOT NULL,
    chars int NOT NULL,
    minutes int NOT NULL,
    seconds int NOT NULL,
    pl varchar
);

CREATE SEQUENCE id_highscore_seq;
CREATE TABLE highscore (
    id_highscore int DEFAULT NEXTVAL('id_highscore_seq') PRIMARY KEY,
    username varchar NOT NULL,
    date_added timestamp NOT NULL,
    ip varchar(32),
    speed real NOT NULL,
    mistakes int NOT NULL,
    corrections int NOT NULL,
    chars int NOT NULL,
    minutes int NOT NULL,
    seconds int NOT NULL,
    pl varchar
);

CREATE SEQUENCE id_log_seq;
CREATE TABLE log (
    id_log int DEFAULT NEXTVAL('id_log_seq') PRIMARY KEY,
    date_added timestamp NOT NULL,
    ip varchar(32),
    message varchar NOT NULL
);
