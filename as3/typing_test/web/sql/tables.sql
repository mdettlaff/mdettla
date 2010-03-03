DROP SCHEMA tt CASCADE;

CREATE SCHEMA tt;

CREATE SEQUENCE tt.id_text_seq;
CREATE TABLE tt.texts (
    id_text int DEFAULT NEXTVAL('tt.id_text_seq') PRIMARY KEY,
    name varchar,
    text varchar NOT NULL
);

CREATE SEQUENCE tt.id_ttlog_seq;
CREATE TABLE tt.ttlog (
    id int DEFAULT NEXTVAL('tt.id_ttlog_seq') PRIMARY KEY,
    date_added timestamp NOT NULL,
    ip varchar(32),
    speed real NOT NULL,
    mistakes int NOT NULL,
    chars int NOT NULL,
    minutes int NOT NULL,
    seconds int NOT NULL,
    corrections int,
    pl varchar
);

CREATE SEQUENCE tt.id_highscore_seq;
CREATE TABLE tt.highscore (
    id_highscore int DEFAULT NEXTVAL('tt.id_highscore_seq') PRIMARY KEY,
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

CREATE SEQUENCE tt.id_log_seq;
CREATE TABLE tt.log (
    id_log int DEFAULT NEXTVAL('tt.id_log_seq') PRIMARY KEY,
    date_added timestamp NOT NULL,
    ip varchar(32),
    message varchar NOT NULL
);
