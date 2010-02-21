DROP SCHEMA tt CASCADE;

CREATE SCHEMA tt;

CREATE SEQUENCE tt.id_text_seq;
CREATE SEQUENCE tt.id_ttlog_seq;
CREATE SEQUENCE tt.id_user_seq;

CREATE TABLE tt.texts (
    id_text int DEFAULT NEXTVAL('tt.id_text_seq') PRIMARY KEY,
    name varchar,
    text varchar NOT NULL
);

CREATE TABLE tt.ttlog (
    id int DEFAULT NEXTVAL('tt.id_ttlog_seq') PRIMARY KEY,
    date_added timestamp NOT NULL,
    ip varchar(32),
    speed real NOT NULL,
    mistakes int NOT NULL,
    chars int NOT NULL,
    minutes int NOT NULL,
    seconds int NOT NULL,
    pl boolean,
    id_user int
);

CREATE TABLE tt.users (
    id_user int DEFAULT NEXTVAL('tt.id_user_seq') PRIMARY KEY,
    username varchar NOT NULL
);
