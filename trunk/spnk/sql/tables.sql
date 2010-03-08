DROP TABLE guestbook;
DROP SEQUENCE id_guestbook_seq;

CREATE SEQUENCE id_guestbook_seq;
CREATE TABLE guestbook (
    id_guestbook int DEFAULT NEXTVAL('id_guestbook_seq') PRIMARY KEY,
    date_added timestamp NOT NULL,
    ip varchar(32),
    username varchar NOT NULL,
    email varchar,
    content varchar NOT NULL
);
