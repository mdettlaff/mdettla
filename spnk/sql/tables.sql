CREATE TABLE guestbook (
    id_guestbook int NOT NULL auto_increment,
    date_added datetime DEFAULT '0000-00-00 00:00:00' NOT NULL,
    ip varchar(32),
    username varchar(32) NOT NULL,
    email varchar(128),
    content text NOT NULL,
    PRIMARY KEY(id_guestbook)
);
