CREATE TABLE texts (
    id_text int NOT NULL auto_increment,
    name varchar(128),
    text text NOT NULL,
    PRIMARY KEY(id_text)
);

CREATE TABLE ttlog (
    id int NOT NULL auto_increment,
    date_added datetime DEFAULT '0000-00-00 00:00:00' NOT NULL,
    ip varchar(32),
    speed real NOT NULL,
    mistakes int NOT NULL,
    chars int NOT NULL,
    minutes int NOT NULL,
    seconds int NOT NULL,
    pl varchar(8),
    PRIMARY KEY(id)
);

CREATE TABLE highscore (
    id_highscore int NOT NULL auto_increment,
    username varchar(64) NOT NULL,
    date_added datetime DEFAULT '0000-00-00 00:00:00' NOT NULL,
    ip varchar(32),
    speed real NOT NULL,
    mistakes int NOT NULL,
    corrections int NOT NULL,
    chars int NOT NULL,
    minutes int NOT NULL,
    seconds int NOT NULL,
    pl varchar(8),
    PRIMARY KEY(id_highscore)
);

CREATE TABLE log (
    id_log int NOT NULL auto_increment,
    date_added datetime DEFAULT '0000-00-00 00:00:00' NOT NULL,
    ip varchar(32),
    message text NOT NULL,
    PRIMARY KEY(id_log)
);
