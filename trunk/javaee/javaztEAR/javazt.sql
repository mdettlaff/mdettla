CREATE TABLE PRODUCTS
(ID INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY, 
PRICEZLOTY int, 
PRICEGROSZ int,
TITLE varchar(100),
DESCRIPTION varchar(250));

create table users (ID INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY, 
username varchar(30),
password varchar(30));
insert into users values ('user1','p1');

create table groups (ID INT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
username varchar(30),
groupname varchar(30));
insert into groups values ('user1','registeredUsers');


INSERT INTO PRODUCTS(ID, PRICEZLOTY, PRICEGROSZ, TITLE, DESCRIPTION, AUTHOR, CATEGORY)
VALUES (1, 19, 99, 'Red Dwarf: season I', 'Akcja serialu rozpoczyna sie kilkadziesiat lat w przyszlosci (wymieniany jest rok 2044), w erze lotow kosmicznych na duza skale. Dave Lister i Arnold Rimmer pracuja jako technicy na pokladzie gorniczego statku kosmicznego Czerwony karzel.','Rob Grant, Doug Naylor','movie');
INSERT INTO PRODUCTS(ID, PRICEZLOTY, PRICEGROSZ, TITLE, DESCRIPTION, AUTHOR, CATEGORY)
VALUES (2, 29, 99, 'Gwiezdne Wojny', 'Klasyczny film science fiction.','Geroge Lucas','movie');
INSERT INTO PRODUCTS(ID, PRICEZLOTY, PRICEGROSZ, TITLE, DESCRIPTION, AUTHOR, CATEGORY)
VALUES (3, 24, 99, 'Most na rzece Kwai', 'Japonski oboz jeniecki w Birmie. Jency maja zbudowac most na waznym strategicznie szlaku. Japonczycy traktuja ich w wyjatkowo okrutny sposob. Pulkownik Nicholson jest oficerem angielskim, ktory chce zachowac swoja godnosc.','David Lean','movie');
INSERT INTO PRODUCTS(ID, PRICEZLOTY, PRICEGROSZ, TITLE, DESCRIPTION, AUTHOR, CATEGORY)
VALUES (4, 100, 00, 'Thinking in Java', 'Ksiazka stanowiaca znakomite wprowadzenie do programowania w jezyku Java.','Bruce Eckel','books');
INSERT INTO PRODUCTS(ID, PRICEZLOTY, PRICEGROSZ, TITLE, DESCRIPTION, AUTHOR, CATEGORY)
VALUES (5, 35, 00, 'Jak sie szybko wzbogacic', 'Poradnik dla kazdego.','John Bickenbauer','books');
INSERT INTO PRODUCTS(ID, PRICEZLOTY, PRICEGROSZ, TITLE, DESCRIPTION, AUTHOR, CATEGORY)
VALUES (6, 25, 99, 'V Symfonia Beethovena', 'Jedna z najslynniejszych symfonii niemieckiego kompozytora.','Herbert von Karajan','music');
INSERT INTO PRODUCTS(ID, PRICEZLOTY, PRICEGROSZ, TITLE, DESCRIPTION, AUTHOR, CATEGORY)
VALUES (7, 44, 99, 'The Cosmos Rocks', 'Nowa plyta Queen wydana po 15 latach przerwy z legendarnym wokalista zespolow Free i Bad Company - Paulem Rodgersem.','Queen + Paul Rodgers','music');
INSERT INTO PRODUCTS(ID, PRICEZLOTY, PRICEGROSZ, TITLE, DESCRIPTION, AUTHOR, CATEGORY)
VALUES (8, 16, 50, 'Dzwieki oceanu', 'Doznaj wszechogarniajacego uczucia relaksu sluchajac piesni wielorybow i odglosow krabow antakrtycznych.','New Age Music','music');
INSERT INTO PRODUCTS(ID, PRICEZLOTY, PRICEGROSZ, TITLE, DESCRIPTION, AUTHOR, CATEGORY)
VALUES (9, 22, 00, 'Chinese Democracy', 'Najnowsza plyta kultowego zespolu Guns n Roses. Na gitarze mial wystapic Brian May, ale w koncu zdecydowano sie nie wykorzystywac jego nagran.','Guns n Roses','music');