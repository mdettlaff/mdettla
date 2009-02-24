#!/bin/bash

###############################################################################
# Skrypt ściągający komiksy z Internetu i wyświetlający je                    #
# autor: Michał Dettlaff                                                      #
###############################################################################

path='/tmp/comics'
ilosc=0
if [ ! -d $path/ ]
then
  mkdir $path/
fi
cd $path/
if [ $(ls -1A $path/ | wc -l) -ne 0 ]; then
  rm $path/*
fi
echo -n "Pobieranie: "


# Dilbert
((ilosc++))
echo -n "Dilbert"
wget -q http://www.dilbert.com/fast/
adres=$( grep '^<img src="' index.html \
    | awk '{ gsub(/.*?img src="/, "http://www.dilbert.com"); print }' \
    | awk '{ gsub(/gif.*?/, "gif"); print }' )
wget -q $adres
rm $path/index.html*

# xkcd
((ilosc++))
echo -n ", xkcd"
wget -q http://xkcd.com
adres=$( grep 'Image URL' index.html \
    | awk '{ gsub(/.*?http/, "http"); print }' \
    | awk '{ gsub(/png.*?/, "png"); print }' )
wget -q $adres
rm $path/index.html*

# User Friendly
((ilosc++))
echo -n ", User Friendly"
wget -q http://www.userfriendly.org
adres=$( grep 'Latest Strip' index.html \
    | awk '{ gsub(/.*?Latest.*?http/, "http"); print }' \
    | awk '{ gsub(/gif.*?/, "gif"); print }' )
wget -q $adres
rm $path/index.html*

# Freefall
((ilosc++))
echo -n ", Freefall"
wget -q http://freefall.purrsia.com
adres=$( grep 'png' index.html \
    | awk '{ gsub(/.*?HREF="/, "http://freefall.purrsia.com"); print }' \
    | awk '{ gsub(/"><img.*?/, ""); print }' )
wget -q $adres
rm $path/index.html*

# Questionable Content
((ilosc++))
echo -n ", Questionable Content"
wget -q http://questionablecontent.net
adres=$( grep 'questionablecontent.net\/comics' index.html \
    | awk '{ gsub(/.*?src="/, ""); print }' \
    | awk '{ gsub(/">.*?/, ""); print }' )
wget -q $adres
rm $path/index.html*

echo

# PHD Comics
((ilosc++))
echo -n "PHD Comics"
wget -q http://www.phdcomics.com/comics.php
adres=$( grep 'Link to Piled' comics.php \
    | awk '{ gsub(/.*?http/, "http"); print }' \
    | awk '{ gsub(/gif.*?/, "gif"); print }' )
wget -q $adres
rm $path/comics.php

# Cectic
((ilosc++))
echo -n ", Cectic"
wget -q http://cectic.com
adres=$( grep 'id="comic"' index.html \
    | awk '{ gsub(/.*?src="/, "http://cectic.com/"); print }' \
    | awk '{ gsub(/" \/>.*?/, ""); print }' )
wget -q $adres
rm $path/index.html*

# Jesus and Mo
((ilosc++))
echo -n ", Jesus and Mo"
wget -q http://www.jesusandmo.net
adres=$( grep 'alt="Comic"' index.html \
    | awk '{ gsub(/.*?src="/, ""); print }' \
    | awk '{ gsub(/" alt=.*?/, ""); print }' )
wget -q $adres
rm $path/index.html*

# Cyanide and Happiness
((ilosc++))
echo -n ", Cyanide and Happiness"
wget -q http://www.explosm.net/comics
adres=$( grep 'http://www.explosm.net/db/files/Comics' index.html \
    | awk '{ gsub(/.*?<img alt="Cyanide and Happiness, a daily webcomic" src="/, ""); print }' \
    | awk '{ gsub(/"><\/div>.*?/, ""); print }' )
wget -q $adres
rm $path/index.html*

# Full Frontal Nerdity
((ilosc++))
echo -n ", Full Frontal Nerdity"
wget -q http://nodwick.humor.gamespy.com/ffn
adres=$( grep 'ffn' index.html \
    | awk '{ gsub(/.*?src="/, ""); print }' \
    | awk '{ gsub(/jpg">.*?/, "jpg"); print }' )
wget -q $adres
rm $path/index.html*

echo

# Thinking Ape Blues
((ilosc++))
echo -n "Thinking Ape Blues"
wget -q http://thinkingapeblues.com
adres=$( grep '7"><img src="' index.html \
    | awk '{ gsub(/.*?img src="/, "http://thinkingapeblues.com/"); print }' \
    | awk '{ gsub(/" width=.*?/, ""); print }' )
wget -q $adres
rm $path/index.html*

# Monty
((ilosc++))
echo -n ", Monty"
wget -q http://www.comics.com/comics/monty
adres=$( grep 'ALT="Today' index.html \
    | awk '{ gsub(/.*?IMG SRC="/, "http://www.comics.com"); print }' \
    | awk '{ gsub(/" ALT="Today.*?/, ""); print }' )
wget -q $adres
rm $path/index.html*

# Pearls Before Swine
((ilosc++))
echo -n ", Pearls Before Swine"
wget -q http://www.comics.com/comics/pearls
adres=$( grep 'ALT="Today' index.html \
    | awk '{ gsub(/.*?IMG SRC="/, "http://www.comics.com"); print }' \
    | awk '{ gsub(/" ALT="Today.*?/, ""); print }' )
wget -q $adres
rm $path/index.html*

# Get Fuzzy
((ilosc++))
echo -n ", Get Fuzzy"
wget -q http://www.comics.com/comics/getfuzzy
adres=$( grep 'ALT="Today' index.html \
    | awk '{ gsub(/.*?IMG SRC="/, "http://www.comics.com"); print }' \
    | awk '{ gsub(/" ALT="Today.*?/, ""); print }' )
wget -q $adres
rm $path/index.html*

# Garfield
((ilosc++))
echo -n ", Garfield"
wget -q http://www.gocomics.com/garfield
adres=$( grep 'var linuxContent' index.html \
    | awk '{ gsub(/.*?img src="/, ""); print }' \
    | awk '{ gsub(/" width=.*?/, ""); print }' )
wget -q $adres
rm $path/index.html*

# FoxTrot
((ilosc++))
echo -n ", FoxTrot"
wget -q http://www.gocomics.com/foxtrot
adres=$( grep 'var linuxContent' index.html \
    | awk '{ gsub(/.*?img src="/, ""); print }' \
    | awk '{ gsub(/" width=.*?/, ""); print }' )
wget -q $adres
rm $path/index.html*

echo

# Ronaldinho Gaucho
((ilosc++))
echo -n "Ronaldinho Gaucho"
wget -q http://www.gocomics.com/ronaldinhogaucho
adres=$( grep '<center><img src ="' index.html \
    | awk '{ gsub(/.*?img src ="/, ""); print }' \
    | awk '{ gsub(/" width=.*?/, ""); print }' )
wget -q $adres
rm $path/index.html*

# Cowbirds in Love
((ilosc++))
echo -n ", Cowbirds in Love"
wget -q http://cowbird.110mb.com
adres=$( grep -A5 'Latest update' index.html | tail -1 \
    | awk '{ gsub(/.*?href=/, ""); print }' \
    | awk '{ gsub(/>.*?/, ""); print }' )
rm $path/index.html*
wget -q $adres
adres=$( grep '<img src="' *.html \
    | awk '{ gsub(/.*?img src="/, "http://cowbird.110mb.com/"); print }' \
    | awk '{ gsub(/" title=.*?/, ""); print }' )
wget -q $adres
rm $path/*.html*


echo
echo "Zakończono pobieranie `ls $path/ | wc -w ` z $ilosc plików"
echo "Komiksy zostały zapisane w katalogu $path/"

# przeglądamy komiksy za pomocą Eye Of GNOME
#eog $path/*
