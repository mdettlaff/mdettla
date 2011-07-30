echo -e "qwertyuiop\nasdfghjkl;\nzxcvbnm,.?" \
    | java -cp bin scripts.KeyLayoutGen /tmp/qwerty.png \
    && eog /tmp/qwerty.png

