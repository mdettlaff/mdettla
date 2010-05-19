Autor programu: Michał Dettlaff

1. Krótki opis programu

Gra w kółko i krzyżyk dla dwóch graczy, na dużej planszy.
Kto pierwszy zaznaczy 5 pól swojego koloru w jednej linii (poziomo,
pionowo lub na skos), ten wygrywa.

2. Merytoryczne uzasadnienie wyboru mechanizmu komunikacji międzyprocesowej

Do synchronizacji ruchów graczy została zastosowana para semaforów. Jeden
semafor umożliwiłby wymuszenie wykonywania ruchu tylko przez jednego gracza
na raz, ale wtedy jeden gracz mógłby wykonać wiele ruchów pod rząd, dlatego
potrzebna jest para semaforów. Stan gry (plansza) przechowywany jest
w pamięci współdzielonej, ponieważ jest on wspólny dla obu graczy,
a wspomniane semafory, poprzez wymuszanie naprzemienności ruchów, zapewniają
jednocześnie synchronizację dostępu do pamięci współdzielonej.

3. Opis użytkowania programu

Program można skompilować używając polecenia:
gcc -Wall -lX11 binary_sem.h binary_sem.c tic_tac_toe.c -o tic_tac_toe
Każdy gracz uruchamia swoją własną instancję programu. Kliknięcie na polu
planszy powoduje jego zaznaczenie kolorem gracza.

4. Sytuacje wyjątkowe

Sytuacja: Niedostępny serwer X, lub nieustawiona zmienna środowiskowa DISPLAY.
Zachowanie programu: Wypisanie komunikatu "cannot open display" na stderr
i zakończenie pracy programu z kodem błędu.

Sytuacja: Otrzymanie sygnału SIGINT (wciśnięcie CTRL-C przez użytkownika).
Zachowanie programu: Jeśli obu graczy ma uruchomiony program, to program
który otrzymał sygnał kończy działanie, a drugi program wyświetla komunikat
"wygrana walkowerem". Jeśli tylko jeden gracz ma uruchomiony program, usunięte
zostają semafory oraz pamięć współdzielona, po czym program kończy działanie.

Sytuacja: Trzeci użytkownik próbuje dołączyć się do gry.
Zachowanie programu: Trzeciemu użytkownikowi zostaje wypisany komunikat
"w grze może brać udział co najwyżej dwóch graczy", a jego program zostaje
zakończony (z kodem błędu).

Sytuacja: Zamknięcie okna gry przed zakończeniem rozgrywki.
Zachowanie programu: Program kończy działanie, a drugi program wyświetla
komunikat "wygrana walkowerem".

Sytuacja: Nie można utworzyć/usunąć semaforów lub pamięci współdzielonej.
Zachowanie programu: Wypisanie komunikatu o błędzie na stderr i zakończenie
pracy programu z kodem błędu.

Sytuacja: Kliknięcie przez gracza poza planszą, lub na zajętym polu.
Zachowanie programu: Nieprawidłowe ruchy są ignorowane.

Sytuacja: Wykonanie przez gracza ruchu w czasie kolejki przeciwnika.
Zachowanie programu: Ruchy wykonane w czasie kolejki przeciwnika są ignorowane.

Sytuacja: Nie można wykonać ruchu, bo cała plansza jest zapełniona.
Zachowanie programu: Obu graczom zostaje wyświetlony komunikat "remis".

Uwaga ogólna: program jest zaprojektowany w taki sposób, aby w każdej sytuacji,
również wyjątkowej, na końcu zostały usunięte semafory i pamięć współdzielona.
