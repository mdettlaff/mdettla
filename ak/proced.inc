puts	PROC			; wyswietla napis na ktory wskazuje bx
	push ax			; odkladamy ax na stos, zeby go nie zepsuc dzialaniem procedury
	push bx
znak:				; petla do wyswietlania napisu
	mov al,[bx]
	and al,al		; NULL oznacza...
	jz end_znak		; ...koniec napisu
	cmp al,'$'		; $ oznacza...
	je end_znak		; ...koniec napisu
	call putchar
	inc bx
	jmp znak
end_znak:
	pop bx
	pop ax
	ret
puts	ENDP


putln	PROC			; wyswietla pierwsza linie napisu na ktory wskazuje bx
	push ax			; odkladamy ax na stos, zeby go nie zepsuc dzialaniem procedury
	push bx
znak1:				; petla do wyswietlania napisu
	mov al,[bx]
	and al,al		; NULL oznacza...
	jz end_znak1		; ...koniec napisu
	cmp al,0Dh		; ENTER oznacza...
	je end_znak1		; ...koniec napisu
	call putchar
	inc bx
	jmp znak1
end_znak1:
	pop bx
	pop ax
	ret
putln	ENDP


putchar PROC
	push ax
	mov ah,0Eh
	int 10h
	pop ax
	ret
putchar ENDP


readstr	PROC			; wczytuje podany napis do adresu wskazywanego przez bx
	push ax
	push bx
litera:
	mov ah,01h		; odczyt znaku z klawiatury, do al kod ASCII znaku
	int 21h
	cmp al,0Dh		; czy koniec podawania (enter)
	je wczytano
	mov byte PTR [bx],al	; przeslij wczytany znak do tablicy
	inc bx
	jmp litera
wczytano:
	mov al,'$'		; zaznacz koniec napisu
	mov byte PTR [bx],al
	pop bx
	pop ax
	ret
readstr	ENDP


str2num	PROC			; konwersja napisu wskazywanego przez bx (decymalna) na liczbe do ax
	push bx
	push dx
	xor ax,ax		; zerujemy ax
	xor dx,dx
licz_dl:			; policz dlugosc napisu w bx i zapisz do dx
	mov al,[bx]		; al jest kolejnymi znakami napisu
	and al,al		; czy al=0?
	jz koniec_ld		; jesli al=0 to koniec liczenia dlugosci
	cmp al,'$'		; $ oznacza...
	je koniec_ld		; ...koniec napisu; mozna zamiast je uzyc jz, bo to to samo
	cmp al,0Dh		; ENTER oznacza...
	je koniec_ld		; ...koniec napisu; mozna zamiast je uzyc jz, bo to to samo
	inc bx
	inc dx
	jmp licz_dl
koniec_ld:
	mov ax,dx		; ax bedzie licznikiem wykonan petli licz_cyfre
	xor dx,dx		; zmienna pomocnicza do zapamietania liczby
	mov cx,1		; mnoznik potegi
licz_cyfre:			; dodawaj cyfry zaczynajac od najmniej znaczacej
	and ax,ax
	jz koniec_lc
	dec bx
	dec ax
	push ax
	mov al,[bx]
	sub al,'0'		; zamien znak ASCII na cyfre
	call mnoz		; mnozenie ax=ax*cx; mozna tez zastosowac 'mul cl'
;====mnozenie cx=cx*10====
	push ax
	mov ax,cx
	mov cx,10
	call mnoz		; mozna tez zastosowac 'mul cl'
	mov cx,ax
	pop ax
;=========================
	add dx,ax
	pop ax
	jmp licz_cyfre
koniec_lc:
	mov ax,dx		; wlasciwa liczba skonwertowana byla przechowywana w dx
	pop dx
	pop bx
	ret
str2num	ENDP


strhex2num PROC			; konwersja napisu wskazwanego przez bx (hex) na liczbe do ax
	push dx
licz_dl1:			; policz dlugosc napisu i zapisz w bx
	mov al,[bx]		; al jest kolejnymi znakami napisu
	and al,al		; czy al=0?
	jz koniec_ld1		; jesli al=0 to koniec liczenia dlugosci
	cmp al,'$'		; $ oznacza...
	je koniec_ld1		; ...koniec napisu
	inc bx
	jmp licz_dl1
koniec_ld1:
	xor dx,dx		; zmienna pomocnicza do zapamietania liczby
	and ax,00FFh		; kasujemy ah
	mov cx,1		; mnoznik potegi
licz_cyfre1:			; dodawaj cyfry zaczynajac od najmniej znaczacej
	and bx,bx
	jz koniec_lc1
	dec bx
	mov al,[bx]
;====zamiana ASCII na cyfre====
	cmp al,'9'		; w mniej niz 58 sa cyfry
	jle mniej1		; jump less equal - jesli mniejsze rowne
	sub al,7
mniej1:
	sub al,'0'
;==============================
	call mnoz		; mnozenie ax=ax*cx
	shl cx,4		; mnozenie cx=cx*16
	add dx,ax
	jmp licz_cyfre1
koniec_lc1:
	mov ax,dx		; wlasciwa liczba skonwertowana byla przechowywana w dx
	pop dx
	ret
strhex2num ENDP


puthex	PROC			; wyswietla zawartosc al w postaci heksadecymalnej
	push ax			; odkladamy ax na stos, zeby go nie zepsuc dzialaniem procedury
	push bx
	mov cx,2		; licznik petli
	mov bl,al		; zapamietujemy al, bo sie skasuja bity w przesunieciu bitowym
	shr al,4		; przesuniecie bitowe w prawo
petla_putdigit:			; petla do wyswietlania cyfry hex (0..F)
	push ax			; po konwersji na ASCII chcemy odzyskac wartosc oryg. rejestru
	cmp al,10
	jl mniej		; jump less - jesli mniejsze
	add al,7		; teraz al jest litera ASCII (przesuniecie o 55 w sumie)
mniej:
	add al,'0'		; teraz al jest cyfra ASCII (przesuniecie o 48)
	call putchar
	pop ax
	mov al,bl		; odzyskujemy al sprzed przesuniecia bitowego
	and al,0Fh		; kasujemy cztery najbardziej znaczace bity
	loop petla_putdigit
	pop bx
	pop ax
	ret
puthex	ENDP


putbin	PROC			; wyswietla zawartosc al w postaci dwojkowej
	push ax
	push bx
	mov bl,al		; zapamietujemy al, bo go potem nadpiszemy do wypisywania zer i jedynek
	mov cx,8		; licznik powtorzen; bierzemy liczbe jednobajtowa
binpetla:
	mov al,'1'		; '1' do wypisania
	shl bl,1		; konwertujemy liczbe z rejestru bl
	jc pisz1		; napisz 1, nie zero, kiedy po przesunieciu przeniesiona jedynka
	mov al,'0'		; '0' do wypisania
pisz1:
	call putchar
	loop binpetla
	pop bx
	pop ax
	ret
putbin	ENDP


putdec PROC			; wyswietla zawartosc ax w postaci dziesietnej
; ax nie moze byc wieksze niz 32759 / 7FF7h (sprawdz dlaczego)
	push dx
	push cx
	push bx
	push ax
	xor bx,bx		; bx bedzie licznikiem wykonan petli
	mov cx,10
odloz:				; w tej petli odlozymy na stos kolejno wszystkie cyfry od konca
	call dziel		; otrzymamy kolejne cyfry (od konca) w resztach dzielac przez 10
	inc bx
	push dx			; odkladamy reszte na stos, zeby zdjac w petli "zdejmij"
	and ax,ax
	jz end_odloz		; jesli ax=0, skoncz petle
	jmp odloz
end_odloz:
	mov cx,bx		; bedziemy powtarzac petle tyle razy ile poprzednia
zdejmij:			; wyswietla kolejno odlozone reszty
	pop ax			; odlozona reszte z dx zdejmujemy i przypisujemy na ax
	add al,'0'
	call putchar
	loop zdejmij
	pop ax
	pop bx
	pop cx
	pop dx
	ret
putdec ENDP


mnoz	PROC			; procedura mnozy rejestr ax razy cx
	push cx
	push bx			; odkladamy bx, zeby go nie zepsuc dzialaniem procedury
	mov bx,ax		; zapamietanie jaka liczbe mnozyc
	xor ax,ax
dodawanie:
	add ax,bx
	loop dodawanie
	pop bx
	pop cx
	ret
mnoz	ENDP


dziel	PROC			; dzielenie ax/cl; ax = ax div cl; dx = ax mod cl
; ax nie moze byc wieksze niz 32766 (sprawdz dlaczego)
	and cl,cl		; czy cl=0?
	jz blad_dpz		; blad: dzielenie przez zero
	push cx
	push bx
	xor ch,ch		; odrzucamy, bo dzielnik tylko w cl
	mov dx,ax		; teraz dx i ax zamieniaja sie miejscami, dx staje sie dzielna...
	xor ax,ax		; ...a ax sugerowanym wynikiem
	xor bx,bx		; liczba do porownywania
czydziel:			; sprawdzamy w tej petli po kolei czy bx jest dzielnikiem
	add bx,cx
	cmp bx,dx
	jg end_czydziel
	inc ax
	jmp czydziel
end_czydziel:
	sub bx,cx		; do obliczenia reszty
	sub dx,bx		; teraz w dx jest reszta
	pop bx
	pop cx
blad_dpz:			; blad: dzielenie przez zero
	ret
dziel	ENDP


is_lett	PROC			; jesli liczba w al odpowiada literze ASCII, to wyswietla ta litere
	push cx			; w dx trzeba podac segment kodu
	push ax
	push bx
	mov cx,ds		; zapisujemy aktualny segment do tymczasowego rejestru
	mov ds,dx		; bierzemy napisy z segmentu kodu
	pop bx
	cmp al,'A'		; litery ASCII maja kody pomiedzy 65 i 122
	jl pomin
	cmp al,'z'
	jg pomin
	mov bx,offset schar	; wypisanie 'chr: '
	call puts
	call putchar		; wypisanie litery
	mov bx,offset lnend	; lamanie linii
	call puts
pomin:
	mov ds,cx		; odzyskujemy segment danych
	pop ax
	pop cx
	ret
schar:	DB 'chr: $'		; dane wykorzystywane przez procedure
lnend:	DB 10,13,'$'		; dane wykorzystywane przez procedure
is_lett	ENDP

