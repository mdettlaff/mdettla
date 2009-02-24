; program rysuje wykres funkcji sinus w trybie graficznym 640x480 (12h)
; do obliczania sinusa wykorzystany jest koprocesor arytmetyczny (FPU)

.486

SCREEN_WIDTH	EQU	640
SCREEN_HEIGHT	EQU	480

rozkazy	SEGMENT	use16
	ASSUME cs:rozkazy,ss:stosseg
	ORG 0

; dane musza byc na poczatku segmentu rozkazow, inaczej nie dziala (dlaczego?)
; liczby do obliczen na koprocesorze musza byc co najmniej 32-bitowe
	x		dd 	?	; pozycja x na ekranie
	dy		dd 	?	; odleglosc od osi x
	halfscrwidth	dd	320	; polowa szerokosci ekranu
	halfscrheight	dd	239	; polowa wysokosci ekranu

main:
	call tryb_graficzny
	call ustaw_segment_grafiki
	call rysuj_osie
	call rysuj_sinus
	call czekaj_na_klawisz
	call tryb_tekstowy
	call wyjscie_do_dos


rysuj_sinus	PROC
	push eax
	push ecx
	push edx
	mov ecx,SCREEN_WIDTH	; zaczynamy rysowac wykres od prawej strony
rysuj_punkt_wykresu:
	mov eax,ecx
	mov x,eax		; zapisujemy x=ax do zmiennej x
	mov edx,SCREEN_HEIGHT/2
	; teraz wykonamy na koprocesorze obliczenie
	;   dy=hsh*sin((ax-hsw)/hsw*pi)
	finit			; zerujemy FPU (koprocesor)
	fild x			; wczytujemy zmienna na stos koprocesora
	fild halfscrwidth	; wczytujemy zmienna na stos koprocesora
				; FPU: hsw, x
	fsubp			; FPU: x-hsw
	fild halfscrwidth	; wczytujemy zmienna na stos koprocesora
	fdivp			; FPU: (x-hsw)/hsw
	fldpi			; wczytaj liczbe pi na stos koprocesora
	fmulp			; FPU: (x-hsw)/hsw*pi
	fsin			; FPU: sin((x-hsw)/hsw*pi)
	fild halfscrheight	; wczytujemy zmienna na stos koprocesora
	fmulp			; FPU: hsh*sin((x-hsw)/hsw*pi)

	fistp dy		; zczytujemy integer z koprocesora, FPU: 
	fwait
	sub edx,dy		; y=y+dy, bo dx oznacza wspolrzedna y

	call putpixel
	loop rysuj_punkt_wykresu

	finit			; zerujemy FPU (koprocesor)
	pop edx
	pop ecx
	pop eax
	ret
rysuj_sinus	ENDP


putpixel	PROC		; wyswietla piksel w punkcie x=ax, y=dx
	push ax
	push bx
	push cx
	push dx
	push ax
	mov bx,80		; trzeba pomnozyc dx razy 80 ( szerokosc linii w bajtach)
	mov ax,dx
	mul bx			; dx:ax=ax*bx
	mov dx,ax
	pop ax

	xor bx,bx
	mov cx,8		; podzielimy ax przez 8, zeby uzyskac pozycje bajtu
	div cl			; teraz w al jest pozycja x bajtu, a w ah pozycja piksela 0..7 w bajcie
	mov bl,al		; w bx przechowujemy pozycje bajta do ktorego dodamy nasz piksel
	add bx,dx		; teraz w bx faktycznie jest pozycja tego bajtu
; ustawiamy pozycje piksela w bajcie
	mov cl,ah		
	mov ah,128		; 1000 0000 jedynka oznaczajaca piksel jest z lewej
	shr ah,cl		; teraz przesuniemy ja w prawa
; ========================
	mov al,[bx]		; pobieramy oryginalne 8 pikseli z ekranu, z tego wlasnie bajtu
	or al,ah		; dodajemy nasz nowy piksel
	mov [bx],al		; wysylamy gotowy bajt na ekran
	pop dx
	pop cx
	pop bx
	pop ax
	ret
putpixel	ENDP

rysuj_osie	PROC
	push ax
	push cx
	push dx
	mov cx,SCREEN_WIDTH
	mov dx,SCREEN_HEIGHT/2
rysuj_os_x:
	mov ax,cx
	call putpixel
	loop rysuj_os_x
	mov cx,SCREEN_HEIGHT
	mov ax,SCREEN_WIDTH/2
rysuj_os_y:
	mov dx,cx
	call putpixel
	loop rysuj_os_y
	pop dx
	pop cx
	pop ax
	ret
rysuj_osie	ENDP

ustaw_segment_grafiki	PROC
	push bx
	mov bx,0A000h		; adresowanie posrednie dla
	mov ds,bx		; segmentu danych
	pop bx
	ret
ustaw_segment_grafiki	ENDP

tryb_graficzny	PROC		; ekran ma szerokosc 80 bajtow
	mov al,12h		; tryb graficzny 640x480 czarno-bialy
	mov ah,00h		; ustaw tryb ekranu z al
	int 10h			; wykonaj funkcje z ah
	ret
tryb_graficzny	ENDP

tryb_tekstowy	PROC
	mov al,03h		; tryb tekstowy 80x25
	mov ah,00h		; ustaw tryb ekranu z al
	int 10h			; wykonaj funkcje z ah
	ret
tryb_tekstowy	ENDP

czekaj_na_klawisz	PROC
	mov ah,08h
	int 21h
	ret
czekaj_na_klawisz	ENDP

wyjscie_do_dos	PROC
        mov ah,4Ch
        int 21h
wyjscie_do_dos	ENDP


rozkazy	ENDS

stosseg	SEGMENT stack
	DB 256 dup (?)
stosseg	ENDS

END main
