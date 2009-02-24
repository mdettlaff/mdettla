; program rysuje na ekranie okrag w trybie graficznym 640x480 (12h)
; funkcje trygonometryczne i inne oblicza koprocesor arytmetyczny (FPU)

.486

SCREEN_WIDTH	EQU	640
SCREEN_HEIGHT	EQU	480

rozkazy	SEGMENT	use16
	ASSUME cs:rozkazy,ss:stosseg
	ORG 0

; dane musza byc na poczatku segmentu rozkazow, inaczej nie dziala (dlaczego?)
; liczby do obliczen na koprocesorze musza byc co najmniej 32-bitowe
	kat		dd 	?
	x		dd 	?
	y		dd 	?
	r		dd 	200	; promien kola
	pix		dd	2000	; ilosc pikseli tworzacych okrag
	dwa		dd	2
	halfscrwidth	dd	320	; polowa szerokosci ekranu
	halfscrheight	dd	240	; polowa wysokosci ekranu

main:
	call tryb_graficzny
	call ustaw_segment_grafiki
	call rysuj_okrag
	call czekaj_na_klawisz
	call tryb_tekstowy
	call wyjscie_do_dos


rysuj_okrag	PROC
	push eax
	push ecx
	push edx
	mov ecx,pix		; rysujemy kolejne piksele dla kolejnych katow
rysuj_punkt_okregu:
	mov kat,ecx

	finit			; zerujemy FPU (koprocesor)
	; obliczamy wspolrzedna x=hsw+r*cos(kat/pix*2*pi)
	fild kat		; wczytujemy zmienna na stos koprocesora
	fild pix		; FPU: pix, kat
	fdivp			; FPU: kat/pix
	fldpi			; FPU: pi, kat/pix
	fild dwa
	fmulp
	fmulp			; FPU: kat/pix*2*pi
	fcos			; FPU: cos(kat/pix*2*pi)
	fild r
	fmulp			; FPU: r*cos(kat/pix*2*pi)
	fild halfscrwidth
	faddp			; FPU: hsw+r*cos(kat/pix*2*pi)
	fistp x			; mamy wspolrzedna x

	; obliczamy wspolrzedna y=hsh+r*sin(kat/pix*2*pi)
	fild kat		; wczytujemy zmienna na stos koprocesora
	fild pix		; FPU: pix, kat
	fdivp			; FPU: kat/pix
	fldpi			; FPU: pi, kat/pix
	fild dwa
	fmulp
	fmulp			; FPU: kat/pix*2*pi
	fsin			; FPU: sin(kat/pix*2*pi)
	fild r
	fmulp			; FPU: r*sin(kat/pix*2*pi)
	fild halfscrheight
	faddp			; FPU: hsh+r*sin(kat/pix*2*pi)
	fistp y			; mamy wspolrzedna y

	fwait
	mov eax,x
	mov edx,y
	call putpixel
	loop rysuj_punkt_okregu

	finit			; zerujemy FPU (koprocesor)
	pop edx
	pop ecx
	pop eax
	ret
rysuj_okrag	ENDP

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
