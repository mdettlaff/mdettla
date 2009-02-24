; program gra dzwiek o zadanej czestotliwosci

sndinit	MACRO			; inicjacja PC speakera
	push ax
	mov al,10110110b
	out 043h,al
	pop ax
	ENDM

note	MACRO	FREQUENCY	; generuje ton o czestotliwosci FREQUENCY
	push ax
	push bx
	push dx
	mov dx,012h		; czestotliwosc bazowa to 1193180 Hz, czyli 01234DCh Hz
	mov ax,034DCh		; wpisujemy czestotliwosc bazowa do DX:AX
	mov bx,FREQUENCY
	div bx			; uzyskujemy dzielnik danej czestotliwosci
	out 042h,al		; wysylamy LSB dzielnika do zegara CTC
	mov al,ah		; teraz w al jest MSB dzielnika
	out 042h,al		; wysylamy MSB dzielnika do zegara CTC
	in al,061h
	or al,00000011b		; ustawiamy dwa ostatnie bity, ktore zamykaja obwod
	out 061h,al		; wlaczamy dzwiek
	pop dx
	pop bx
	pop ax
	ENDM

delay	MACRO	MSB,LSB		; pauza dlugosci MSB:LSB mikrosekund; 0F4240h to 1 sekunda
	push ax
	push cx
	push dx
	mov cx,MSB
	mov dx,LSB
	mov ah,86h
	int 15h			; pauza o dlugosci CX:DX mikrosekund
	pop dx
	pop cx
	pop ax
	ENDM

nosound	MACRO			; wylaczamy dzwiek
	push ax
	in al,061h
	and al,11111100b	; zerujemy dwa ostatnie bity
	out 061h,al
	pop ax
	ENDM

wyjscie	MACRO			; wyjscie do DOS
	mov ah,04Ch
	int 21h
	ENDM

ddane	SEGMENT
	opis:	DB 'Podaj czestotliwosc dzwieku (w Hz) jako parametr$'
	opis2:	DB 'Opcja off wylacza dzwiek$'
	blad1:	DB 'Czestotliwosc nie moze byc mniejsza niz 20 Hz$'
	blad2:	DB 'Czestotliwosc nie moze byc wieksza niz 22 kHz$'
	blad3:	DB 'Nieprawidlowy parametr$'
ddane	ENDS


rozkazy	SEGMENT use16
	ASSUME cs:rozkazy,ds:ddane
	ORG 0

main:
;=======SPRAWDZANIE PARAMETROW================================================
	mov bx,082h		; adres napisu z parametru
	mov al,[bx-1]
	cmp al,0Dh		; czy nie podano zadnego parametru
	je opisopcji
;=======sprawdzanie czy podano opcje off=======
	mov al,[bx]
	cmp al,'o'
	jne tone
	mov al,[bx+1]
	cmp al,'f'
	jne tone
	mov al,[bx+2]
	cmp al,'f'
	jne tone
	mov al,[bx+3]
	cmp al,0Dh
	jne tone
	jmp toneoff

;=======WYLACZA DZWIEK========================================================
toneoff:
	nosound
	jmp wyjsciedos

;=======WYSWIETLANIE OPISU===============================================
opisopcji:
	mov bx,SEG ddane	; adresowanie posrednie dla
	mov ds,bx		; segmentu danych
	mov bx,offset opis
	call puts

	sndinit
	note 1000
	delay 0Fh,4240h
	nosound

	jmp wyjsciedos

;=======WLACZA DZWIEK O CZESTOTLIWOSCI PODANEJ Z PARAMETRU====================
tone:
	mov bx,082h
;=======sprawdzanie czy podano liczbe=======
sprczyl:
	mov al,[bx]
	cmp al,0Dh
	je endsprczyl
	cmp al,'0'
	jl nieliczba
	cmp al,'9'
	jg nieliczba
	inc bx
	jmp sprczyl
endsprczyl:
;===========================================
	mov bx,082h
	call str2num
	cmp ax,20		; czestotliwosc dzwieku musi byc wieksza od 20 Hz
	jl zamalac
	cmp ax,22000		; czestotliwosc dzwieku musi byc mniejsza od 22 kHz
	jg zaduzac
	mov bx,SEG ddane	; adresowanie posrednie dla
	mov ds,bx		; segmentu danych
	mov bx,offset opis2
	call puts
	mov bx,ax		; str2num wpisala podana liczbe do ax
	sndinit
	note bx
	jmp wyjsciedos

nieliczba:
	mov bx,SEG ddane	; adresowanie posrednie dla
	mov ds,bx		; segmentu danych
	mov bx,offset blad3
	call puts
	jmp wyjsciedos

zamalac:
	mov bx,SEG ddane	; adresowanie posrednie dla
	mov ds,bx		; segmentu danych
	mov bx,offset blad1
	call puts
	jmp wyjsciedos

zaduzac:
	mov bx,SEG ddane	; adresowanie posrednie dla
	mov ds,bx		; segmentu danych
	mov bx,offset blad2
	call puts

wyjsciedos:
	wyjscie			; wywolanie makra, wyjscie do DOS

	include proced.inc

rozkazy	ENDS

sstos	SEGMENT stack
	DW 128 dup (?)
sstos	ENDS

END main
