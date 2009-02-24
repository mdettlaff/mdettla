; program wyswietla liczbe (8 bit) w postaci szestnastkowej i dwojkowej
; liczba jest brana z napisu w postaci dziesietnej
; napis jest podawany z klawiatury
; jesli liczba odpowiada literze ASCII, litera ta jest wyswietlana

wyjscie MACRO
        mov ah,4Ch		; wyjscie
        int 21h			; do DOS
	ENDM

ddane	SEGMENT
	EOLN EQU 10,13		; lamanie linii, nowa linia + karetka
	liczba: DB 8 dup ('$')	; liczba do podania do konwersji
	napis0:	DB 'Podaj liczbe (8 bit):$'
	napis1:	DB 'str: $'
	napis2:	DB 'hex: $'
	napis3:	DB 'bin: $'
	EOL:	DB EOLN,'$'	; lamanie linii
ddane	ENDS

rozkazy SEGMENT use16
	ASSUME cs:rozkazy,ds:ddane
        ORG 0

main:   
	mov ax,SEG ddane	; adresowanie posrednie dla
	mov ds,ax		; segmentu danych

	mov bx,offset napis0	; prosba o podanie napisu
	call puts
	mov bx,offset EOL
	call puts

	mov bx,offset liczba	; podanie napisu z klawiatury
	call readstr

	mov bx,offset napis1
	call puts
	mov bx,offset liczba	; wyswietlenie podanego napisu
	call puts

	mov bx,offset EOL
	call puts

	mov bx,offset liczba
	call str2num		; konwersja napisu na liczbe

	mov dx,seg rozkazy	; procedura is_lett wymaga podania w dx segmentu kodu
	call is_lett		; wyswietla znak ASCII o kodzie z al, jesli to litera

	mov bx,offset napis2
	call puts

	call puthex

	mov bx,offset EOL
	call puts
	mov bx,offset napis3
	call puts

	call putbin

	wyjscie			; wywolanie makra

	include proced.inc	; plik z procedurami
rozkazy ENDS

sstos	SEGMENT stack
	DW 128 dup (?)
sstos	ENDS

END main
