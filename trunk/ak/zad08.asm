; program wyswietla liczbe (16 bit) w postaci dziesietnej
; liczba jest brana z napisu w postaci szesnastkowej
; napis jest podawany z klawiatury

wyjscie MACRO
        mov ah,4Ch		; wyjscie
        int 21h			; do DOS
	ENDM

ddane	SEGMENT
	EOLN EQU 10,13		; lamanie linii, nowa linia + karetka
	liczbahex: DB 8 dup ('$')	; liczba do podania (jako hex) do konwersji
	napis0:	DB 'Podaj liczbe w systemie szesnastkowym (16 bit):$'
 	napis1:	DB 'str: $'
	napis2:	DB 'dec: $'
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

	mov bx,offset liczbahex	; podanie napisu z klawiatury
	call readstr

	mov bx,offset napis1
	call puts
	mov bx,offset liczbahex	; wyswietlanie podanego napisu
	call puts
	mov bx,offset EOL
	call puts

	mov bx,offset napis2
	call puts
	mov bx,offset liczbahex
	call strhex2num		; konwersja napisu (podanego jako hex) na liczbe do ax

	call putdec		; wyswietla liczbe z ax w postaci dziesietnej

	wyjscie			; wywolanie makra

	include proced.inc	; plik z procedurami
rozkazy ENDS

sstos	SEGMENT stack
	DW 128 dup (?)
sstos	ENDS

END main
