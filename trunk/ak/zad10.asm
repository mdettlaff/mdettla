; program wyswietla liczbe (8 bit) w postaci szestnastkowej i dwojkowej
; liczba jest brana z napisu w postaci dziesietnej
; napis jest brany z parametru (PSP)
; jesli liczba odpowiada literze ASCII, litera ta jest wyswietlana
; jesli nie podano parametrow, wyswietlany jest komunikat

psp_seg	MACRO			; uzyskanie dostepu do parametrow, jesli korzystano z segmentu danych
	push ax
	mov ah,062h		; funkcja wpisuje do bx adres segmentu PSP (Program Segment Prefix)...
	int 21h			; ...czyli tam, gdzie jest linia polecen zapisana
	mov ds,bx		; teraz PSP jest segmentem danych
	pop ax
	ENDM

wyjscie MACRO
        mov ah,4Ch		; wyjscie
        int 21h			; do DOS
	ENDM

ddane	SEGMENT
	EOLN EQU 10,13		; lamanie linii, nowa linia + karetka
	napis1:	DB 'str: $'
	napis2:	DB 'hex: $'
	napis3:	DB 'bin: $'
	napis4:	DB 'Podaj liczbe do konwersji jako parametr programu$'
	EOL:	DB EOLN,'$'	; lamanie linii
ddane	ENDS

rozkazy SEGMENT use16
	ASSUME cs:rozkazy,ds:ddane
        ORG 0

main:   
;====sprawdzamy czy podano parametry====
	mov bx,082h		; wyswietlenie podanego napisu z parametru
	mov al,[bx-1]		; poczatek napisu
	cmp al,0Dh		; jesli nie podano parametrow (jest sam ENTER)
	je niepodano

	mov bx,SEG ddane	; adresowanie posrednie dla
	mov ds,bx		; segmentu danych

	mov bx,offset napis1
	call puts

	psp_seg			; dostajemy segment z parametrami

	mov bx,082h		; wyswietlenie podanego napisu z parametru
	call putln
	call str2num		; konwersja napisu na liczbe do ax

	mov bx,SEG ddane	; adresowanie posrednie dla
	mov ds,bx		; segmentu danych

	mov bx,offset EOL
	call puts

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

	jmp wyjsciedos
niepodano:			; jesli nie podano parametrow, pokaz komunikat
	mov bx,SEG ddane	; adresowanie posrednie dla
	mov ds,bx		; segmentu danych
	mov bx,offset napis4
	call puts
wyjsciedos:
	wyjscie			; wywolanie makra

	include proced.inc	; plik z procedurami
rozkazy ENDS

sstos	SEGMENT stack
	DW 128 dup (?)
sstos	ENDS

END main
