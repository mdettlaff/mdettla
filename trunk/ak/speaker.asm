; program gra wybrana melodie PC speakerem
; do wyboru: Tetris, Red Dwarf, Silent night lub sygnal alarmowy

;=======DEFINICJE CZESTOTLIWOSCI NUT==========================================
	C0	EQU	16
	C1	EQU	33
	C2	EQU	65

	C3	EQU	131
	Cis3	EQU	139
	D3	EQU	147
	Dis3	EQU	155
	E3	EQU	165
	F3	EQU	175
	Fis3	EQU	185
	G3	EQU	196
	Gis3	EQU	208
	A3	EQU	220
	Ais3	EQU	233
	B3	EQU	245

	C4	EQU	262
	Cis4	EQU	277
	D4	EQU	294
	Dis4	EQU	311
	E4	EQU	330
	F4	EQU	349
	Fis4	EQU	370
	G4	EQU	392
	Gis4	EQU	415
	A4	EQU	440
	Ais4	EQU	466
	B4	EQU	494

	C5	EQU	523
	Cis5	EQU	554
	D5	EQU	587
	Dis5	EQU	622
	E5	EQU	659
	F5	EQU	698
	Fis5	EQU	740
	G5	EQU	784
	Gis5	EQU	831
	A5	EQU	880
	Ais5	EQU	932
	B5	EQU	988

	C6	EQU	1046
	Cis6	EQU	1109
	D6	EQU	1175
	Dis6	EQU	1244
	E6	EQU	1328
	F6	EQU	1397
	Fis6	EQU	1480
	G6	EQU	1568
	Gis6	EQU	1661
	A6	EQU	1760
	Ais6	EQU	1865
	B6	EQU	1975

	C7	EQU	2093
	Cis7	EQU	2217
	C8	EQU	4186

;=======DEFINICJE DLUGOSCI NUT================================================
;=======TEMPO 160 BPM=========================================================
	WNH	EQU	16h	; cala nuta (MSB)
	WNL	EQU	0E360h	; cala nuta (LSB)
	HNH	EQU	0Bh	; polnuta
	HNL	EQU	71B0h
	QNH	EQU	5h	; cwiercnuta
	QNL	EQU	0B8D8h
	ENH	EQU	2h	; osemka
	ENL	EQU	0DC6Ch
	SNH	EQU	1h	; szesnastka
	SNL	EQU	06E36h

;=======TEMPO 120 BPM=========================================================
	WNH2	EQU	1Eh	; cala nuta (MSB)
	WNL2	EQU	8480h	; cala nuta (LSB)
	HNH2	EQU	0Fh	; polnuta
	HNL2	EQU	4240h
	QNH2	EQU	7h	; cwiercnuta
	QNL2	EQU	0A120h
	ENH2	EQU	3h	; osemka
	ENL2	EQU	0D090h
	SNH2	EQU	1h	; szesnastka
	SNL2	EQU	0E848h

psp_seg	MACRO			; uzyskanie dostepu do parametrow, jesli korzystano z segmentu danych
	push ax
	mov ah,062h		; funkcja wpisuje do bx adres segmentu PSP (Program Segment Prefix)...
	int 21h			; ...czyli tam, gdzie jest linia polecen zapisana
	mov ds,bx		; teraz PSP jest segmentem danych
	pop ax
	ENDM

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
	opis:	DB 'Program gra wybrana melodie. Opcje (podaj jako parametr):',10,13,'t - Tetris, r - Red Dwarf, s - Silent night, a - alarm$'
	blad:	DB 'Nieprawidlowy parametr$'
ddane	ENDS


rozkazy	SEGMENT use16
	ASSUME cs:rozkazy,ds:ddane
	ORG 0

main:
;=======SPRAWDZANIE PARAMETROW================================================
	mov bx,082h		; adres napisu z parametru
	mov al,[bx-1]		; pierwszy znak z parametru
	cmp al,0Dh		; czy nie podano zadnego parametru
	je opisopcji
	mov al,[bx+1]		; drugi znak napisu z parametru
	cmp al,0Dh
	jne zlyparam		; jesli nie podano opcji jednoliterowej
	mov al,[bx]		; poczatek napisu
	cmp al,'t'		; czy drugim znakiem z parametru jest t
	je playtetris
	cmp al,'a'		; czy drugim znakiem z parametru jest a
	je playalarm
	cmp al,'r'		; czy drugim znakiem z parametru jest r
	je playred
	cmp al,'s'		; czy drugim znakiem z parametru jest s
	je playsilentn

;=======jesli podano nieprawidlowy parametr===================================
zlyparam:
	mov bx,SEG ddane	; adresowanie posrednie dla
	mov ds,bx		; segmentu danych
	mov bx,offset blad
	call puts
	jmp wyjsciedos

;=======WYSWIETLANIE OPISU====================================================
opisopcji:
	mov bx,SEG ddane	; adresowanie posrednie dla
	mov ds,bx		; segmentu danych
	mov bx,offset opis
	call puts
	jmp wyjsciedos

playtetris:
	call tetris
	jmp wyjsciedos

playalarm:
	call alarm
	jmp wyjsciedos

playred:
	call red
	jmp wyjsciedos

playsilentn:
	call silentn

wyjsciedos:
	wyjscie			; wywolanie makra, wyjscie do DOS

	include proced.inc

;=============================================================================
;=======MOTYW Z GRY TETRIS====================================================
;=============================================================================
tetris PROC			; gra motyw z Tetrisa
	sndinit

;=======FRAZA 1======
	note E5
	delay QNH,QNL
	note B4
	delay ENH,ENL
	note C5
	delay ENH,ENL
	note D5
	delay QNH,QNL
	note C5
	delay ENH,ENL
	note B4
	delay ENH,ENL
	note A4
	delay QNH,QNL
	nosound
	note A4
	delay ENH,ENL
	note C5
	delay ENH,ENL
	note E5
	delay QNH,QNL

	note D5
	delay ENH,ENL
	note C5
	delay ENH,ENL
	note B4
	delay QNH,QNL
	nosound
	note B4
	delay ENH,ENL
	note C5
	delay ENH,ENL
	note D5
	delay QNH,QNL

	note E5
	delay QNH,QNL
	note C5
	delay QNH,QNL
	note A4
	delay QNH,QNL
	nosound
	note A4
	delay QNH,QNL

	nosound
	delay QNH,QNL

;=======FRAZA 2======
	note D5
	delay QNH,QNL
	nosound
	note D5
	delay ENH,ENL
	note F5
	delay ENH,ENL
	note A5
	delay QNH,QNL
	note G5
	delay ENH,ENL
	note F5
	delay ENH,ENL
	note E5
	delay QNH,QNL
	nosound
	note E5
	delay ENH,ENL
	note C5
	delay ENH,ENL
	note E5
	delay QNH,QNL

	note D5
	delay ENH,ENL
	note C5
	delay ENH,ENL
	note B4
	delay QNH,QNL
	nosound
	note B4
	delay ENH,ENL
	note C5
	delay ENH,ENL
	note D5
	delay QNH,QNL

	note E5
	delay QNH,QNL
	note C5
	delay QNH,QNL
	note A4
	delay QNH,QNL
	nosound
	note A4
	delay QNH,QNL

	nosound
	ret
tetris ENDP

;=============================================================================
;=======ALARM=================================================================
;=============================================================================
alarm	PROC			; syrena alarmowa
	sndinit

	mov cx,16
polton:
	note Cis6
	delay SNH,SNL
	nosound

	note C6
	delay SNH,SNL
	nosound
	loop polton

	ret
alarm	ENDP

;=============================================================================
;=======RED DWARF=============================================================
;=============================================================================
red	PROC			; theme from Red Dwarf
	sndinit

	call GABCD

	nosound
	delay ENH,ENL

	note G5
	delay ENH,ENL
	delay SNH,SNL
	note A5
	delay ENH,ENL
	delay SNH,SNL
	note B5
	delay ENH,ENL
	delay SNH,SNL
	note C6
	delay ENH,ENL
	delay SNH,SNL
	note D6
	delay ENH,ENL
	delay SNH,SNL

;====================

	note C5
	delay QNH,QNL
	note E5
	delay QNH,QNL
	note G5
	delay HNH,HNL
	note A5
	delay QNH,QNL
	nosound
	note A5
	delay ENH,ENL
	delay SNH,SNL
	note G5
	delay SNH,SNL
	note A5
	delay ENH,ENL
	delay SNH,SNL
	note B5
	delay SNH,SNL
	note C6
	delay QNH,QNL

	note C5
	delay ENH,ENL
	delay SNH,SNL
	note E5
	delay QNH,QNL
	note G5
	delay SNH,SNL
	delay HNH,HNL
	
	note F5
	delay ENH,ENL
	delay SNH,SNL
	note E5
	delay QNH,QNL
	note A5
	delay SNH,SNL
	delay HNH,HNL

	nosound
	note A5
	delay QNH,QNL
	note B5
	delay QNH,QNL
	note C6
	delay HNH,HNL
	nosound
	note C6
	delay ENH,ENL
	delay SNH,SNL
	note B5
	delay SNH,SNL
	note A5
	delay ENH,ENL
	delay SNH,SNL
	note B5
	delay SNH,SNL
	note C6
	delay HNH,HNL

	note G5
	delay HNH,HNL
	note Ais5
	delay HNH,HNL
	note A5
	delay WNH,WNL

	note D5
	delay HNH,HNL
	note Gis5
	delay HNH,HNL
	note G5
	delay HNH,HNL
	delay ENH,ENL
	delay SNH,SNL

	note A5
	delay SNH,SNL
	nosound
	note A5
	delay ENH,ENL
	delay SNH,SNL
	nosound
	note A5
	delay SNH,SNL

;====================

	note D5
	delay QNH,QNL
	note Fis5
	delay QNH,QNL
	note A5
	delay HNH,HNL
	note B5
	delay QNH,QNL
	nosound
	note B5
	delay ENH,ENL
	delay SNH,SNL
	note A5
	delay SNH,SNL
	note B5
	delay ENH,ENL
	delay SNH,SNL
	note Cis6
	delay SNH,SNL
	note D6
	delay QNH,QNL

	note D5
	delay ENH,ENL
	delay SNH,SNL
	note Fis5
	delay QNH,QNL
	note A5
	delay SNH,SNL
	delay HNH,HNL
	
	note G5
	delay ENH,ENL
	delay SNH,SNL
	note Fis5
	delay QNH,QNL
	note B5
	delay SNH,SNL
	delay HNH,HNL

	nosound
	note B5
	delay QNH,QNL
	note Cis6
	delay QNH,QNL
	note D6
	delay HNH,HNL
	nosound
	note D6
	delay ENH,ENL
	delay SNH,SNL
	note Cis6
	delay SNH,SNL
	note B5
	delay ENH,ENL
	delay SNH,SNL
	note Cis6
	delay SNH,SNL
	note D6
	delay HNH,HNL

	note A5
	delay HNH,HNL
	note C6
	delay HNH,HNL
	note B5
	delay WNH,WNL

	note E5
	delay HNH,HNL
	note Ais5
	delay HNH,HNL
	note A5
	delay WNH,WNL

	nosound
	note A5
	delay HNH,HNL
	note C6
	delay HNH,HNL
	note B5
	delay WNH,WNL

	note D5
	delay HNH,HNL
	note Gis5
	delay HNH,HNL
	note G5
	delay HNH,HNL
	delay QNH,QNL
	delay ENH,ENL
	nosound
	delay ENH,ENL

	call GABCD
	nosound
	delay ENH,ENL
	delay SNH,SNL

	call GABCD
	nosound
	delay ENH,ENL
	delay SNH,SNL

	note G5
	delay ENH,ENL
	delay SNH,SNL
	note A5
	delay ENH,ENL
	delay SNH,SNL
	note B5
	delay ENH,ENL
	delay SNH,SNL
	note C6
	delay ENH,ENL
	delay SNH,SNL
	note D6
	delay ENH,ENL
	delay SNH,SNL
	note C6
	delay HNH,HNL
	delay ENH,ENL
	delay SNH,SNL
	note C5
	delay HNH,HNL
	delay QNH,QNL
	delay ENH,ENL

	nosound
	ret
red	ENDP

GABCD	PROC

	note G4
	delay ENH,ENL
	delay SNH,SNL
	note A4
	delay ENH,ENL
	delay SNH,SNL
	note B4
	delay ENH,ENL
	delay SNH,SNL
	note C5
	delay ENH,ENL
	delay SNH,SNL
	note D5
	delay ENH,ENL
	delay SNH,SNL

	ret
GABCD	ENDP

;=============================================================================
;=======SILENT NIGHT==========================================================
;=============================================================================
silentn	PROC			; Silent night, Cicha noc
	sndinit

	mov cx,2
sn1:
	call snbar12		; pierwsze dwa takty powtorzone 2 razy
	loop sn1

	note A5
	delay HNH2,HNL2
	nosound
	note A5
	delay QNH2,QNL2
	note Fis5
	delay HNH2,HNL2
	nosound
	delay QNH2,QNL2

	note G5
	delay HNH2,HNL2
	nosound
	note G5
	delay QNH2,QNL2
	note D5
	delay HNH2,HNL2
	nosound
	delay QNH2,QNL2

	mov cx,2		; takty 9-12 powtorzone 2 razy
sn2:
	call snbar9
	loop sn2

	note A5
	delay HNH2,HNL2
	nosound
	note A5
	delay QNH2,QNL2
	note C6
	delay QNH2,QNL2
	delay ENH2,ENL2
	note A5
	delay ENH2,ENL2
	note Fis5
	delay QNH2,QNL2
	note G5
	delay HNH2,HNL2
	delay QNH2,QNL2
	note B5
	delay HNH2,HNL2
	nosound
	delay QNH2,QNL2

	note G5
	delay QNH2,QNL2
	note D5
	delay QNH2,QNL2
	note B4
	delay QNH2,QNL2
	note D5
	delay QNH2,QNL2
	delay ENH2,ENL2
	note C5
	delay ENH2,ENL2
	note A4
	delay QNH2,QNL2
	note G4
	delay HNH2,HNL2

	nosound
	ret
silentn	ENDP

snbar12	PROC			; pierwsze dwa takty Silent Night
	note D5
	delay QNH2,QNL2
	delay ENH2,ENL2
	note E5
	delay ENH2,ENL2
	note D5
	delay QNH2,QNL2
	note B4
	delay QNH2,QNL2
	delay ENH2,ENL2
	nosound
	delay QNH2,QNL2
	delay ENH2,ENL2
	ret
snbar12	ENDP

snbar9	PROC			; takty  9-12 Silent night
	note E5
	delay HNH2,HNL2
	nosound
	note E5
	delay QNH2,QNL2
	note G5
	delay QNH2,QNL2
	delay ENH2,ENL2
	note Fis5
	delay ENH2,ENL2
	note E5
	delay QNH2,QNL2
	note D5
	delay QNH2,QNL2
	delay ENH2,ENL2
	note E5
	delay ENH2,ENL2
	note D5
	delay QNH2,QNL2
	note B4
	delay HNH2,HNL2
	nosound
	delay QNH2,QNL2
	ret
snbar9	ENDP

;=============================================================================

rozkazy	ENDS

sstos	SEGMENT stack
	DW 128 dup (?)
sstos	ENDS

END main
