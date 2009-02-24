; program wyswietla dowolny napis procedura puts

wyjscie	MACRO
        mov ah,4Ch		; wyjscie
        int 21h			; do DOS
	ENDM

rozkazy SEGMENT use16
	ASSUME cs:rozkazy,ds:rozkazy
        ORG 100h

main:
	mov bx,OFFSET napis
	call puts

	wyjscie


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


putchar PROC
	push ax
	mov ah,0Eh
	int 10h
	pop ax
	ret
putchar ENDP


;====MIEJSCE NA DANE====
napis:	DB "Witaj swiecie!",10,13,'$'

rozkazy ENDS

END main
