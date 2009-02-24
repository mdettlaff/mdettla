; program wypisuje linie polecen

rozkazy SEGMENT use16
	ASSUME cs:rozkazy,ds:rozkazy
        ORG 100h

main:
	mov bx,082h		; bx wskazuje na linie polecen
	call putln

        mov ah,4Ch		; wyjscie
        int 21h			; do DOS

putln	PROC			; wyswietla jedna linijke tekstu z napisu na ktory wskazuje bx
	push ax			; odkladamy ax na stos, zeby go nie zepsuc dzialaniem procedury
	push bx
znak:				; petla do wyswietlania napisu
	mov al,[bx]
	cmp al,13		; ENTER oznacza...
	je end_znak		; ...koniec napisu
	call putchar
	inc bx
	jmp znak
end_znak:
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

rozkazy ENDS

END main
