; program rysuje choinke z mrugajacymi lampkami (uruchamiac w trybie pelnoekranowym)

bombka	MACRO	X,Y		; rysuje mrugajaca bombke w pozycji x, y
	mov dl,X		; pozycja x
	mov dh,Y		; pozycja y
	mov ah,02h		; funkcja przesuwa kursor w dl, dh
	int 10h

	mov al,219
	mov bl,0C3h		; atrybut (kolor)
	mov ah,09h		; specjalne wypisywanie znaku
	mov cx,1		; ile razy napisac znak
	int 10h
	ENDM

wyjscie	MACRO
        mov ah,4Ch		; wyjscie
        int 21h			; do DOS
	ENDM


rozkazy SEGMENT use16
	ASSUME cs:rozkazy,ds:rozkazy
        ORG 100h

main:
	call clrscr

	mov dl,38		; pozycja x
	mov dh,0		; pozycja y
	mov cx,8		; ile rzadkow napisac
	call trojkat

	mov dl,38		; pozycja x
	mov dh,3		; pozycja y
	mov cx,11		; ile rzadkow napisac
	call trojkat

	mov dl,38		; pozycja x
	mov dh,7		; pozycja y
	mov cx,15		; ile rzadkow napisac
	call trojkat

	bombka 37,5
	bombka 39,9
	bombka 40,14
	bombka 33,16
	bombka 36,19

	mov dh,23		; pozycja y
	mov ah,02h
	int 10h

	wyjscie

clrscr	PROC			; w³¹czenie trybu tekstowego (czyszczenie ekranu)
	mov al,3
	xor ah,ah
	int 10h
	ret
clrscr	ENDP

trojkat	PROC			; rysuje zielony trojkat o wsp. x=dx, y=dh, wys=cx
	push di
	mov di,cx		; pamieta wysokosc
rzad:
	sub dl,1
	add dh,1
	mov al,219		; ASCII: wypelniony prostokat
	mov ah,02h		; ustawianie pozycji
	int 10h
	mov bl,2		; atrybut (kolor)
	mov ah,09h

	push cx
	push di
	add di,di
	add di,2
	sub di,cx
	sub di,cx
	mov cx,di		; ile razy napisac znak
	int 10h
	pop di
	pop cx
	loop rzad
	pop di
	ret
trojkat	ENDP

rozkazy ENDS

END main
