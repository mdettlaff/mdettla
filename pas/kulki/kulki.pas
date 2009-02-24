uses CRT;

const COLORS=7; {liczba kolorow kulek; domyslnie 7}
      BONUSTIME=12; {czas trwania bonusow czasowych (7, 8 i 9-go); domyslnie 12}
      CHEATCODE='bonus'; {kod do wlaczania CHEATMODE}
      SPEED=100; {szybkosc przemieszczania sie kulek}
      BALL='O'; {wyglad kulki}
      SQUARE=#250; {wyglad pola}
      MARKED='X'; {zaznaczona kulka}
      VANISH='X'; {wyglad kulki przed zniknieciem}
      SPECIAL='@'; {kulki specjalne}
      ADDBALL=3; {ilosc kulek dodawana w jednej turze}
      MAXPLN=99; {maksymalna dlugosc trasy}
      P1=2; {przesuniecie planszy w dol}
      P2=24; {przesuniecie planszy w prawo}
      SAVEGAMENAME='savegame.sav'; {nazwa pliku z zapisana gra}
      HSFILENAME='hscore.hs'; {nazwa pliku ze wpisami}
      HSQ=15; {ilosc wpisow na liscie}
type plansza=array [1..9,1..9] of integer;
     rucht=array [1..4] of integer;
     trasat=array [1..MAXPLN,1..2] of integer;
     dokasowt= array[1..32,1..2] of integer;
     kierunekt=array [1..2] of integer;
     kiet=array [1..8,1..2] of integer;
     direct=array [1..4,1..2] of integer;
     nastkulkit=array [1..ADDBALL] of integer;
     bonust=array [1..9] of integer;
     koloryt=array [1..8] of integer;
     savedgamet=record
 		  gra:plansza;
		  punkty:integer;
		  nastkulki:nastkulkit;
		  bonus:bonust;
	        end;
     wpis=record
	    imie:string;
	    wynikpunktowy:integer;
	  end;
     wpisyt=array [1..HSQ] of wpis;
var CHEATMODE:boolean;
    i,j,nkulek,kuldouloz,punkty:integer;
    gra:plansza;
    bonus:bonust;
    nastkulki:nastkulkit;
    trasa:trasat;
    ruch:rucht;
    przerwanie,grazaczeta:boolean;
    kolory:koloryt;
    savedgame:savedgamet;
    wpisy:wpisyt;


procedure ustalkolory;
begin
  for i:=1 to 6 do
    kolory[i]:=i+8;
  kolory[7]:=6;
  kolory[8]:=15; {kulki specjalne}
end;

procedure gotoxypla(i:integer; j:integer);
begin
  gotoxy(i+p2,j+p1);
end;

procedure pole(kolor:integer);
begin
  if kolor = 0 then
  begin
    textcolor(8);
    write(SQUARE);
    textcolor(7);
  end
  else if kolor = 8 then {kulki specjalne}
  begin
    textcolor(kolory[kolor]);
    write(SPECIAL);
    textcolor(7);
  end
  else
  begin
    textcolor(kolor);
    write(BALL);
    textcolor(7)
  end;
end;

procedure wyswietl;
var i,j:integer;
begin
  clrscr; 
  gotoxy(2,2); {wypisz kulki w nastepnej turze}
  write('Nastepne kulki:');
  gotoxy(2,3);
  for i:=1 to ADDBALL do
    if nastkulki[i] = 8 then
    begin {kulki specjalne}
      textcolor(kolory[nastkulki[i]]);
      write(SPECIAL);
    end
    else
    begin
      textcolor(kolory[nastkulki[i]]);
      write(BALL)
    end;
  gotoxy(2,5); {wypisz liczbe punktow}
  textcolor(7);
  write('Punkty:');
  gotoxy(2,6);
  write(punkty);

  gotoxy(45,2);
  write('Bonusy:');
  gotoxy(45,4);
  textcolor(8); if bonus[1] > 0 then textcolor(7);
  write('1. Nastepne');
  gotoxy(45,5);
  textcolor(8); if bonus[2] > 0 then textcolor(7);
  write('2. Inne kolory');
  gotoxy(45,6);
  textcolor(8); if bonus[3] > 0 then textcolor(7);
  write('3. Dwa ruchy');
  gotoxy(45,7);
  textcolor(8); if bonus[4] > 0 then textcolor(7);
  write('4. Cofnij ruch');
  gotoxy(45,8);
  textcolor(8); if bonus[5] > 0 then textcolor(7);
  write('5. Usun kolor');
  if bonus[5]>0 then
  begin
    textcolor(kolory[bonus[5]]);
    write(' ',BALL);
  end;
  gotoxy(45,9);
  textcolor(8); if bonus[6] > 0 then textcolor(7);
  write('6. Skasuj kulke x',bonus[6]);
  gotoxy(45,10);
  textcolor(8); if bonus[7] > 0 then textcolor(7);
  write('   Punkty x3');
  gotoxy(45,11);
  textcolor(8); if bonus[8] > 0 then textcolor(7);
  write('   Cztery kulki');
  gotoxy(45,12);
  textcolor(8); if bonus[9] > 0 then textcolor(7);
  write('   Kulki specjalne');
  textcolor(7);

  gotoxy(24,2); {wypisz pole gry}
  write(#218);
  for i:=1 to 9 do
    write(#196);
  write(#191);
  gotoxy(24,3);
  for i:=1 to 9 do
  begin
    write(#179);
    for j:=1 to 9 do
    begin
      case gra[i,j] of
        0 : pole(0);
        1 : pole(kolory[1]);
        2 : pole(kolory[2]);
        3 : pole(kolory[3]);
        4 : pole(kolory[4]);
        5 : pole(kolory[5]);
        6 : pole(kolory[6]);
        7 : pole(kolory[7]);
	8 : pole(8); {kulki specjalne}
      end;
    end;
    write(#179);
    gotoxy(24,i+3); 
  end;
  write(#192);
  for i:=1 to 9 do
    write(#196);
  write(#217);
  gotoxy(2,12);
  write('Esc - menu');
end;

procedure losujkulki(ile:integer);
var i:integer;
begin
  for i:=1 to ile do
    nastkulki[i]:=random(COLORS)+1;
end;

procedure usunkulke;
begin
  gra[ruch[1],ruch[2]]:=0;
  dec(bonus[6]);
end;

procedure podaj(var przerwijgre:boolean; var bonusgo:integer);
  procedure idz(k1:integer; k2:integer; var kursor:kierunekt);
  begin
    inc(kursor[1],k1);
    inc(kursor[2],k2);
    gotoxypla(kursor[2],kursor[1]);
  end;
var kursor:kierunekt;
begin
  writeln;
  gotoxypla(5,5);
  kursor[1]:=5; kursor[2]:=5; 

  repeat {podawanie kulki do przesuniecia}
    case readkey of
      #0  : begin
	      case readkey of
		'P' : if kursor[1] < 9 then idz(1,0,kursor);  {dol}
		'H' : if kursor[1] > 1 then idz(-1,0,kursor); {gora}
		'M' : if kursor[2] < 9 then idz(0,1,kursor);  {prawo}
		'K' : if kursor[2] > 1 then idz(0,-1,kursor); {lewo}
	      end;
            end;
      ' ' : if gra[kursor[1],kursor[2]]<>0 then
       	      begin
	        ruch[1]:=kursor[1]; ruch[2]:=kursor[2];
	        break
              end;
      #27 : begin przerwijgre:=true; break end;
      {uwaga: trzeba zmieniac ponizszy kod w dwoch miejscach}
      '1' : if bonus[1] > 0 then begin
				   dec(bonus[1]); bonusgo:=1; break
			         end;
      '2' : if bonus[2] > 0 then begin
				   dec(bonus[2]); bonusgo:=2; break
			         end;
      '3' : if bonus[3] = 3 then dec(bonus[3]);
      '4' : if bonus[4] = 1 then begin
      				   inc(bonus[4]); break
				 end;
      '5' : if bonus[5] > 0 then begin
      				   bonusgo:=5; break
				 end;
      '6' : if bonus[6] > 0 then begin
      				   gotoxy(2,14);
				   writeln('Najpierw wybierz kulke');
				   gotoxypla(kursor[2],kursor[1]);
				 end;
      {CHEATMODE}
      'q' : if CHEATMODE then bonus[1]:=1; 		{nastepne}
      'w' : if CHEATMODE then bonus[2]:=1;		{inne kolory}
      'e' : if CHEATMODE then bonus[3]:=3;		{dwa ruchy}
      'r' : if CHEATMODE then bonus[4]:=1;		{cofnij}
      't' : if CHEATMODE then bonus[5]:=random(COLORS)+1;{usun kolor}
      'y' : if CHEATMODE then bonus[6]:=bonus[6]+3;	{usun 3}
      'u' : if CHEATMODE then bonus[7]:=2*BONUSTIME;	{punkty x3}
      'i' : if CHEATMODE then bonus[8]:=1*BONUSTIME;	{cztery kulki}
      'o' : if CHEATMODE then bonus[9]:=2*BONUSTIME;	{kulki specjalne}
      'p' : if CHEATMODE then inc(punkty,100);
    end;
  until przerwanie;

  textcolor(kolory[gra[kursor[1],kursor[2]]]);
  write(MARKED);
  gotoxypla(ruch[2],ruch[1]);

  while (przerwijgre = false) and (bonusgo = 0) and (bonus[4] <> 2) do {podawanie miejsca docelowego kulki}
    case readkey of
      #0  : begin
	      case readkey of
		'P' : if kursor[1] < 9 then idz(1,0,kursor);
		'H' : if kursor[1] > 1 then idz(-1,0,kursor);
		'M' : if kursor[2] < 9 then idz(0,1,kursor);
		'K' : if kursor[2] > 1 then idz(0,-1,kursor);
	      end;
            end;
      ' ' : if gra[kursor[1],kursor[2]]=0 then
       	    begin
	      ruch[3]:=kursor[1]; ruch[4]:=kursor[2];
	      break
            end
	    else if not ((kursor[1]=ruch[1]) and (kursor[2]=ruch[2])) then
            begin {zmiana zaznaczenia}
              textcolor(kolory[gra[kursor[1],kursor[2]]]);
              gotoxypla(kursor[2],kursor[1]);
              write(MARKED);
              textcolor(kolory[gra[ruch[1],ruch[2]]]);
	      gotoxypla(ruch[2],ruch[1]);
	      write(BALL);
              gotoxypla(kursor[2],kursor[1]);
	      ruch[1]:=kursor[1]; ruch[2]:=kursor[2];
            end;
      #27 : begin przerwijgre:=true; break end;
      {uwaga: trzeba zmieniac ponizszy kod w dwoch miejscach}
      '1' : if bonus[1] > 0 then begin
				   dec(bonus[1]); bonusgo:=1; break
			         end;
      '2' : if bonus[2] > 0 then begin
				   dec(bonus[2]); bonusgo:=2; break
			         end;
      '3' : if bonus[3] = 3 then dec(bonus[3]);
      '4' : if bonus[4] = 1 then begin
      				   inc(bonus[4]); break
				 end;
      '5' : if bonus[5] > 0 then begin
      				   bonusgo:=5; break
				 end;
      '6' : if bonus[6] > 0 then begin
      				   bonusgo:=6;
				   usunkulke; break
				 end;

      {CHEATMODE}
      'q' : if CHEATMODE then bonus[1]:=1; 		{nastepne}
      'w' : if CHEATMODE then bonus[2]:=1;		{inne kolory}
      'e' : if CHEATMODE then bonus[3]:=3;		{dwa ruchy}
      'r' : if CHEATMODE then bonus[4]:=1;		{cofnij}
      't' : if CHEATMODE then bonus[5]:=random(COLORS)+1;{usun kolor}
      'y' : if CHEATMODE then bonus[6]:=bonus[6]+3;	{usun 3}
      'u' : if CHEATMODE then bonus[7]:=2*BONUSTIME;	{punkty x3}
      'i' : if CHEATMODE then bonus[8]:=1*BONUSTIME;	{cztery kulki}
      'o' : if CHEATMODE then bonus[9]:=2*BONUSTIME;	{kulki specjalne}
      'p' : if CHEATMODE then inc(punkty,100);
    end;
  textcolor(7);

end;

function wolnepole(gra:plansza; x:integer; y:integer):boolean; {x - os pionowa, y - pozioma!}
begin
  wolnepole:=true;
  if (x > 9) or (x < 1) or (y > 9) or (y < 1) then {czy wyszlaby poza plansze}
    wolnepole:=false;
  if gra[x,y]<>0 then {czy kulka blokuje}
    wolnepole:=false;
end;

function min(a:integer; b:integer):integer;
begin
  if a < b then min:=a
  else min:=b;
end;

procedure znajdztrase(gra:plansza; var moznaprzes:boolean);
var i,j,k,x,y,dx,dy:integer;
    pathlen:array[1..9,1..9] of integer;
begin
  {przypisywanie "nieskonczonej" odleglosci}
  for i:=1 to 9 do
    for j:=1 to 9 do
      pathlen[i,j]:=MAXPLN;
  pathlen[ruch[1],ruch[2]]:=0;

  if wolnepole(gra,ruch[1]-1,ruch[2]) then pathlen[ruch[1]-1,ruch[2]]:=1;
  if wolnepole(gra,ruch[1]+1,ruch[2]) then pathlen[ruch[1]+1,ruch[2]]:=1;
  if wolnepole(gra,ruch[1],ruch[2]-1) then pathlen[ruch[1],ruch[2]-1]:=1;
  if wolnepole(gra,ruch[1],ruch[2]+1) then pathlen[ruch[1],ruch[2]+1]:=1;
  
  {algorytm Dijkstry zmodyfikowany}
  for k:=1 to 81 do
    for i:=1 to 9 do
      for j:=1 to 9 do
        begin
	  if wolnepole(gra,i-1,j) and wolnepole(gra,i,j) then pathlen[i,j]:=min(pathlen[i,j], pathlen[i-1,j]+1);
	  if wolnepole(gra,i+1,j) and wolnepole(gra,i,j) then pathlen[i,j]:=min(pathlen[i,j], pathlen[i+1,j]+1);
	  if wolnepole(gra,i,j-1) and wolnepole(gra,i,j) then pathlen[i,j]:=min(pathlen[i,j], pathlen[i,j-1]+1);
	  if wolnepole(gra,i,j+1) and wolnepole(gra,i,j) then pathlen[i,j]:=min(pathlen[i,j], pathlen[i,j+1]+1);
        end;

  if pathlen[ruch[3],ruch[4]]=MAXPLN then moznaprzes:=false
  else
  begin {algorytm znajdowania sciezki przy znanych odleglosciach}
    moznaprzes:=true;
    for i:=1 to MAXPLN do
    begin
      trasa[i,1]:=0; trasa[i,2]:=0;
    end;

    k:=pathlen[ruch[3],ruch[4]];
    trasa[k+1,1]:=ruch[3];
    trasa[k+1,2]:=ruch[4];

    for i:=k+1 downto 2 do
    begin 
      x:=trasa[i,1];
      y:=trasa[i,2];
      {polnoc}
      dx:=-1; dy:=0;
      if wolnepole(gra,x+dx,y+dy) then
        if (pathlen[x+dx,y+dy]+1) = pathlen[x,y] then
        begin
          trasa[i-1,1]:=x+dx;
          trasa[i-1,2]:=y+dy;
        end;
      {poludnie}
      dx:=1; dy:=0;
      if wolnepole(gra,x+dx,y+dy) then
        if (pathlen[x+dx,y+dy]+1) = pathlen[x,y] then
        begin
          trasa[i-1,1]:=x+dx;
          trasa[i-1,2]:=y+dy;
        end;
      {wschod}
      dx:=0; dy:=-1;
      if wolnepole(gra,x+dx,y+dy) then
        if (pathlen[x+dx,y+dy]+1) = pathlen[x,y] then
        begin
          trasa[i-1,1]:=x+dx;
          trasa[i-1,2]:=y+dy;
        end;
      {zachod}
      dx:=0; dy:=1;
      if wolnepole(gra,x+dx,y+dy) then
        if (pathlen[x+dx,y+dy]+1) = pathlen[x,y] then
        begin
          trasa[i-1,1]:=x+dx;
          trasa[i-1,2]:=y+dy;
        end;
    end;
    trasa[1,1]:=ruch[1];
    trasa[1,2]:=ruch[2];
  end;
end;

procedure przesun; 
var a,kolor:integer; 
begin
  a:=1;
  kolor:=gra[ruch[1],ruch[2]];

  gotoxypla(trasa[1,2],trasa[1,1]);
  textcolor(8);
  write(SQUARE);
  repeat
    inc(a);
    gotoxypla(trasa[a,2],trasa[a,1]); {narysuj przemieszczana kulke}
    textcolor(kolory[kolor]);
    if kolor = 8 then write(SPECIAL) {kulki specjalne}
    else write(BALL);

    delay(SPEED);

    gotoxypla(trasa[a,2],trasa[a,1]); {przywroc to co bylo} 
    textcolor(8); 
    write(SQUARE);
  until (trasa[a,1]=ruch[3]) and (trasa[a,2]=ruch[4]) or (a>MAXPLN);
 
  textcolor(7);
  gra[ruch[1],ruch[2]]:=0;
  gra[ruch[3],ruch[4]]:=kolor;
end;

function licz(kolor:integer; wsp:kierunekt; kiex:integer; kiey:integer):integer;
var i,j:integer;
begin
  licz:=-1;
  i:=wsp[1]; j:=wsp[2];
  while (gra[i,j] = kolor) or (gra[i,j] = 8) do
  begin
    inc(i,kiex); inc(j,kiey);
    inc(licz);
  end;
end;

procedure dopiszdokasow(kolor:integer; wsp:kierunekt; kie1:integer; kie2:integer; kie3:integer; kie4:integer;
			var dokasow:dokasowt; var k:integer);
begin
  {k - liczba kulek do skasowania}
  i:=wsp[1]; j:=wsp[2];
  inc(i,kie1); inc(j,kie2);
  while (gra[i,j] = kolor) or (gra[i,j] = 8) do
  begin
    if gra[i,j] <> 8 then
    begin
      inc(k);
      dokasow[k,1]:=i;
      dokasow[k,2]:=j;
    end;
    inc(i,kie1); inc(j,kie2);
  end;
  i:=wsp[1]; j:=wsp[2];
  inc(i,kie3); inc(j,kie4);
  while (gra[i,j] = kolor) or (gra[i,j] = 8) do
  begin
    if gra[i,j] <> 8 then
    begin
      inc(k);
      dokasow[k,1]:=i;
      dokasow[k,2]:=j;
    end;
    inc(i,kie3); inc(j,kie4);
  end;
end;

procedure sprawdzspecjalne(wsp:kierunekt; kie:kiet; var dokasow:dokasowt; var k:integer);
var i,j,kolor:integer;
begin
  for j:=1 to 7 do
  begin
    i:=1;
    kolor:=j;
    while i < 9 do {dodajemy do kasowania w kierunku: pionowym12, poziomym34, ukosnym56, ukosnym78}
    begin
      if (1 + licz(kolor,wsp,kie[i,1],kie[i,2]) + licz(kolor,wsp,kie[i+1,1],kie[i+1,2]) >= kuldouloz) then
	dopiszdokasow(kolor,wsp,kie[i,1],kie[i,2],kie[i+1,1],kie[i+1,2],dokasow,k);
      inc(i,2);
    end;
  end;
end;

procedure usunwzor(wsp:kierunekt; var k:integer);
var kie:kiet;
    i,kolor:integer;
    dokasow:dokasowt;
begin
  k:=0;

  kie[1,1]:=-1; kie[1,2]:=0;		{kie:	5 1 7	}
  kie[2,1]:=1; kie[2,2]:=0;             {        \|/	}
  kie[3,1]:=0; kie[3,2]:=-1;            {       3- -4	}
  kie[4,1]:=0; kie[4,2]:=1;             {        /|\	}
  kie[5,1]:=-1; kie[5,2]:=-1;		{	8 2 6	}
  kie[6,1]:=1; kie[6,2]:=1;
  kie[7,1]:=-1; kie[7,2]:=1;
  kie[8,1]:=1; kie[8,2]:=-1;
  
  i:=1;
  kolor:=gra[wsp[1],wsp[2]];
  while i < 9 do {dodajemy do kasowania w kierunku: pionowym12, poziomym34, ukosnym56, ukosnym78}
  begin
    if (1 + licz(kolor,wsp,kie[i,1],kie[i,2]) + licz(kolor,wsp,kie[i+1,1],kie[i+1,2]) >= kuldouloz) then
      dopiszdokasow(kolor,wsp,kie[i,1],kie[i,2],kie[i+1,1],kie[i+1,2],dokasow,k);
    inc(i,2);
  end;

  if kolor = 8 then {kulki specjalne}
    sprawdzspecjalne(wsp,kie,dokasow,k);

  if k > 0 then
  begin
    textcolor(kolory[gra[wsp[1],wsp[2]]]);
    if kolor <> 8 then {kulki specjalne}
      gra[wsp[1],wsp[2]]:=0; {kasowanie kulki przesunietej}
    gotoxypla(wsp[2],wsp[1]);
    write(VANISH);
    for i:=1 to k do {kasowanie kulek ulozonych we wzor}
    begin
      gra[dokasow[i,1],dokasow[i,2]]:=0;
      gotoxypla(dokasow[i,2],dokasow[i,1]);
      write(VANISH);
    end;
    delay(400);
    textcolor(7);
  end;
end;

procedure przyznajpunkty(k:integer; var przyznanopunkty:boolean);
begin
  if (k+1) = 6 then bonus[1]:=1 			{nastepne}
  else if (k+1) = 7 then bonus[2]:=1 			{inne kolory}
  else if (k+1) = 8 then bonus[3]:=3 			{dwa ruchy}
  else if (k+1) = 9 then bonus[4]:=1 			{cofnij}
  else if (k+1) = 10 then bonus[5]:=random(COLORS)+1	{usun kolor}
  else if (k+1) = 11 then bonus[6]:=bonus[6]+3 		{usun 3}
  else if (k+1) = 12 then bonus[7]:=2*BONUSTIME		{punkty x3}
  else if (k+1) = 13 then bonus[8]:=1*BONUSTIME		{cztery kulki}
  else if (k+1) >= 14 then bonus[9]:=2*BONUSTIME;	{kulki specjalne}

  if (bonus[7] > 0) and (k > 0) then
  begin
    k:=3*k;
    inc(punkty,4);
  end;

  if k > 0 then
  begin
    punkty:=punkty+2*k+2; {przyznawanie punktow}
    przyznanopunkty:=true;
  end
  else przyznanopunkty:=false;
end;

procedure dodajkulki(ile:integer);
var i,j,l,tmp:integer;
    dodano:boolean;
    wsp:kierunekt;
begin
  for l:=1 to ile do
  begin
    dodano:=false;
    while dodano=false do
    begin
      i:=random(9)+1;
      j:=random(9)+1;
      if gra[i,j]=0 then
      begin
        gra[i,j]:=nastkulki[l]; {dodawanie nastepnego wylosowanego koloru}
	wsp[1]:=i; wsp[2]:=j;
	usunwzor(wsp, tmp);
        dodano:=true;
      end;
    end;
  end;
end;

procedure policzkulki;
begin
  nkulek:=0;
  for i:=1 to 9 do
    for j:=1 to 9 do
      if gra[i,j]<>0 then inc(nkulek);
end;

procedure bonusy;
var i:integer;
begin
  if bonus[9] = 2*BONUSTIME then {kulki specjalne}
    begin
    for i:=1 to ADDBALL do
      nastkulki[i]:=8;
    dec(bonus[9]);
    end
  else if (bonus[9] > 1) then
      dec(bonus[9]);

  if bonus[7] > 0 then dec(bonus[7]);

  if (bonus[3] = 1) or (bonus[3] = 2) then dec(bonus[3]);

  if bonus[4] = 2 then bonus[4]:=0;

  if bonus[8] = 1 then
  begin
    kuldouloz:=5;
    dec(bonus[8]);
  end
  else if bonus[8] > 0 then
  begin
    kuldouloz:=4;
    dec(bonus[8]);
  end;
end;

procedure odczytajstan(stangry:savedgamet);
var i,j:integer;
begin
  for i:=1 to 9 do
    for j:=1 to 9 do
      gra[i,j]:=stangry.gra[i,j];
  punkty:=stangry.punkty;
  for i:=1 to ADDBALL do
    nastkulki[i]:=stangry.nastkulki[i];
end;

procedure zapiszstan(var stangry:savedgamet);
var i,j:integer;
begin
  for i:=1 to 9 do
    for j:=1 to 9 do
      stangry.gra[i,j]:=gra[i,j];
  stangry.punkty:=punkty;
  for i:=1 to ADDBALL do
    stangry.nastkulki[i]:=nastkulki[i];
end;

procedure usunkolor(kolor:integer);
var i,j:integer;
begin
  for i:=1 to 9 do
    for j:= 1 to 9 do
      if gra[i,j] = kolor then
      begin
	gotoxypla(j,i);
	textcolor(kolory[kolor]);
	write(VANISH);
	gra[i,j]:=0;
      end;
  delay(500);
  textcolor(7);
end;

procedure zamienmiejsc(var a:wpis; var b:wpis);
var tmp:wpis;
begin
  tmp:=a;
  a:=b;
  b:=tmp;
end;

function istniejeplik(nazwapliku:string):boolean;
var f: file of savedgamet;
    IOR: integer;
begin
  assign(f,nazwapliku);
  {$I-}
  reset(f);
  {$I+}
  IOR:=IOResult;
  istniejeplik:=true;
  if IOR = 2 then
    istniejeplik:=false
  else if IOR <> 0 then
    Halt;
  close(f);
end;

procedure wczytajwpisy;
var highscorefile:file of wpisyt;
begin
  assign(highscorefile, HSFILENAME);
  reset(highscorefile);
  read(highscorefile, wpisy);
  close(highscorefile);
end;

procedure zapiszwpisy;
var highscorefile:file of wpisyt;
begin
  assign(highscorefile, HSFILENAME);
  rewrite(highscorefile);
  write(highscorefile, wpisy);
  close(highscorefile);
end;

procedure newhighscore;
var i:integer;
    s:string;
begin
  for i:=1 to HSQ do
  begin
    str(i,s);
    wpisy[i].imie:='Gracz '+s;
    wpisy[i].wynikpunktowy:=(HSQ+1)*100-i*100;
  end;
  zapiszwpisy;
end;

procedure showhighscore(highlight:integer);
var i,j:integer;
      s:string;
begin
  clrscr;
  wczytajwpisy;
  gotoxy(29,2);
  write('NAJLEPSZE WYNIKI');
  for i:=1 to HSQ do
  begin
    if highlight = i then textcolor(15);
    str(wpisy[i].wynikpunktowy,s);
    if (length(s) mod 2) <> 0 then s:=' '+s;
    gotoxy(18,i+4);
    if i < 10 then write(' ',i,'. ')
    else write(i,'. ');
    if (length(wpisy[i].imie) mod 2) <> 0 then wpisy[i].imie:=wpisy[i].imie+' ';
    write(wpisy[i].imie);
    for j:=1 to (12-length(wpisy[i].imie) div 2) do
      write(' .');
    for j:=1 to (4-length(s) div 2) do
      write(' .');
    write(' ');
    write(s);
    textcolor(7);
  end;
  gotoxy(18,20);
  readkey;
end;

procedure koniecgry;
var i,min,minpoz,highlight:integer;
    maszwpis:boolean;
    imiegracza:string;
begin
  clrscr;
  highlight:=0;
  imiegracza:='Uzytkownik';
  grazaczeta:=false;
  wczytajwpisy;
  maszwpis:=false;
  for i:=1 to HSQ do
    if punkty > wpisy[i].wynikpunktowy then maszwpis:=true;
  if maszwpis then
  begin
    min:=wpisy[1].wynikpunktowy;
    for i:=1 to HSQ do {szukanie najmniejszego wyniku na liscie}
      if wpisy[i].wynikpunktowy < min then
      begin
	min:=wpisy[i].wynikpunktowy;
	minpoz:=i
      end;
    repeat
      clrscr;
      gotoxy(33,2);
      write('Gratulacje!');
      gotoxy(20,4);
      write('Twoj wynik punktowy zostanie zapisany');
      gotoxy(20,5);
      write('   na liscie najlepszych wynikow.');
      gotoxy(20,7);
      if length(imiegracza) > 22 then
      write('Imie zbyt dlugie. Wpisz jeszcze raz:')
      else
      write('         Wpisz swoje imie:');
      gotoxy(31,9);
      readln(imiegracza);
    until (length(imiegracza) < 23) and (length(imiegracza) > 0);

    wpisy[minpoz].imie:=imiegracza;
    wpisy[minpoz].wynikpunktowy:=punkty;
    for j:=1 to HSQ do {sortowanie}
      for i:=1 to (HSQ-1) do
	if wpisy[i].wynikpunktowy < wpisy[i+1].wynikpunktowy then
	  zamienmiejsc(wpisy[i],wpisy[i+1]);
    for i:=1 to HSQ do {ktory jestes na liscie}
      if (wpisy[i].imie = imiegracza) and (wpisy[i].wynikpunktowy = punkty) then
	begin
	  highlight:=i;
	  break
       	end;
    zapiszwpisy;
    showhighscore(highlight);
  end
  else
  begin
    gotoxy(33,5);
    writeln('Koniec gry!');
    gotoxy(33,6);
    readkey;
  end;
  punkty:=0;
end;

procedure continuegame;
label poczatek,bonusgo1,bonusgo2;
var przerwijgre,moznaprzes,przyznanopunkty:boolean;
    wsp:kierunekt;
    k,bonusgo:integer;
    stangry:savedgamet;
begin
  for i:=1 to 9 do {inicjalizacja zmiennej stangry}
    for j:=1 to 9 do
      stangry.gra[i,j]:=gra[i,j];
  stangry.punkty:=punkty;
  for i:=1 to ADDBALL do
    stangry.nastkulki[i]:=nastkulki[i];
  przerwijgre:=false;
  while (nkulek < 79) and (przerwijgre=false) do
  begin
    poczatek:
    bonusgo:=0;
    moznaprzes:=true;
    bonusy;
    if bonus[9] = 1 then begin dec(bonus[9]); usunkolor(8) end; {kulki specjalne}
    wyswietl;
    podaj(przerwijgre, bonusgo);
    if bonus[4] = 2 then begin odczytajstan(stangry); goto poczatek end;
    zapiszstan(stangry);
    if bonusgo = 1 then goto bonusgo1;
    if bonusgo = 2 then goto bonusgo2;
    if bonusgo = 5 then begin usunkolor(bonus[5]); bonus[5]:=0; goto poczatek end;
    if bonusgo = 6 then goto poczatek;
    if przerwijgre then break;
    znajdztrase(gra,moznaprzes);
    if not moznaprzes then goto poczatek;
    przesun;
    wsp[1]:=ruch[3]; wsp[2]:=ruch[4];
    usunwzor(wsp, k);
    przyznajpunkty(k, przyznanopunkty);
    if przyznanopunkty then goto poczatek;
    if (bonus[3] = 1) or (bonus[3] = 2) then goto poczatek;
    policzkulki;
    if nkulek > 77 then break;
    bonusgo1:
    dodajkulki(ADDBALL);
    bonusgo2:
    losujkulki(ADDBALL);
  end;
  if przerwijgre = false then koniecgry;
end;

procedure newgame;
var i,j:integer;
begin
  clrscr;
  nkulek:=0;
  punkty:=0;
  for i:=1 to 9 do
    bonus[i]:=0;
  for i:=1 to 9 do
    for j:=1 to 9 do
      gra[i,j]:=0;
  losujkulki(ADDBALL);
  dodajkulki(ADDBALL);
  losujkulki(ADDBALL);

  continuegame;
end;

procedure loadgame;
var savedgamefile: file of savedgamet;
begin
  assign(savedgamefile, SAVEGAMENAME);
  reset(savedgamefile);
  read(savedgamefile, savedgame);
  close(savedgamefile);

  clrscr;
  nkulek:=0;
  for i:=1 to 9 do
    for j:=1 to 9 do
      gra[i,j]:=savedgame.gra[i,j];
  punkty:=savedgame.punkty;
  for i:=1 to ADDBALL do
    nastkulki[i]:=savedgame.nastkulki[i];
  for i:=1 to 9 do
    bonus[i]:=savedgame.bonus[i];

  continuegame;
end;

procedure savegame;
var savedgamefile: file of savedgamet;
begin
  for i:=1 to 9 do
    for j:=1 to 9 do
      savedgame.gra[i,j]:=gra[i,j];
  savedgame.punkty:=punkty;
  for i:=1 to ADDBALL do
    savedgame.nastkulki[i]:=nastkulki[i];
  for i:=1 to 9 do
    savedgame.bonus[i]:=bonus[i];

 {zapisywanie do pliku}
  assign(savedgamefile, SAVEGAMENAME);
  rewrite(savedgamefile);
  write(savedgamefile, savedgame);
  close(savedgamefile);

  gotoxy(2,14);
  writeln('Gra zapisana    ');
end;

procedure instrukcja;
begin
  clrscr;
  writeln;
  writeln(' ZASADY GRY');
  writeln;
  writeln(' Celem gry jest zdobycie jak najwiekszej liczby punktow.');
  writeln(' Jesli ulozysz co najmniej 5 kulek tego samego koloru obok siebie w jednym');
  writeln(' wierszu, kolumnie lub na skos, kulki znikna, a za kazda dostaniesz 2 punkty.');
  writeln(' Jezeli ulozysz w ten sposob wiecej niz 5 kulek, dostaniesz tez bonusa.');
  writeln(' Gra konczy sie, gdy cala plansza zapelni sie kulkami.');
  writeln;
  writeln(' STEROWANIE');
  writeln;
  writeln(' Kursorem poruszasz za pomoca klawiszy strzalek. Aby przemiescic kulke,');
  writeln(' najedz na nia kursorem i nacisnij spacje. Nastepnie wybierz pole, na');
  writeln(' ktore ma ona trafic i ponownie nacisnij spacje.');
  writeln(' Aby aktywowac bonusa, nacisnij odpowiadajaca mu na liscie bonusow cyfre.');
  writeln;
  writeln(' Milej zabawy!');
  gotoxy(2,19);
  readkey;
end;

procedure koniec;
begin
  clrscr;
end;

procedure podswietl(opcja:integer; kolor:integer);
begin
  textcolor(kolor);
  case opcja of
    1 : begin gotoxy(2,4); write('1. Nowa gra') end;
    2 : begin gotoxy(2,5); write('2. Kontynuuj biezaca gre') end;
    3 : begin gotoxy(2,6); write('3. Zaladuj gre') end;
    4 : begin gotoxy(2,7); write('4. Zapisz gre') end;
    5 : begin gotoxy(2,8); write('5. Wyniki') end;
    6 : begin gotoxy(2,9); write('6. Instrukcja') end;
  end;
  textcolor(7);
  gotoxy(2,opcja+3);
end;

procedure menu;
var exitgame:boolean;
    c:char;
    ch:array [1..length(CHEATCODE)] of char;
    i,opcja:integer;
begin
  if bonus[8] = 0 then kuldouloz:=5;
  for i:=1 to length(CHEATCODE) do
    ch[i]:=' ';
  opcja:=1;
  grazaczeta:=false;
  przerwanie:=false;
  exitgame:=false;
  if not istniejeplik(HSFILENAME) then newhighscore;
  repeat
    clrscr;
    textcolor(7);
    writeln;
    writeln( ' - KULKI 1.3 -');
    writeln;
    writeln(' 1. Nowa gra');
    if not grazaczeta then textcolor(8);
    writeln(' 2. Kontynuuj biezaca gre');
    textcolor(7);
    if not istniejeplik(SAVEGAMENAME) then textcolor(8);
    writeln(' 3. Zaladuj gre');
    textcolor(7);
    if not grazaczeta then textcolor(8);
    writeln(' 4. Zapisz gre');
    textcolor(7);
    writeln(' 5. Wyniki');
    writeln(' 6. Instrukcja');
    writeln;
    writeln;
    writeln(' Esc - wyjscie');
    repeat
      case opcja of {zeby nie zaznaczyc nieaktywnego}
	2 : if not grazaczeta then opcja:=1;
	3 : if not istniejeplik(SAVEGAMENAME) then opcja:=1;
	4 : if not grazaczeta then opcja:=1;
      end;
      podswietl(opcja,15);
      c:=readkey;
      case c of
	#0  : begin
		case readkey of
		'H' : begin
		  	podswietl(opcja,7);
			case opcja of	{gora}
			  0 : opcja:=6;
			  1 : opcja:=6;
			  2 : opcja:=1;
			  3 : if grazaczeta then opcja:=2
				else opcja:=1;
			  4 : if istniejeplik(SAVEGAMENAME) then opcja:=3
				else if grazaczeta then opcja:=2
				else opcja:=1;
			  5 : if grazaczeta then opcja:=4
				else if istniejeplik(SAVEGAMENAME) then opcja:=3
				else opcja:=1;
			  6 : opcja:=5;
			end;
		      end;
		'P' : begin
		  	podswietl(opcja,7);
		  	case opcja of	{dol}
			  0 : opcja:=1;
			  1 : if grazaczeta then opcja:=2
				else if istniejeplik(SAVEGAMENAME) then opcja:=3
				else opcja:=5;
			  2 : if istniejeplik(SAVEGAMENAME) then opcja:=3
				else if grazaczeta then opcja:=4
				else opcja:=5;
			  3 : if grazaczeta then opcja:=4
				else opcja:=5;
			  4 : opcja:=5;
			  5 : opcja:=6;
			  6 : opcja:=1;
			end;
		      end;
		end;
	      end;
	' ' : case opcja of
		0 : ;
		1 : begin grazaczeta:=true; newgame; break end;
		2 : if grazaczeta then begin continuegame; break end;
		3 : if istniejeplik(SAVEGAMENAME) then begin grazaczeta:=true; loadgame; break end;
		4 : if grazaczeta then begin savegame; podswietl(opcja,15); delay(1200); break end;
		5 : begin showhighscore(0); break end;
		6 : begin instrukcja; break end;
	      end;
	#13 : case opcja of
		0 : ;
		1 : begin grazaczeta:=true; newgame; break end;
		2 : if grazaczeta then begin continuegame; break end;
		3 : if istniejeplik(SAVEGAMENAME) then begin grazaczeta:=true; loadgame; break end;
		4 : if grazaczeta then begin savegame; podswietl(opcja,15); delay(1200); break end;
		5 : begin showhighscore(0); break end;
		6 : begin instrukcja; break end;
	      end;
	'1' : begin grazaczeta:=true; newgame; break end;
	'2' : if grazaczeta then begin continuegame; break end;
	'3' : if istniejeplik(SAVEGAMENAME) then begin grazaczeta:=true; loadgame; break end;
	'4' : if grazaczeta then begin savegame; podswietl(opcja,15); delay(1200); break end;
	'5' : begin showhighscore(0); break end;
	'6' : begin instrukcja; break end;
	#27 : begin
		gotoxy(2,12);
		writeln('Czy na pewno chcesz wyjsc? (T/N)');
		podswietl(opcja,7);
		gotoxy(2,12);
		case readkey of
		  't' : begin exitgame:=true; break end;
		  'y' : begin exitgame:=true; break end;
		  'T' : begin exitgame:=true; break end;
		  'Y' : begin exitgame:=true; break end;
		  #27 : begin exitgame:=true; break end;
		  #13 : begin exitgame:=true; break end;
		  ' ' : begin exitgame:=true; break end;
		  else break
		end;
	      end;
      end;
      {Wlaczanie i wylaczanie CHEATMODE}
      for i:=1 to (length(CHEATCODE)-1) do
	ch[i]:=ch[i+1];
      ch[length(CHEATCODE)]:=c;
      if (ch = CHEATCODE) and (CHEATMODE = false) then
      begin
        CHEATMODE:=true;
	gotoxy(2,14);
	writeln('Kody wlaczone   ');
	podswietl(opcja,15);
	delay(1500); break
      end
      else if (ch = CHEATCODE) and (CHEATMODE = true) then
      begin
	CHEATMODE:=false;
	gotoxy(2,14);
	writeln('Kody wylaczone  ');
	podswietl(opcja,15);
	delay(1500); break
      end;
    until przerwanie;
  until exitgame;
  koniec;
end;



{program glowny}
begin
  randomize;
  CHEATMODE:=false; {wlaczanie bonusow klawiszami QWERTY}
  ustalkolory;
  menu;
end.
