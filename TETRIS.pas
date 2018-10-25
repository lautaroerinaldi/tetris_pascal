
{***************************************************************************}
{***************************************************************************}

{ESTE ES EL TETRIS, EL TP N§ 1 DE ALGORITMOS Y PROGRAMACION 1}
{PROGRAMA ESCRITO POR: LAUTARO EZEQUIEL RINALDI}
{FECHA: 12 DE MAYO DE 2008}
{NOMBRE DEL ARCHIVO: TETRIS.pas}

{***************************************************************************}
{***************************************************************************}

PROGRAM TETRIS;
USES
	CRT, DOS, SYSUTILS, STRUTILS, KEYBOARD, VIDEO;
CONST
	XMIN= 34;
	PRICOLFICHA= 0;
	ULTFILAFICHA= 3;
	X1= '01222100';
	X2= '00012221';
	Y1= '33321112';
	Y2= '12333211';
TYPE
   T_MATFICHA= ARRAY[0..3, 0..3] OF BOOLEAN;
   T_VECFICHAS= ARRAY[0..6] OF STRING;
   T_MATTABLERO= ARRAY[0..9, 0..19] OF BOOLEAN;

{***************************************************************************}
{***************************************************************************}

{ACTUA COMO UN READKEY PERO TIENE LA VENTAJA QUE TAMBIEN LEE LAS TECLAS DE FUNCION.
AL USAR LA UNIT KEYBOARD Y ESTA FUNCION, ESTA ACTUA COMO UN DRIVER, Y READKEY NO FUNCIONA MAS.}
FUNCTION LEERTECLA: STRING;
VAR
   K: TKEYEVENT;
BEGIN
   LEERTECLA:= '';
   K:= POLLKEYEVENT; {LEE EL PROXIMO EVENTO DEL TECLADO, Y LO GUARDA EN LA LISTA DE EVENTOS PENDIENTES. PERO NO ESPERA HASTA QUE SE PRESIONE UNA TECLA}

   IF (K <> 0) THEN
   BEGIN
      K:= GETKEYEVENT; {BORRA EL EVENTO DEL BUFFER DEL TECLADO, Y HABILITA PARA LEER LA SIGUIENTE TECLA. SIN ESTA INSTRUCCION, ES COMO SI LA TECLA QUEDASE APRETADA.}
      K:= TRANSLATEKEYEVENT(K); {ME CONVIERTE EL CODIGO QUE ME DEVUELVE EL TECLADO EN UNO QUE PUEDEN IDENTIFICAR LAS FUNCIONES}

      CASE GETKEYEVENTFLAGS(K) OF {SELECCIONA SEGUN EL TIPO DE TECLA PRESIONADA}
         KBASCII: LEERTECLA:= GETKEYEVENTCHAR(K); {CONVIERTE EL CODIGO EN EL CARACTER}
         KBFNKEY: LEERTECLA:= FUNCTIONKEYNAME(GETKEYEVENTCODE(K)) {ME DEVUELVE EL NOMBRE DE LA TECLA DE FUNCION PRESIONADA}
      END
   END
END;

{***************************************************************************}
{***************************************************************************}

{EN ESTE PROCEDIMIENTO LE DIGO AL PROGRAMA QUE FORMA TIENEN LAS FICHAS.
LA FORMA DE CADA UNA DE ELLAS LA GUARDO EN VECFICHAS, EN LUGAR DE USAR
UNA VARIABLE O MATRIZ PARA CADA UNA.}
PROCEDURE GUARDARFICHASEN(VAR VECFICHAS: T_VECFICHAS);
BEGIN
   VECFICHAS[0]:= '0000000011001100'; {CUADRADO = FICHA 0}
   VECFICHAS[1]:= '0000000010001110'; {ELE = FICHA 1}
   VECFICHAS[2]:= '0000000000101110'; {L REFLEJADA HORIZONTALMENTE = FICHA 2}
   VECFICHAS[3]:= '0000000001101100'; {ESE = FICHA 3}
   VECFICHAS[4]:= '0000000011000110'; {ZETA = FICHA 4}
   VECFICHAS[5]:= '0000000001001110'; {TE AL REVES = FICHA 5}
   VECFICHAS[6]:= '0000000000001111' {LINEA (COLUMNA) = FICHA 6}
END;

{***************************************************************************}
{***************************************************************************}

{EN ESTE PROCEDIMIENTO PASO LA FORMA DE UNA FICHA ALEATORIA DESDE UNA POSICIàN
DEL VECTOR VECFICHAS A LA MATRIZ QUE CONTIENE A LA FICHA EN JUEGO. EL NUMALEA-
TORIO DEBE ESTAR COMPRENDIDO ENTRE 0 Y 6 INCLUYENDO LOS EXTREMOS. EN RESUMEN,
CAMBIO LA FICHA EN JUEGO.}
PROCEDURE PROXIMAFICHA(NUMALEATORIO: BYTE; VECFICHAS: T_VECFICHAS; VAR MATFICHA: T_MATFICHA);
VAR
   CONT, Y, X: BYTE;
BEGIN
   CONT:= 0;
   FOR Y:= 0 TO 3 DO
      FOR X:= 0 TO 3 DO
      BEGIN
         INC(CONT);
         MATFICHA[X, Y]:= BOOLEAN(STRTOINT(VECFICHAS[NUMALEATORIO][CONT]))
      END
END;

{***************************************************************************}
{***************************************************************************}

{EN ESTE PROCEDIMIENTO CREO EL TABLERO DE JUEGO. LA VARIABLE XMIN GUARDA LA
LA PRIMERA POSICION DE X DEL TABLERO (0) EN LA QUE PUEDE HABER UNA FICHA.
ESTA HECHO EXCLUSIVAMENTE PARA TABLEROS DE 10 COLUMNAS POR 20 FILAS.}
PROCEDURE CREARTABLERO;
VAR
   X: BYTE;
   CADENAX, CADENAY, CARACTER: STRING;
BEGIN
   CLRSCR;
   TEXTCOLOR(12);
   CADENAX:= '-01+02+07+10-01+10';
   CADENAY:= '000000000000+21+21';
   CARACTER:= '201188200187200188';
   X:= XMIN;

   WHILE (X <= (XMIN+19)) DO
   BEGIN
      IF (X <= (XMIN+9)) THEN
      BEGIN
         GOTOXY(X, 23);
         WRITE(CHR(205));

         IF ((X < (XMIN+2)) OR (X > (XMIN+7))) THEN
         BEGIN
            GOTOXY(X, 2);
            WRITE(CHR(205))
         END
         ELSE
         BEGIN
            GOTOXY(XMIN+STRTOINT(MIDSTR(CADENAX, (((X-XMIN-2)*3)+1),3)), (2+(STRTOINT(MIDSTR(CADENAY, (((X-XMIN-2)*3)+1),3)))));
            WRITE(CHR(STRTOINT(MIDSTR(CARACTER, (((X-XMIN-2)*3)+1),3))))
         END
      END;

      GOTOXY((XMIN-1), (X-XMIN+3));
      WRITE(CHR(186));
      GOTOXY((XMIN+10), (X-XMIN+3));
      WRITE(CHR(186));
      INC(X)
   END
END;

{***************************************************************************}
{***************************************************************************}

{OBTIENE LAS COORDENADAS DE LA FICHA DENTRO DE LA MATRIZ EN LA QUE SE ENCUEN-
TRA (MATFICHA)}
PROCEDURE DONDEESTALAFICHA(MATFICHA: T_MATFICHA; VAR PRIFILAFICHA, ULTCOLFICHA: BYTE);
VAR
   X, Y: BYTE;
BEGIN
   PRIFILAFICHA:= 3;
   ULTCOLFICHA:= 0;

   FOR Y:= 0 TO 3 DO
      FOR X:= 0 TO 3 DO
         IF (MATFICHA[X, Y]) THEN
         BEGIN
            IF (PRIFILAFICHA > Y) THEN
               PRIFILAFICHA:= Y;
            IF (ULTCOLFICHA < X) THEN
               ULTCOLFICHA:= X
         END
END;

{***************************************************************************}
{***************************************************************************}

{MUSTRA O BORRA LA MATRIZ QUE CONTIENE A LA FICHA EN JUEGO EN PANTALLA.
NUMBAJADAS ES EL NUMERO DE VECES QUE BAJO LA FICHA. SI NUMCARACTER ES 0 ENTON-
CES BORRA LA FICHA, SINO, USA EL ASCII DE NUMCARACTER PARA MOSTRAR LA FICHA.}
PROCEDURE MOSTRARFICHA(MATFICHA: T_MATFICHA; NUMBAJADAS, NUMCARACTER, AQUEX, CENTRO: BYTE);
VAR
   X, Y: BYTE;
BEGIN
   FOR Y:= 3 DOWNTO 0 DO
      FOR X:= 0 TO 3 DO
      BEGIN
         IF (MATFICHA[X, Y]) AND ((NUMBAJADAS+Y) > 2) THEN {PIDO MAYOR QUE SEA 2 PORQUE LA PRIMER FILA DEL TABLERO ES LA 3. MUESTRA LA FICHA INCREMENTALMENTE.}
         BEGIN
            GOTOXY((X+AQUEX+CENTRO), (NUMBAJADAS+Y));
            WRITE(CHR(NUMCARACTER))
         END
      END
END;

{***************************************************************************}
{***************************************************************************}

{INICIALIZO TODAS LAS POSICIONES DE LA MATRIZ PARA QUE LAS COMPARACIONES NO
HAYA ERRORES LOGICOS}
PROCEDURE VACIARTABLERO(VAR MATTABLERO: T_MATTABLERO);
VAR
   X, Y: BYTE;
BEGIN
   FOR Y:= 0 TO 19 DO
      FOR X:= 0 TO 9 DO
         MATTABLERO[X,Y]:= FALSE
END;

{***************************************************************************}
{***************************************************************************}

PROCEDURE CARGARTABLERO(VAR MATTABLERO: T_MATTABLERO; MATFICHA: T_MATFICHA; NUMBAJADAS, DERIZQ, CENTRO: BYTE);
VAR
   X, Y: BYTE;
BEGIN
   FOR Y:= 0 TO 3 DO
      FOR X:= 0 TO 3 DO
         IF ((MATFICHA[X, Y]) AND ((NUMBAJADAS-3+Y)>=0)) AND (((DERIZQ-XMIN+X+CENTRO)>=0) AND ((DERIZQ-XMIN+X+CENTRO)<=9))  THEN
            MATTABLERO[(DERIZQ-XMIN+X+CENTRO), (NUMBAJADAS-3+Y)]:= TRUE
END;

{***************************************************************************}
{***************************************************************************}

{CONTROLA SI LA FICHA EN JUEGO VA A SUPERPONERSE (VERTICALMENTE) O NO CON
LA SIGUIENTE LINEA DEL TABLERO.}
FUNCTION SESUPERPONEV(MATTABLERO: T_MATTABLERO; COLPOS1FICHA, POSULTFILAFICHA, ULTCOLFICHA: BYTE; MATFICHA: T_MATFICHA): BOOLEAN;
VAR
   X, Y: BYTE;
   SW: BOOLEAN;
BEGIN
   Y:= 0;
   SW:= FALSE;

   IF (POSULTFILAFICHA < 19) THEN
      WHILE (((NOT(SW)) AND ((ULTFILAFICHA-Y) >= 0)) AND ((POSULTFILAFICHA+ULTFILAFICHA-2-Y) >= 0)) DO
      BEGIN
         X:= 0;
         WHILE (NOT((SW)) AND (X <= (ULTCOLFICHA-PRICOLFICHA))) DO
         BEGIN
            IF (MATFICHA[(X+PRICOLFICHA), (ULTFILAFICHA-Y)]) AND (MATFICHA[(X+PRICOLFICHA), (ULTFILAFICHA-Y)] = MATTABLERO[(X+COLPOS1FICHA+PRICOLFICHA), (POSULTFILAFICHA+ULTFILAFICHA-2-Y)]) THEN
               SW:= TRUE;
            INC(X)
         END;
         INC(Y)
      END
   ELSE
      SW:= TRUE;

   SESUPERPONEV:= SW
END;

{***************************************************************************}
{***************************************************************************}

{CONTROLA SI LA FICHA EN JUEGO VA A SUPERPONERSE (HORIZONTALMENTE) CON ALGUNA
OTRA FICHA DEL TABLERO}
FUNCTION SESUPERPONEH(MATTABLERO: T_MATTABLERO; COLPOS1FICHA, POSULTFILAFICHA, ULTCOLFICHA: BYTE; MATFICHA: T_MATFICHA; SUMOORESTO: INTEGER): BOOLEAN;
VAR
   X, Y: BYTE;
   SW: BOOLEAN;
BEGIN
   Y:= 0;
   SW:= FALSE;

   WHILE ((NOT((SW)) AND ((ULTFILAFICHA-Y) >= 0)) AND ((POSULTFILAFICHA+ULTFILAFICHA-3-Y) >= 0)) DO
   BEGIN
      X:= 0;
      WHILE (NOT((SW)) AND (X <= (ULTCOLFICHA-PRICOLFICHA))) DO
      BEGIN
         IF (MATFICHA[(X+PRICOLFICHA), (ULTFILAFICHA-Y)]) AND (MATFICHA[(X+PRICOLFICHA), (ULTFILAFICHA-Y)] = MATTABLERO[(X+COLPOS1FICHA+PRICOLFICHA+SUMOORESTO), (POSULTFILAFICHA+ULTFILAFICHA-3-Y)]) THEN
            SW:= TRUE;
         INC(X)
      END;
      INC(Y)
   END;

   SESUPERPONEH:= SW
END;

{***************************************************************************}
{***************************************************************************}
{ME FIJO SI LA TECLA PRESIONADA ES UN DE MI INTERES}
FUNCTION TECLAVALIDA(TECLA: STRING): CHAR;
BEGIN
   TECLA:= UPCASE(TECLA);

   IF (TECLA= 'RIGHT') OR (TECLA= 'LEFT') OR (TECLA= 'DOWN') OR (ORD(TECLA[1])= 27) OR (TECLA= 'Z') THEN
      TECLAVALIDA:= TECLA[1]
   ELSE
      TECLAVALIDA:= ' '
END;

{***************************************************************************}
{***************************************************************************}
{ROTACION DE TODAS LAS FICHAS Y SU VALIDACION}
PROCEDURE GIRA3X3(VAR MO: T_MATFICHA; NUMBAJADAS, DERIZQ, CENTRO, ULTCOLFICHA: BYTE; MATTABLERO: T_MATTABLERO; ALEATORIO0: BYTE);
VAR
   X, Y: BYTE;
   MC: T_MATFICHA;
   SW: BOOLEAN;
BEGIN
   MC:= MO;
   SW:= FALSE;
   IF (((ULTCOLFICHA= 1) AND ((DERIZQ + ULTCOLFICHA + CENTRO) < (XMIN + 9))) OR (ULTCOLFICHA = 2) OR (ULTCOLFICHA= 3) OR ((DERIZQ + ULTCOLFICHA + CENTRO) < (XMIN + 7))) AND (ALEATORIO0 <> 0) THEN
   BEGIN
      IF (ALEATORIO0 <> 6) THEN
      BEGIN
         FOR X:= 1 TO 8 DO
            MC[STRTOINT(X1[X]), STRTOINT(Y1[X])]:= MO[STRTOINT(X2[X]), STRTOINT(Y2[X])];

         FOR X:= 1 TO 3 DO
            IF MC[0, X] THEN
               SW:= TRUE;
         IF NOT(SW) THEN
         BEGIN
            FOR Y:= 0 TO 2 DO
               FOR X:= 0 TO 3 DO
                  MC[Y, X]:= MC[Y+1, X];
            FOR X:= 0 TO 3 DO
               MC[3, X]:= FALSE
         END;

         SW:= FALSE;
         FOR X:= 0 TO 2 DO
            IF MC[X, 3] THEN
               SW:= TRUE;
            IF NOT(SW) THEN
            BEGIN
               FOR X:= 3 DOWNTO 1 DO
                  FOR Y:= 0 TO 3 DO
                     MC[Y, X]:= MC[Y, X-1];
               FOR X:= 0 TO 3 DO
                  MC [X, 0]:= FALSE
            END
      END
      ELSE
      BEGIN
         IF (MO[0, 0]) THEN
            FOR X:= 1 TO 3 DO
            BEGIN
               MC[X, 3]:= TRUE;
               MC[0, X-1]:= FALSE
            END
         ELSE
         BEGIN
            FOR X:= 1 TO 3 DO
            BEGIN
               MC[X, 3]:=FALSE;
               MC[0, X-1]:=TRUE
            END;
            MC[0, 3]:= TRUE
         END
      END;

      SW:= TRUE;
      FOR X:= 0 TO 3 DO
         FOR Y:= 0 TO 3 DO
            IF ((MC[X, Y]) AND ((NUMBAJADAS-3+Y) >= 0)) AND (((DERIZQ-XMIN+X+CENTRO) >= 0) AND ((DERIZQ-XMIN+X+CENTRO) <= 9)) THEN
               IF (MATTABLERO[DERIZQ-XMIN+X+CENTRO, NUMBAJADAS-3+Y]) THEN
                  SW:= FALSE;
      IF SW THEN
      BEGIN
         MOSTRARFICHA(MO, NUMBAJADAS, 0 , DERIZQ, CENTRO);
         MOSTRARFICHA(MO, 6, 0, (XMIN+18), 0);
         TEXTCOLOR(14);
         MOSTRARFICHA(MC, 6, 2, (XMIN+18), 0);
         MO:= MC
      END
   END
END;

{***************************************************************************}
{***************************************************************************}
{SELECCIONO QUE ACCIONES REALIZAR SEGUN LA TECLA PRESIONADA. CAMBIA EL TIEMPO
POR NIVEL}
PROCEDURE TECLADO(MATTABLERO: T_MATTABLERO; VAR MATFICHA:T_MATFICHA; VAR ESCAPE: BOOLEAN; VAR DERIZQ, ULTCOLFICHA, PRIFILAFICHA: BYTE; CENTRO, NUMBAJADAS, ALEATORIO0, NIVEL: BYTE);
VAR
   TECLA: CHAR;
   SWTECLA: BOOLEAN;
   DERIZQANT, X: BYTE;
BEGIN
   X:= 0;
   REPEAT
      TECLA:= TECLAVALIDA(LEERTECLA);
      SWTECLA:= FALSE;

      IF (TECLA <> ' ') THEN
      BEGIN
         DERIZQANT:= DERIZQ;

         CASE TECLA OF
            'R':
               IF (((DERIZQ+ULTCOLFICHA+CENTRO) < (XMIN+9)) AND (NOT(SESUPERPONEH(MATTABLERO, (DERIZQ-XMIN+CENTRO), NUMBAJADAS, ULTCOLFICHA, MATFICHA, 1)))) THEN
               BEGIN
                  DERIZQ:= DERIZQ + 1;
                  SWTECLA:= TRUE
               END;
            'L':
               IF (((DERIZQ+PRICOLFICHA+CENTRO) > XMIN) AND (NOT(SESUPERPONEH(MATTABLERO, (DERIZQ-XMIN+CENTRO), NUMBAJADAS, ULTCOLFICHA, MATFICHA, -1)))) THEN
               BEGIN
                  DERIZQ:= DERIZQ - 1;
                  SWTECLA:= TRUE
               END;
            'D':
               INC(X, 5);
            'Z':
               BEGIN
                  GIRA3X3(MATFICHA, NUMBAJADAS, DERIZQ, CENTRO, ULTCOLFICHA, MATTABLERO, ALEATORIO0);
                  DONDEESTALAFICHA(MATFICHA, PRIFILAFICHA, ULTCOLFICHA);
                  SWTECLA:= TRUE;
               END;
            CHR(27):
               BEGIN
                  GOTOXY(9, 24);
                  TEXTCOLOR(10);
                  WRITE('°°°°°±±±±±²²²²² ');
                  TEXTCOLOR(14);
                  WRITE('CONFIRMA QUE DESEA SALIR (S/N)');
                  TEXTCOLOR(10);
                  WRITE(' ²²²²²±±±±±°°°°°');
                  CASE UPCASE(GETKEYEVENTCHAR(TRANSLATEKEYEVENT(GETKEYEVENT))) OF
                     'S':
                           ESCAPE:= TRUE
                     ELSE
                        BEGIN
                           GOTOXY(1, 24);
                           CLREOL;
                           TEXTCOLOR(14)
                        END
                  END
               END
         END;

         IF (SWTECLA) THEN
         BEGIN
            MOSTRARFICHA(MATFICHA, NUMBAJADAS, 0 , DERIZQANT, CENTRO);
            TEXTCOLOR(10);
            MOSTRARFICHA(MATFICHA, NUMBAJADAS, 2 , DERIZQ, CENTRO)
         END
      END;
      DELAY(25 - (5 * (NIVEL-1)));
      INC(X)
   UNTIL ((X >= 15) OR (ESCAPE))
END;

{***************************************************************************}
{***************************************************************************}
{CONTROLO SI EL JUGADOR PERDIO}
FUNCTION PERDISTE(MATTABLERO: T_MATTABLERO; MATFICHA, MATFICHA2: T_MATFICHA; PRIFILAFICHA, ULTCOLFICHA, CENTRO, NUMBAJADAS: BYTE): BOOLEAN;
VAR
   X, Y: BYTE;
BEGIN
   X:= 0;
   PERDISTE:= FALSE;

   FOR Y:= 0 TO 3 DO
      FOR X:= 0 TO 3 DO
         IF((MATFICHA[X, Y]) AND ((NUMBAJADAS-3+Y)<0)) THEN
            PERDISTE:= TRUE;

   DONDEESTALAFICHA(MATFICHA2, PRIFILAFICHA, ULTCOLFICHA);
   CENTRO:= (9 - ULTCOLFICHA + PRICOLFICHA) DIV 2;
   FOR X:= 0 TO 3 DO
      IF (MATFICHA2[X, 3]) AND (MATTABLERO[X+CENTRO, 0]) THEN
         PERDISTE:= TRUE;

   IF PERDISTE THEN
   BEGIN
      DELAY(1000);
      CLRSCR;
      GOTOXY(XMIN-1, 10);
      TEXTCOLOR(11);
      WRITELN('­­PERDISTE!!');
      DONEKEYBOARD;
      DELAY(3000)
   END
END;

{***************************************************************************}
{***************************************************************************}
{FIJO EN NUMERO DE FILAS Y COLUMNAS EN PANTALLA}
PROCEDURE AJUSTOPANTALLA;
VAR
   PANTALLA: TVIDEOMODE;
BEGIN
   INITVIDEO;

   PANTALLA.COL:= 80;
   PANTALLA.ROW:= 25;
   PANTALLA.COLOR:= TRUE;

   SETVIDEOMODE(PANTALLA);

   CURSOROFF {DESHABILITA EL CURSOR TITILANTE EN PANTALLA.}
END;

{***************************************************************************}
{***************************************************************************}
{CONTROLA Y CUENTA LAS LINEAS HORIZONTALES LLENAS}
FUNCTION FILASLLENAS(VAR MATTABLERO: T_MATTABLERO; VAR CONTFILAS: BYTE): BOOLEAN;
VAR
   X, Y, YNOV, CONT: BYTE;
BEGIN
   FILASLLENAS:= FALSE;
   YNOV:= 19;
   REPEAT
      CONT:= 0;
      FOR X:= 0 TO 9 DO
    	   IF MATTABLERO[X, YNOV] THEN INC(CONT);
      IF CONT = 10 THEN
      BEGIN
         INC(CONTFILAS);
    	   IF FILASLLENAS= FALSE THEN FILASLLENAS:= TRUE;
    	   FOR Y:= YNOV DOWNTO 1 DO
      	   FOR X:= 0 TO 9 DO
        	      MATTABLERO[X, Y]:= MATTABLERO[X, Y-1];
         FOR X:= 0 TO 9 DO
      	   MATTABLERO[X, 0]:= FALSE
      END
      ELSE
    	   DEC(YNOV)
   UNTIL ((YNOV = 0) OR (CONT = 0))
END;

{***************************************************************************}
{***************************************************************************}
{ACTUALIZA EL TABLERO EN PANTALLA CON EL TABLERO LOGICO}
PROCEDURE RECARGATABLERO(MATTABLERO: T_MATTABLERO);
VAR
   X, Y, CONT: BYTE;
BEGIN
   FOR Y:= 19 DOWNTO 0 DO
   BEGIN
      FOR X:= 0 TO 9 DO
      BEGIN
         GOTOXY(X + XMIN, Y+3);
         IF MATTABLERO[X, Y] THEN
      	   WRITE(CHR(2))
         ELSE
      	   WRITE(CHR(0))
      END
   END
END;

{***************************************************************************}
{***************************************************************************}
{INCREMENTA NIVELES Y PUNTAJES. GANASTE}
PROCEDURE PUNTOSYNIVEL(VAR CONTFILAS, CONTFILASANT, NIVEL: BYTE; VAR PUNTAC: INTEGER; VAR ESCAPE: BOOLEAN);
BEGIN
   PUNTAC:= PUNTAC + (10 * NIVEL * CONTFILAS);
   CONTFILASANT:= CONTFILASANT + CONTFILAS;
   CONTFILAS:= 0;

   IF (CONTFILASANT >= 5) THEN
   BEGIN
      INC(NIVEL);
      CONTFILASANT:= 0
   END;
   GOTOXY((XMIN + 20), 19);
   WRITE(PUNTAC);
   IF NIVEL > 5 THEN
   BEGIN
      DELAY(1000);
      CLRSCR;
      GOTOXY(XMIN-1, 10);
      TEXTCOLOR(11);
      WRITELN('­­GANASTE!!');
      DONEKEYBOARD;
      ESCAPE:= TRUE;
      DELAY(3000)
   END
   ELSE
   BEGIN
      GOTOXY((XMIN + 20), 22);
      WRITE(NIVEL)
   END
END;

{***************************************************************************}
{***************************************************************************}
{PROGRAMA EN SI MISMO}
PROCEDURE PRINCIPAL;
VAR
   MATFICHA: T_MATFICHA;
   VECFICHAS: T_VECFICHAS;
   MATTABLERO: T_MATTABLERO;
   MATFICHA2: T_MATFICHA;

   NUMBAJADAS, DERIZQ, CENTRO, ALEATORIO0, ALEATORIO1, PRIFILAFICHA, ULTCOLFICHA, CONTFILAS, CONTFILASANT, NIVEL: BYTE;
   PRIMERAVEZ, NOCONTINUA, ESCAPE: BOOLEAN;
   PUNTAC: INTEGER;

BEGIN
   INITKEYBOARD; {INICIALIZO EL DRIVER DEL TECLADO.}

   PRIMERAVEZ:= TRUE;
   ESCAPE:= FALSE;
   CONTFILAS:= 0;
   NIVEL:= 1;
   PUNTAC:= 0;
   CONTFILASANT:=0;

   VACIARTABLERO(MATTABLERO);
   GUARDARFICHASEN(VECFICHAS);
   CREARTABLERO;

   TEXTCOLOR(11);
   GOTOXY((XMIN + 13), 4);
   WRITE('FICHA ACTUAL');
   GOTOXY((XMIN + 13), 11);
   WRITE('PROXIMA FICHA');
   GOTOXY((XMIN + 13), 18);
   WRITE('PUNTAJE:');
   GOTOXY((XMIN+13), 21);
   WRITE('NIVEL:');

   TEXTCOLOR(14);
   GOTOXY((XMIN + 20), 19);
   WRITE(PUNTAC);
   GOTOXY((XMIN + 20), 22);
   WRITE(NIVEL);


   REPEAT
      IF (NOT(PRIMERAVEZ)) THEN
      BEGIN
         TEXTCOLOR(10);
         MOSTRARFICHA(MATFICHA, NUMBAJADAS, 2, DERIZQ, CENTRO);
         TECLADO(MATTABLERO, MATFICHA, ESCAPE, DERIZQ, ULTCOLFICHA, PRIFILAFICHA, CENTRO, NUMBAJADAS, ALEATORIO0, NIVEL);
         NOCONTINUA:= SESUPERPONEV(MATTABLERO, DERIZQ-XMIN+CENTRO, NUMBAJADAS, ULTCOLFICHA, MATFICHA)
      END;
      IF ((NOCONTINUA) OR (PRIMERAVEZ)) THEN
      BEGIN
         IF (NOT(PRIMERAVEZ)) THEN
         BEGIN
            CARGARTABLERO(MATTABLERO, MATFICHA, NUMBAJADAS, DERIZQ, CENTRO);
            TEXTCOLOR(14);
            MOSTRARFICHA(MATFICHA, NUMBAJADAS, 2, DERIZQ, CENTRO);
            IF FILASLLENAS(MATTABLERO, CONTFILAS) THEN
            BEGIN
               RECARGATABLERO(MATTABLERO);
               PUNTOSYNIVEL(CONTFILAS, CONTFILASANT, NIVEL, PUNTAC, ESCAPE)
            END;

            IF PERDISTE(MATTABLERO, MATFICHA, MATFICHA2, PRIFILAFICHA, ULTCOLFICHA, CENTRO, NUMBAJADAS) THEN
               ESCAPE:= TRUE;

            MOSTRARFICHA(MATFICHA, 6, 0, (XMIN+18), 0);
            MOSTRARFICHA(MATFICHA2, 13, 0, (XMIN+18), 0);
            ALEATORIO0:= ALEATORIO1;
            MATFICHA:= MATFICHA2
         END
         ELSE
         BEGIN
            ALEATORIO0:=RANDOM(7);
            PROXIMAFICHA(ALEATORIO0, VECFICHAS, MATFICHA);
            PRIMERAVEZ:= FALSE
         END;
         ALEATORIO1:= RANDOM(7);
         PROXIMAFICHA(ALEATORIO1, VECFICHAS, MATFICHA2);
         MOSTRARFICHA(MATFICHA, 6, 2, (XMIN+18), 0);
         MOSTRARFICHA(MATFICHA2, 13, 2, (XMIN+18), 0);
         NUMBAJADAS:= 0;
         DERIZQ:= XMIN;
         DONDEESTALAFICHA(MATFICHA, PRIFILAFICHA, ULTCOLFICHA);
         CENTRO:= (9 - ULTCOLFICHA + PRICOLFICHA) DIV 2
      END
      ELSE
      BEGIN
         MOSTRARFICHA(MATFICHA, NUMBAJADAS, 0, DERIZQ, CENTRO);
         INC(NUMBAJADAS)
      END
   UNTIL (ESCAPE);
   DONEKEYBOARD;
   CLRSCR
END;

{***************************************************************************}
{***************************************************************************}
{INSTRUCCIONES DE JUEGO}
PROCEDURE ENQUECONSISTE;
BEGIN
   CLRSCR;
   GOTOXY(27, 3);
   TEXTCOLOR(11);
   WRITELN('¨EN QUE CONSISTE EL JUEGO?');
   TEXTCOLOR(15);
   WRITELN;
   WRITELN;
   WRITELN('      Siete tipos de piezas bidimensionales de 4 bloques,  en distintas posi-');
   WRITELN('  ciones, caen desde la parte superior de la pantalla.');
   WRITELN;
   WRITELN('      El jugador las puede rotar (0ø, 90ø, 180ø, 270ø) y/o desplazar horizon-');
   WRITELN('  talmente por una  superficie definida de juego llamada  tablero,  y decidir');
   WRITELN('  como colocarlas de forma que se formen l¡neas completas.');
   WRITELN;
   WRITELN('      Cuando una l¡nea horizontal se completa,  esa l¡nea desaparece, y todas');
   WRITELN('  las piezas que est n por encima descienden una posici¢n,  liberando espacio');
   WRITELN('  de juego,  y por tanto facilitando la tarea de situar nuevas piezas. Pueden');
   WRITELN('  completarse y liberarse varias l¡neas simult neamente.');
   WRITELN;
   WRITELN('      El  juego consta de  5  niveles,  en donde la velocidad de caida de las');
   WRITELN('  piezas se va acelerando, agreg ndole dificultad al juego. La condici¢n para');
   WRITELN('  incrementar el "nivel" es que el jugador complete 5 l¡neas.');
   TEXTCOLOR(14);
   GOTOXY(1, 24);
   WRITE('Presione cualquier tecla para volver al MENU PRINCIPAL');
   READKEY;
   CLRSCR
END;
{***************************************************************************}
{***************************************************************************}
{INSTRUCCIONES DE PROGRAMA}
PROCEDURE COMOJUGAR;
BEGIN
   CLRSCR;
   GOTOXY(39, 3);
   TEXTCOLOR(11);
   WRITELN('¨COMO JUGAR?');
   TEXTCOLOR(15);
   WRITELN;
   WRITELN;
   WRITELN('      Con las flechas de direcci¢n derecha e izquierda,  desplaza las fichas');
   WRITELN('  horizontalmente por la pantalla. Al mantener apretada la  flecha de direc-');
   WRITELN('  ci¢n abajo, la velocidad de ca¡da de la pieza se incrementa.');
   WRITELN;
   WRITELN('      Presione la tecla "Z" para girar la ficha 90§ (en sentido contrario al');
   WRITELN('  de las agujas del reloj).');
   WRITELN;
   WRITELN('      En cualquier momento de la partida puede presionar la tecla  ESC  y el');
   WRITELN('  juego le mostrar  un mensaje, d ndole la oportunidad de abandonar el juego');
   WRITELN('  o continuarlo (con "S" confirma salir, y con cualquier otra tecla continua');
   WRITELN('  la partida actual).');
   TEXTCOLOR(14);
   GOTOXY(1, 24);
   WRITE('Presione cualquier tecla para volver al MENU PRINCIPAL');
   READKEY;
   CLRSCR
END;

{***************************************************************************}
{***************************************************************************}
{MENU PRINCIPAL}
PROCEDURE MENU;
VAR
   A: CHAR;
BEGIN
   CLRSCR;
   REPEAT
      TEXTCOLOR(14);
      GOTOXY(34, 6);
      WRITE(#174' TETRIS '#175);
      GOTOXY(36, 7);
      WRITE('------');

      TEXTCOLOR(10);
      GOTOXY(20, 10);
      WRITE('PRESIONE EL NUMERO DE OPCION DESEADA');

      TEXTCOLOR(15);
      GOTOXY(34, 13);
      WRITE ('1- JUGAR');
      GOTOXY(34, 15);
      WRITE('2- ¨EN QUE CONSISTE EL JUEGO?');
      GOTOXY(34, 17);
      WRITE('3- ¨COMO JUGAR?');
      GOTOXY(34, 19);
      WRITE('4- SALIR');

      A:= READKEY;
      CASE A OF
         '1': PRINCIPAL;
         '2': ENQUECONSISTE;
         '3': COMOJUGAR
      END
   UNTIL (A='4');
   CLRSCR
END;

{***************************************************************************}
{***************************************************************************}
{PROGRAMA PRINCIPAL}
BEGIN
   RANDOMIZE;
   AJUSTOPANTALLA;
   MENU;
   CURSORON
END.

{***************************************************************************}
{***************************************************************************}
