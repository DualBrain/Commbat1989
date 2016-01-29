DECLARE SUB GetDecoy ()
DECLARE SUB DropMine (Howmany%)
DECLARE SUB GetShell (Howmany%)
DECLARE SUB GetLazer (Howmany%)
DECLARE SUB Msg (Txt$)
DECLARE SUB GetMine (Howmany%)
DECLARE SUB UpDateTank2 (V%)
DECLARE SUB UpDateBase ()
DECLARE SUB UpDateDecoy (Number%)
DECLARE SUB GetMap (Maps%)
DECLARE SUB DoCmd ()
DECLARE FUNCTION GetKey% ()
DECLARE SUB UpDateTank (Number%)
DECLARE SUB MoveText (OrgX1%, OrgY1%, OrgX2%, OrgY2%, NewX1%, NewY1%)
DECLARE SUB Clrtxt ()
DECLARE SUB Initgame ()
DECLARE SUB Cursor (a%, b%)
DECLARE SUB Views ()
DECLARE SUB Remote ()
DECLARE SUB EraseTank (Tank%)
DECLARE SUB Convert (Y%, X%, Map%)
DECLARE SUB Showtank (Tank%)
DECLARE SUB Tankview (Tank%)
DECLARE SUB Baseview ()
DECLARE SUB Decoyview (Decoy%)
DECLARE SUB Drawscr ()
DECLARE SUB Movetank (Direction%)
DECLARE SUB Box (Y1%, X1%, Y2%, X2%)
DECLARE SUB PRINTAT (Row%, Column%, Text$)
DECLARE FUNCTION Badmove% (Direction%)
CLS

REM Comm-Bat a two player tank game  by Jim Veneskey 12-18-89 (c)  */


CONST UP = 1
CONST down = 2
CONST Left = 3
CONST Right = 4
DEF SEG = &HB800
REM /* All externally accessed variables are defined here */

DIM SHARED BigMap(84, 84) AS STRING * 1, MineField%(84, 84)
DIM SHARED TankX%(8), TankY%(8), TankM%(8), Twas$(8), Hold%(8)
DIM SHARED DecoyX%(3), DecoyY%(3), DecoyM%(3), US$, Drones%, Nukes%
DIM SHARED Shields%(9), Mines%(11), Shells%(11), Lazers%(11), Dekes%(8), Map%
DIM SHARED BaseX%, BaseY%, BaseM%, RealX%, RealY%, ZX%, Tank%, Decoy%, Cmd$
DIM Loopy%, Loopx%, Keyin%
Tank% = 1: Decoy% = 1: Map% = 1
US$ = "##  ##  # ### ### ### ###  #"

REM /* Initialize BigMap array to all blanks     */
	FOR Loopy% = 1 TO 84
	    FOR Loopx% = 1 TO 84
		  BigMap$(Loopy%, Loopx%) = " "
	    NEXT
	NEXT


	FOR Loopy% = 1 TO 2
	    FOR Loopx% = 1 TO 84
	       BigMap$(Loopy%, Loopx%) = "#"
	       BigMap$(Loopy% + 82, Loopx%) = "#"
	    NEXT
	NEXT


	FOR Loopx% = 1 TO 2
	   FOR Loopy% = 1 TO 84
	    BigMap$(Loopy%, Loopx%) = "#"
	    BigMap$(Loopy%, Loopx% + 82) = "#"
	    NEXT
	NEXT
	


	FOR Loopx% = 1 TO 8
	   Shields%(Loopx%) = 100
	   Hold%(Loopx%) = 300
	   Twas$(Loopx%) = "B"
	NEXT
	
	Shields%(0) = 500
	Mines%(0) = 300
	Shells%(0) = 300
	Lazers%(0) = 300
	Dekes%(0) = 3
	Drones% = 3
	Nukes% = 1


REM /*********** Main programming starts here ***********/


	CALL Drawscr
	CALL Initgame
	CALL Cursor(1, 0)
	CALL Views
	WHILE (Keyin% <> 27)
	
	Keyin% = GetKey%
	SELECT CASE Keyin%
		
		CASE 172: CALL Movetank(UP)
		CASE 180: CALL Movetank(down)
		CASE 175: CALL Movetank(Left)
		CASE 177: CALL Movetank(Right)
		CASE 182: CALL Clrtxt

	END SELECT
       
	CALL Remote
	WEND

CALL Cursor(12, 13): REM      /* Show Cursor */
REM is this the
END

FUNCTION Badmove% (Direction%)
REM /* This function tests the legality of a move - if it will hit a wall. */
CALL Convert(TankY%(Tank%), TankX%(Tank%), TankM%(Tank%))

SELECT CASE Direction%
CASE UP:    IF VAL(BigMap$(RealY% - 1, RealX%)) > 0 THEN Badmove% = 3: GOTO BadOut
CASE down:  IF VAL(BigMap$(RealY% + 1, RealX%)) > 0 THEN Badmove% = 3: GOTO BadOut
CASE Left:  IF VAL(BigMap$(RealY%, RealX% - 1)) > 0 THEN Badmove% = 3: GOTO BadOut
CASE Right: IF VAL(BigMap$(RealY%, RealX% + 1)) > 0 THEN Badmove% = 3: GOTO BadOut
END SELECT

	IF (Direction% = Left AND TankX%(Tank%) > 1) THEN
	    Badmove% = 0: GOTO BadOut
	    END IF
       
	IF (Direction% = Left AND (1 - (TankM%(Tank%) MOD 2))) THEN
	      CALL EraseTank(Tank%)
	      TankX%(Tank%) = 40
	      TankM%(Tank%) = TankM%(Tank%) - 1
	      Badmove% = 2: GOTO BadOut
	    END IF
	   
	    IF (Direction% = Left) THEN
	      Badmove% = 1: GOTO BadOut
	      END IF

	IF (Direction% = Right AND TankX%(Tank%) < 40) THEN
	    Badmove% = 0: GOTO BadOut
	    END IF

	IF (Direction% = Right AND (TankM%(Tank%) MOD 2)) THEN
	      CALL EraseTank(Tank%)
	      TankX%(Tank%) = 1
	      TankM%(Tank%) = TankM%(Tank%) + 1
	      Badmove% = 2: GOTO BadOut
	     END IF

	IF (Direction% = Right) THEN
	      Badmove% = 1: GOTO BadOut
	      END IF

	IF (Direction% = UP AND TankY%(Tank%) > 1) THEN
	      Badmove% = 0: GOTO BadOut
	      END IF

	IF (Direction% = UP AND TankM%(Tank%) > 2) THEN
	      CALL EraseTank(Tank%)
	      TankY%(Tank%) = 20
	      TankM%(Tank%) = TankM%(Tank%) - 2
	      Badmove% = 2: GOTO BadOut
	      END IF

	IF (Direction% = UP) THEN
	      Badmove% = 1: GOTO BadOut
	      END IF


	IF (Direction% = down AND TankY%(Tank%) < 20) THEN
	      Badmove% = 0: GOTO BadOut
	      END IF

	IF (Direction% = down AND TankM%(Tank%) < 7) THEN
	      CALL EraseTank(Tank%)
	      TankY%(Tank%) = 1
	      TankM%(Tank%) = TankM%(Tank%) + 2
	      Badmove% = 2: GOTO BadOut
	      END IF
	      
	IF (Direction% = down) THEN
	      Badmove% = 1: GOTO BadOut
	      END IF

BadOut:
END FUNCTION

SUB Baseview
REM /* Update Base viewport */
	CALL Convert(BaseY%, BaseX%, BaseM%)
	FOR X% = 0 TO 4
	   FOR Y% = 0 TO 4
	     LOCATE 17 + Y%, 17 + X%
	     PRINT BigMap$(RealY% - 2 + Y%, RealX% - 2 + X%);
	   NEXT
	NEXT

END SUB

SUB Clrtxt
REM /* This routine simply clears out the old messages */

	LOCATE 24, 3
	PRINT "                                            ";

END SUB

SUB Convert (Y%, X%, Map%)
REM /* Convert Tank Psuedo co-ords into actual BigMap Co-ords */
	IF (Map% MOD 2) THEN RealX% = X% + 2 ELSE RealX% = X% + 42
	RealY% = Y% + 2 + ((INT((Map% + 1) / 2) - 1) * 20)

END SUB

SUB Cursor (a%, b%)
REM /* Cursor - adjusts the size of cursor in scan lines */
REM char a,b;
REM
REM         union REGS r;
REM         r.h.ah = 1;
REM         r.h.ch = a;
REM         r.h.cl = b;
REM         int86(0x10,&r,&r);
REM         return(0);
REM

END SUB

SUB Decoyview (Decoy%)
REM /* Update Decoy viewport */
IF DecoyM%(Decoy%) = 0 THEN GOTO DecoyOut
	CALL Convert(DecoyY%(Decoy%), DecoyX%(Decoy%), DecoyM%(Decoy%))
	FOR X% = 0 TO 4
	   FOR Y% = 0 TO 4
	      LOCATE 17 + Y%, 5 + X%
	      PRINT BigMap$(RealY% - 2 + Y%, RealX% - 2 + X%);
	   NEXT
	NEXT
DecoyOut:
LOCATE 16, 8: PRINT CHR$(48 + Decoy%);
END SUB

SUB DoCmd
STATIC LastCmd$
LOCATE 14, 2
Cmd$ = LTRIM$(Cmd$)
LOCATE 14, 2: IF LEN(Cmd$) = 0 THEN CALL PRINTAT(14, 2, "*** Illegal Command ***"): GOTO DoOut
Cmd$ = UCASE$(Cmd$)
IF Cmd$ = "QUIT" THEN STOP
IF Cmd$ = "R" THEN Cmd$ = LastCmd$
IF LEFT$(Cmd$, 2) = "SM" THEN Parm% = VAL(RIGHT$(Cmd$, 1)): IF Parm% > 0 AND Parm% < 9 THEN CALL GetMap(Parm%)
IF LEFT$(Cmd$, 2) = "ST" THEN Parm% = VAL(RIGHT$(Cmd$, 1)): IF Parm% > 0 AND Parm% < 9 THEN Tank% = Parm%: CALL Views: CALL UpDateTank2(Tank%)
IF LEFT$(Cmd$, 2) = "SD" THEN Parm% = VAL(RIGHT$(Cmd$, 1)): IF Parm% > 0 AND Parm% < 4 THEN Decoy% = Parm%: CALL Views: CALL UpDateDecoy(Decoy%)
IF LEFT$(Cmd$, 2) = "LM" THEN Parm% = VAL(RIGHT$(Cmd$, 1)): IF Parm% > 0 THEN CALL GetMine(Parm%)
IF LEFT$(Cmd$, 2) = "DM" THEN Parm% = VAL(RIGHT$(Cmd$, 1)): IF Parm% > 0 THEN CALL DropMine(Parm%)
IF LEFT$(Cmd$, 2) = "LS" THEN Parm% = VAL(RIGHT$(Cmd$, 1)): IF Parm% > 0 THEN CALL GetShell(Parm%)
IF LEFT$(Cmd$, 2) = "LL" THEN Parm% = VAL(RIGHT$(Cmd$, 1)): IF Parm% > 0 THEN CALL GetLazer(Parm%)
IF LEFT$(Cmd$, 2) = "LD" THEN CALL GetDecoy
LastCmd$ = Cmd$
LOCATE 14, 2: PRINT "Command Processed      "
DoOut:
END SUB

SUB Drawscr
REM /*  This routine calls other routines to set up the main game screen */

	CALL Box(39, 1, 80, 22)
	CALL Box(4, 16, 10, 22)
	CALL Box(16, 16, 22, 22)
	CALL Box(28, 16, 34, 22)
	CALL Box(1, 1, 38, 15)
	CALL Box(2, 23, 79, 25)
	CALL PRINTAT(1, 53, "[ Main Map ]")
	CALL PRINTAT(22, 54, "[ Map # 1 ]")
	CALL PRINTAT(1, 6, "[ Comm-Bat Command Listing ]")
	CALL PRINTAT(23, 34, "[ Messages ]")
	CALL PRINTAT(16, 5, "[D-1]")
	CALL PRINTAT(16, 17, "[ B ]")
	CALL PRINTAT(16, 29, "[ T ]")

LOCATE 2, 8: PRINT "XX  YY  M SHL MIN SHE LAZ DEC";
LOCATE 3, 2: PRINT "BASE  ##  ##  # ### ### ### ###  #";
LOCATE 4, 2: PRINT "TANK  ##  ##  # ### ### ### ###  #";
LOCATE 5, 2: PRINT "DECOY ##  ##  # ### ### ### ###  #";
LOCATE 7, 2: PRINT "DRONES #";
LOCATE 8, 2: PRINT "NUKES  #";
FOR scrs% = 1 TO 7
PCOPY 0, scrs%
SCREEN , , scrs%, scrs%
LOCATE 22, 62: PRINT CHR$(49 + scrs%);
NEXT
SCREEN , , 0, 0
END SUB

SUB DropMine (Howmany%)
IF (TankX%(Tank%) = BaseX%) AND (TankY%(Tank%) = BaseY%) AND (TankM%(Tank%) = BaseM%) THEN
    CALL Msg("Stored Mines In Base")
    IF Mines%(Tank%) < Howmany% THEN Howmany% = Mines%(Tank%)
    Mines%(Tank%) = Mines%(Tank%) - Howmany%: Mines%(0) = Mines%(0) + Howmany%
    Hold%(Tank%) = Hold%(Tank%) + 10 * Howmany%
    CALL UpDateBase
    CALL UpDateTank2(Tank%)
    GOTO DropOut
    END IF
FOR D% = 1 TO 3
	IF (TankX%(Tank%) = DecoyX%(D%)) AND (TankY%(Tank%) = DecoyY%(D%)) AND (TankM%(Tank%) = DecoyM%(D%)) THEN Q% = D%
NEXT D%
IF Q% = 0 THEN GOTO Dropped
CALL Msg("Stored Mines In Decoy")
IF Mines%(Tank%) < Howmany% THEN Howmany% = Mines%(Tank%)
Mines%(Tank%) = Mines%(Tank%) - Howmany%: Mines%(Q% + 8) = Mines%(Q% + 8) + Howmany%
Hold%(Tank%) = Hold%(Tank%) + 10 * Howmany%
IF Decoy% = Q% THEN CALL UpDateDecoy(Q%)
CALL UpDateTank2(Tank%)
GOTO DropOut
Dropped:   
REM  Not on the Base or a Decoy, must be making a minefield!
IF Howmany% > Mines%(Tank%) THEN Howmany% = Mines%(Tank%)
Mines%(Tank%) = Mines%(Tank%) - Howmany%
IF Howmany% = 0 THEN GOTO DropOut
CALL Convert(TankY%(Tank%), TankX%(Tank%), TankM%(Tank%))
MineField%(RealX%, RealY%) = MineField%(RealX%, RealY%) + 1
Twas$(Tank%) = "+"
REM BigMap$(RealY%, RealX%) = "+"
Hold%(Tank%) = Hold%(Tank%) + 10 * Howmany%
CALL Msg("Mine Drop Successfully.")
CALL UpDateTank2(Tank%)
DropOut:
END SUB

SUB EraseTank (Tank%)
REM /*  ERASETANK(tank #) - removes old image of tanks from screen */
	CALL Convert(TankY%(Tank%), TankX%(Tank%), TankM%(Tank%))
	BigMap$(RealY%, RealX%) = Twas$(Tank%)
	IF (TankM%(Tank%) = Map%) THEN
	LOCATE TankY%(Tank%) + 1, TankX%(Tank%) + 39
	PRINT Twas$(Tank%);
	END IF

END SUB

SUB GetDecoy
IF (TankX%(Tank%) = BaseX%) AND (TankY%(Tank%) = BaseY%) AND (TankM%(Tank%) = BaseM%) THEN
    FOR D% = 1 TO 3
    IF (DecoyX%(D%) = BaseX%) AND (DecoyY%(D%) = BaseY%) AND (DecoyM%(D%) = BaseM%) THEN Q% = D%
    NEXT D%
    IF Q% = 0 THEN CALL Msg("No Decoys In Base"): GOTO DekeOut
    IF 200 > Hold%(Tank%) THEN CALL Msg("Decoys Take 200 Space, Tank Only Has" + STR$(Hold%(Tank%))): GOTO DekeOut
    Dekes%(Tank%) = 1: Dekes%(0) = Dekes%(0) - 1
    Hold%(Tank%) = Hold%(Tank%) - 200
    DecoyM%(Q%) = 0: DecoyX%(Q%) = Tank%
    CALL Convert(TankY%(Tank%), TankX%(Tank%), TankM%(Tank%))
    IF Decoy% = Q% THEN CALL UpDateDecoy(Q%)
    CALL UpDateBase
    CALL UpDateTank2(Tank%)
    GOTO DekeOut
    END IF
FOR D% = 1 TO 3
	IF (TankX%(Tank%) = DecoyX%(D%)) AND (TankY%(Tank%) = DecoyY%(D%)) AND (TankM%(Tank%) = DecoyM%(D%)) THEN Q% = D%
NEXT D%
IF Q% = 0 THEN GOTO NoDekes
IF 200 > Hold%(Tank%) THEN CALL Msg("Decoys Take 200 Spaces, Tank Only Has" + STR$(Hold%(Tank%))): GOTO DekeOut
CALL Convert(TankY%(Tank%), TankX%(Tank%), TankM%(Tank%)): Dekes%(Tank%) = 1: BigMap$(RealY%, RealX%) = CHR$(48 + Tank%): Twas$(Tank%) = " "
Hold%(Tank%) = Hold%(Tank%) - 200
DecoyM%(Q%) = 0: DecoyX%(Q%) = Tank%
IF Decoy% = Q% THEN CALL UpDateDecoy(Q%)
CALL UpDateTank2(Tank%)
GOTO DekeOut
NoDekes:   
CALL Msg(""): CALL Msg("No Decoys Out Here!!!")
DekeOut:

END SUB

FUNCTION GetKey%
J$ = INKEY$: IF J$ = "" THEN GetKey% = 255: GOTO GetKeyOut

IF LEN(J$) = 2 THEN
       J$ = RIGHT$(J$, 1)
       GetKey% = 100 + ASC(J$)
       LOCATE 1, 1
       PRINT ASC(J$);
       GOTO GetKeyOut
       END IF

IF J$ = CHR$(8) AND LEN(Cmd$) > 0 THEN Cmd$ = LEFT$(Cmd$, LEN(Cmd$) - 1): GetKey% = 0: GOTO ShowCmd
IF J$ = CHR$(13) THEN CALL DoCmd: Cmd$ = "": J$ = ""
IF LEN(Cmd$) = 20 THEN GOTO GetKeyOut
Cmd$ = Cmd$ + J$
ShowCmd:
LOCATE 13, 2: PRINT "Cmd>                     "; : LOCATE 13, 7: PRINT Cmd$; CHR$(95);
GetKeyOut: 
END FUNCTION

SUB GetLazer (Howmany%)
IF (TankX%(Tank%) = BaseX%) AND (TankY%(Tank%) = BaseY%) AND (TankM%(Tank%) = BaseM%) THEN
    IF Howmany% * 30 > Hold%(Tank%) THEN CALL Msg("Space on Tank Only" + STR$(Hold%(Tank%))): GOTO GetMOut
    IF Lazers%(0) < Howmany% THEN Howmany% = Lazers%(0)
    Lazers%(Tank%) = Lazers%(Tank%) + Howmany%: Lazers%(0) = Lazers%(0) - Howmany%
    Hold%(Tank%) = Hold%(Tank%) - 30 * Howmany%
    CALL UpDateBase
    CALL UpDateTank2(Tank%)
    GOTO GetMOut
    END IF
FOR D% = 1 TO 3
	IF (TankX%(Tank%) = DecoyX%(D%)) AND (TankY%(Tank%) = DecoyY%(D%)) AND (TankM%(Tank%) = DecoyM%(D%)) THEN Q% = D%
NEXT D%
IF Q% = 0 THEN GOTO Bomber
IF Howmany% * 30 > Hold%(Tank%) THEN CALL Msg("Space On Tank Only" + STR$(Hold%(Tank%))): GOTO GetMOut
IF Lazers%(Q% + 8) < Howmany% THEN Howmany% = Lazers%(Q% + 8)
Lazers%(Tank%) = Lazers%(Tank%) + Howmany%: Lazers%(Q% + 8) = Lazers%(Q% + 8) - Howmany%
Hold%(Tank%) = Hold%(Tank%) - 30 * Howmany%
IF Decoy% = Q% THEN CALL UpDateDecoy(Q%)
CALL UpDateTank2(Tank%)
GOTO GetMOut
Bomber:   
CALL Msg(""): CALL Msg("LOAD LAZERS FROM DECOYS OR BASE ONLY!")
GetMOut:

END SUB

SUB GetMap (Mapps%)
 LOCATE TankY%(Tank%) + 1, TankX%(Tank%) + 39: PRINT Twas$(Tank%);
 Map% = Mapps%
 SCREEN , , Mapps% - 1, Mapps% - 1
 CALL UpDateDecoy(Decoy%)
 CALL UpDateTank2(Tank%)
 CALL UpDateBase
 IF TankX%(Tank%) THEN CALL Views
END SUB

SUB GetMine (Howmany%)
IF (TankX%(Tank%) = BaseX%) AND (TankY%(Tank%) = BaseY%) AND (TankM%(Tank%) = BaseM%) THEN
    IF Howmany% * 10 > Hold%(Tank%) THEN CALL Msg("Space on Tank Only" + STR$(Hold%(Tank%))): GOTO GetMyOut
    IF Mines%(0) < Howmany% THEN Howmany% = Mines%(0)
    Mines%(Tank%) = Mines%(Tank%) + Howmany%: Mines%(0) = Mines%(0) - Howmany%
    Hold%(Tank%) = Hold%(Tank%) - 10 * Howmany%
    CALL UpDateBase
    CALL UpDateTank2(Tank%)
    GOTO GetMyOut
    END IF
FOR D% = 1 TO 3
	IF (TankX%(Tank%) = DecoyX%(D%)) AND (TankY%(Tank%) = DecoyY%(D%)) AND (TankM%(Tank%) = DecoyM%(D%)) THEN Q% = D%
NEXT D%
IF Q% = 0 THEN GOTO Bombed
IF Howmany% * 10 > Hold%(Tank%) THEN CALL Msg("Space On Tank Only" + STR$(Hold%(Tank%))): GOTO GetMyOut
IF Mines%(Q% + 8) < Howmany% THEN Howmany% = Mines%(Q% + 8)
Mines%(Tank%) = Mines%(Tank%) + Howmany%: Mines%(Q% + 8) = Mines%(Q% + 8) - Howmany%
Hold%(Tank%) = Hold%(Tank%) - 10 * Howmany%
IF Decoy% = Q% THEN CALL UpDateDecoy(Q%)
CALL UpDateTank2(Tank%)
GOTO GetMyOut
Bombed:      
CALL Msg(""): CALL Msg("LOAD MINES FROM DECOYS OR BASE ONLY!")
GetMyOut:
END SUB

SUB GetShell (Howmany%)
IF (TankX%(Tank%) = BaseX%) AND (TankY%(Tank%) = BaseY%) AND (TankM%(Tank%) = BaseM%) THEN
    IF Howmany% * 20 > Hold%(Tank%) THEN CALL Msg("Space on Tank Only" + STR$(Hold%(Tank%))): GOTO GetsOut
    IF Shells%(0) < Howmany% THEN Howmany% = Shells%(0)
    Shells%(Tank%) = Shells%(Tank%) + Howmany%: Shells%(0) = Shells%(0) - Howmany%
    Hold%(Tank%) = Hold%(Tank%) - 20 * Howmany%
    CALL UpDateBase
    CALL UpDateTank2(Tank%)
    GOTO GetsOut
    END IF
FOR D% = 1 TO 3
	IF (TankX%(Tank%) = DecoyX%(D%)) AND (TankY%(Tank%) = DecoyY%(D%)) AND (TankM%(Tank%) = DecoyM%(D%)) THEN Q% = D%
NEXT D%
IF Q% = 0 THEN GOTO Bombbed
IF Howmany% * 20 > Hold%(Tank%) THEN CALL Msg("Space On Tank Only" + STR$(Hold%(Tank%))): GOTO GetsOut
IF Shells%(Q% + 8) < Howmany% THEN Howmany% = Shells%(Q% + 8)
Shells%(Tank%) = Shells%(Tank%) + Howmany%: Shells%(Q% + 8) = Shells%(Q% + 8) - Howmany%
Hold%(Tank%) = Hold%(Tank%) - 20 * Howmany%
IF Decoy% = Q% THEN CALL UpDateDecoy(Q%)
CALL UpDateTank2(Tank%)
GOTO GetsOut
Bombbed:   
CALL Msg(""): CALL Msg("LOAD SHELLS FROM DECOYS OR BASE ONLY!")
GetsOut:

END SUB

SUB Initgame
REM /* Initialize game - get base co-ordinates  */
       
	LOCATE 2, 50
	PRINT "Enter base co-ords X,Y,Map ";
	LOCATE 3, 50
	INPUT BaseX%, BaseY%, BaseM%
	Map% = BaseM%: REM  CALL GetMap(BaseM%)
	LOCATE 2, 50: PRINT "                            ";
	LOCATE 3, 50: PRINT "                   ";
	LOCATE 3, 8: PRINT USING "##  ##  #"; BaseX%; BaseY%; BaseM%
	CALL UpDateDecoy(Decoy%)
	LOCATE 13, 2: PRINT "Cmd>";
	CALL Convert(BaseY%, BaseX%, BaseM%)
	BigMap$(RealY%, RealX%) = "B"
	FOR loop1% = 1 TO 8
	  
	   TankX%(loop1%) = BaseX%
	   TankY%(loop1%) = BaseY%
	   TankM%(loop1%) = BaseM%
	   IF loop1% < 4 THEN
	      DecoyX%(loop1%) = BaseX%
	      DecoyY%(loop1%) = BaseY%
	      DecoyM%(loop1%) = BaseM%
REM              CALL Convert(DecoyY%(loop1%), DecoyX%(loop1%), DecoyM%(loop1%))
REM              BigMap$(RealY%, RealX%) = "D"
	   END IF
	NEXT
CALL GetMap(BaseM%)
CALL UpDateTank(Tank%)
END SUB

SUB Movetank (Direction%)
REM /* this routine is responsible for moving the tank if it is ok to do so */

	IF Shields%(Tank%) = 0 THEN
	  CALL Msg("")
	  CALL Msg("That Tank is dead - immobile!")
	  GOTO MoveOut
	END IF

	zq = Badmove%(Direction%): REM  0=Normal, 1=RIP, 2=New Map, 3=No Go
       
	IF zq = 3 THEN
		    CALL Msg("")
		    CALL Msg("Tanks Cannot Stack!")
		    GOTO MoveOut
		    END IF
       
IF zq <> 2 THEN CALL EraseTank(Tank%)
       
	IF zq = 1 THEN
	  
	  Shields%(Tank%) = 0
	  CALL Msg("Tank has hit map edge and exploded!")
	  CALL Msg("")
	  CALL Convert(TankY%(Tank%), TankX%(Tank%), TankM%(Tank%))
		IF (BigMap$(RealY%, RealX%) <> "B") AND (BigMap$(RealY%, RealX%) <> "D") THEN
		BigMap$(RealY%, RealX%) = "X"
		END IF
			IF TankM%(Tank%) = Map% THEN
			LOCATE TankY%(Tank%) + 1, TankX%(Tank%) + 39
			PRINT "X";
			END IF
       CALL Views
       GOTO MoveOut
       END IF
       
	IF zq = 2 THEN
	 CALL Convert(TankY%(Tank%), TankX%(Tank%), TankM%(Tank%))
	  CALL UpDateTank(Tank%)
	  CALL Views
	  GOTO MoveOut
	END IF

	REM if proram gets here it must be a normal move
	SELECT CASE Direction%
	  
	  CASE UP: TankY%(Tank%) = TankY%(Tank%) - 1
	  CASE down: TankY%(Tank%) = TankY%(Tank%) + 1
	  CASE Left: TankX%(Tank%) = TankX%(Tank%) - 1
	  CASE Right: TankX%(Tank%) = TankX%(Tank%) + 1
       
	END SELECT

	CALL UpDateTank(Tank%)
	CALL Views
	GOTO MoveOut:

MoveOut:

END SUB

SUB MoveText (X1%, Y1%, X2%, Y2%, X3%, Y3%)
DEF SEG = &HB800 + &H100 * (Map% - 1)
RealAdd% = (X1% - 1) * 2 + (Y1% - 1) * 160
NewAdd% = (X3% - 1) * 2 + (Y3% - 1) * 160
Reps% = X2% - X1% + 1: Rose% = Y2% - Y1% + 1
FOR Rows% = 1 TO Rose%
FOR Time% = 1 TO Reps%
	Byte% = PEEK(RealAdd% + (Time% - 1) * 2 + 160 * (Rows% - 1))
	POKE NewAdd% + (Time% - 1) * 2 + 160 * (Rows% - 1), Byte%
NEXT
NEXT
END SUB

SUB Msg (Txt$)
CALL Clrtxt
LOCATE 24, 3: PRINT Txt$;

END SUB

SUB PRINTAT (Column%, Row%, Text$)
REM /* a print @ type of routine            */
       
	LOCATE Column%, Row%
	PRINT Text$;
END SUB

SUB Remote
REM /* Remote - this simulates tasking in background */
STATIC ZX
	ZX = ZX + 1
	IF (ZX = 100) THEN ZX = 0
	LOCATE 10, 2
	PRINT "Count is"; ZX; "and counting.";

	REM IF Twas$(Tank%) <> " " THEN STOP
END SUB

SUB Showtank (Tank%)
REM /* Displays currently selected tank IF on current map */
       
IF (TankM%(Tank%) = Map%) THEN
	
	IF (TankX%(Tank%) = 1) THEN
	   a = 0
	ELSEIF TankX%(Tank%) = 2 THEN
	   a = 1
	ELSE
	   a = 2
	END IF

	IF (TankX%(Tank%) = 40) THEN
	   c = 0
	ELSEIF TankX%(Tank%) = 39 THEN
	   c = 1
	ELSE
	   c = 2
	END IF

	IF (TankY%(Tank%) = 1) THEN
	   b = 0
	ELSEIF (TankY%(Tank%) = 2) THEN
	   b = 1
	ELSE
	   b = 2
	END IF

	IF (TankY%(Tank%) = 20) THEN
	   D = 0
	ELSEIF (TankY%(Tank%) = 19) THEN
	   D = 1
	ELSE
	   D = 2
	END IF

CALL MoveText(31 - a, 19 - b, 31 + c, 19 + D, TankX%(Tank%) + 39 - a, TankY%(Tank%) + 1 - b)


END IF

END SUB

SUB Tankview (Tank%)
REM /* Update Tank viewport                     */
	CALL Convert(TankY%(Tank%), TankX%(Tank%), TankM%(Tank%))
	FOR X% = 0 TO 4
	   FOR Y% = 0 TO 4
	      LOCATE 17 + Y%, 29 + X%
	      PRINT BigMap$(RealY% - 2 + Y%, RealX% - 2 + X%);
	   NEXT
	NEXT
	CALL Showtank(Tank%)

END SUB

SUB UpDateBase
LOCATE 3, 8: PRINT USING US$; BaseX%; BaseY%; BaseM%; Shields%(0); Mines%(0); Shells%(0); Lazers%(0); Dekes%(0);
LOCATE 7, 8: PRINT Drones%
LOCATE 8, 8: PRINT Nukes%
END SUB

SUB UpDateDecoy (Number%)
IF DecoyM%(Number%) = 0 THEN
	LOCATE 5, 8: PRINT "--  --  -";
	LOCATE 19, 5: PRINT "O-F-F";
	GOTO UpOut
	END IF
LOCATE 5, 8: PRINT USING US$; DecoyX%(Decoy%); DecoyY%(Decoy%); DecoyM%(Decoy%); Shi%; Mines%(Decoy% + 8); Shells%(Decoy% + 8); Lazers%(Decoy% + 8); Shi%

UpOut:
END SUB

SUB UpDateTank (Number%)
REM /*  UpdateTank(tank #) - updates display of tanks moves   */
	CALL Convert(TankY%(Tank%), TankX%(Tank%), TankM%(Tank%))
	Twas$(Tank%) = BigMap$(RealY%, RealX%)
	IF (Twas$(Tank%) <> "B") AND (Twas$(Tank%) <> "D") THEN
	BigMap$(RealY%, RealX%) = CHR$(48 + Tank%)
	END IF
LOCATE 4, 8: PRINT USING "##  ##  #"; TankX%(Tank%); TankY%(Tank%); TankM%(Tank%);

END SUB

SUB UpDateTank2 (V%)
LOCATE 4, 8: PRINT USING US$; TankX%(V%); TankY%(V%); TankM%(V%); Shields%(V%); Mines%(V%); Shells%(V%); Lazers%(V%); Dekes%(V%);
END SUB

SUB Views
REM /* Package all view handling nice and neat */
       
	CALL Tankview(Tank%)
	CALL Baseview
	CALL Decoyview(Decoy%)

END SUB

