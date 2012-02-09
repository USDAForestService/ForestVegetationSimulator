!== last modified  4-13-2004
      SUBROUTINE BLMTAP(DBHOB,HTTOT,TLH,HTUP,D17,TOP,XLEN,D2,Profile)
C###########################################################
      USE DEBUG_MOD
      
      REAL BLMTHT (4,10),BLMBA (6,9)
      REAL HTUP,D2,TLH,H,SMALLH,C
C     REAL HBUTT, HTDIB, HTUP
      REAL D17,TOP,HBUTT,XLEN,HTDIB,RA,TOLERANCE,TOL_LIMIT
      REAL DBHOB,HTTOT,DIBCOR,A,B,LN
      REAL       E
      PARAMETER (E=2.7182818284)
      INTEGER    Profile,ILIMIT,ICOUNT,profileht

C          COFFICIENTS FOR BEHRE'S HYPERBOLA (B0,B1,B2,B3)
C  01 zone 01
      DATA (( BLMTHT(I,J), I=1,4), J=1,10)
     >      / 0.6448,  -0.00196,  0.0,       0.0,
C  01 zone 02
     >        0.6096,  -0.00196,  0.0,       0.0,
C  10, 11
     >        0.31385,  0.0,      0.002985, -0.00003386,
C  13
     >        0.4779,   0.0,      0.0,       0.0,
C  14
     >        0.5455,  -0.00196,  0.0,       0.0,
C  31 zone 1, 33
     >        0.45648,  0.00289,  0.0,       0.0,
C  32, 34, 35
     >        0.6014,   0.0,      0.0,       0.0, 
C  48
     >        0.54568,  0.0,      0.0,       0.0000546,
C  51, 54, 55 
     >        0.4606,   0.0,      0.0,       0.0, 
C  ALL OTHER SPECIES
     >        0.6200,   0.0,      0.0,       0.0/

C          COFFICIENTS FOR BEHRE'S HYPERBOLA (B0,B1,B2,B3,B4,B5)
C  DOUGLAS FIR, COAST
      DATA (( BLMBA(I,J), I=1,6), J=1,9)
     >      / -0.5366,  1.0,     0.07124, 1.0,  0.07816,  0.0,
C  DOUGLAS FIR, CASCADE
     >        -0.8015,  1.0,     0.14317, 1.0,  0.12271,  0.0,
C  DOUGLAS FIR, SOUTHWEST,EAST,CALIFORNIA & W LARCH
     >        -0.9611,  1.0,     0.16403, 1.0,  0.11527,  0.0,
C  PONDEROSA, SUGAR, JEFFREY & W WHITE PINE
     >        -1.0170,  1.0,     0.16343, 1.0,  0.10446,  0.0,
C  GRAND, WHITE FIR, SPRUCES EAST & WEST SIDE
     >         0.5869, -5.9782, -1.0,     1.0,  0.0,      0.0,
C  SHASTA, SILVER, & NOBLE FIR
     >         0.0,     1.0,     0.0,     1.0,  0.1036,  -3.0482,
C  HEMLOCK
     >        -0.1761,  1.0,     0.0,     1.0,  0.12568,  0.0,
C  CEDARS
     >         0.5919, -0.00484, 0.0,     1.0, -1.0,      0.0,
C  ALL OTHER SPECIES
     >         0.6200,  0.0,     1.0,     1.0,  1.0,      0.0    /

C *************************************************************
C     THIS IS THE START OF THE MAIN LOGIC
C *************************************************************
C
      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, '(A)') ' -->Enter BLMTAP'
   		ENDIF
     
     
C   DETERMINE IF HEIGHT IS IN LOGS OR FEET
      IF (TLH.EQ.0.0) THEN

C--     FOR TOTAL HEIGHT IN FEET FROM STUMP TO TIP *********************

C-- BEHRE'S HYPERBOLA PORTION TO Calculate DIB *************************

        HBUTT = HTTOT - (XLEN + 1.5)
        HTDIB = HTTOT - HTUP
 
        A = BLMTHT(1,Profile) +
     >       BLMTHT(2,Profile)*DBHOB +
     >       BLMTHT(3,Profile)*HTTOT +
     >       BLMTHT(4,Profile)*DBHOB*HTTOT

        B = 1.0 - A

        DIBCOR = D17 * ( (HTDIB/HBUTT) / (A*(HTDIB/HBUTT)+(B)) )
        D2 = DIBCOR
C
C     Return a Zero Value for the Diameter-Inside-Bark IFF height is
C     given in feet.  RJM, OSO 931.5, 20 Sep 93, in consultation with
C     Jim Alegria, OSO 930.
C
c         D2 = 0.0
      ELSE

C--      FOR TOTAL HEIGHT IN NUMBER OF LOGS PER TREE ******************

C--  BEHRE'S HYPERBOLA PORTION TO CALCULATE THE "A" COEFFICIENT *******
C--  using an iterative method devised by Jim Alegria, September, 1993.
C--  Following checks per J. Alegria, 7 Dec 93.  RJM, 931.5.
         If (D17 .LT. 5.0)  D17 = 5.0
         If (D17 .LT. TOP)  D17 = TOP

         If (TLH .EQ. 1.0 ) Then
           D2 = D17
           Return
         Elseif(TLH.EQ.2.0) Then
	         If(HTUP.EQ.1.0) Then
	           D2 = D17
	         Else
	           D2 = TOP
           EndIf
	         Return
	       Endif

         If (TOP .EQ. D17)  Then
           D2 = D17
           Return
         EndIf

         RA = 0.0
         Tolerance   =  1.0
         Tol_Limit   =  0.01

C        Estimate the coefficient as 0.62, then perform a count limited
C        iterative procedure using the given DIB @ 17 feet, <D17>, the
C        Tree Height in LOGS <TLH>, the DBHOB, <D>, and the Top Diameter <TOP>:
         A           =  0.62
         B           = 1 - A
         C           =(TLH -  1.0) * XLEN
         H           =   C / (1.0 - TOP * B / (D17 - A * TOP) )
         HtTot       = H   + 17.8

         iLimit      = 20
         iCount      =  0

!         Write (LUDBG,1) Profile, iCount,  TLH, DBHOB, D17, TOP,
!     & C, H, HtTot, A, RA
!1      Format  ( ' BLMTap:', 2I3,  ';',     4F6.2,     ';', 5F7.2)

         Do While (Tolerance .GT. Tol_Limit .AND. iCount .LT. iLimit)
       
           iCount    = iCount + 1
        A = BLMTHT(1,Profile) +
     >       BLMTHT(2,Profile)*DBHOB +
     >       BLMTHT(3,Profile)*HTTOT +
     >       BLMTHT(4,Profile)*DBHOB*HTTOT
c           A         = BLMBA(1,Profile) +
c     >                (BLMBA(2,Profile) * HtTot**BLMBA(3,Profile)) /
c     >                (BLMBA(4,Profile) *   DBHOB**BLMBA(5,Profile)) +
c     >                 BLMBA(6,Profile) /   DBHOB
           B         = 1.0 - A

           H         =  C / (1.0 - TOP * B / (D17 - A * TOP) )
           HtTot     =  H + XLEN + 1.5

           Tolerance = Abs ( RA - A )
           
!           Write (LUDBG,2) Profile, iCount,  TLH, DBHOB, D17, TOP,
!     & C, H, HtTot, A, RA
!2      Format  ( ' BLMTap:', 2I3,  ';',     4F6.2,     ';', 5F7.2)
           
           RA        = A

           End Do

C-- The "A" Co-efficient has now been corrctly computed.

C-- BEHRE'S HYPERBOLA PORTION TO Calculate DIB ************************

           SMALLH = H - (HTUP - 1.0) * XLEN
           LN     = SMALLH / H
           DIBCOR =        D17 * LN / (A * LN + B) 

           D2 = DIBCOR
           
           IF (DEBUG%MODEL) THEN
              WRITE  (LUDBG, 300)'  A      B     C    H    HTTOT' 
     &        //'   SMALLH  LN  DIBCOR  TOP  PROFILE  B0     B1   B2'
     &        //'   B3'
300           FORMAT (A)
  		        WRITE  (LUDBG, 320)A, B, C, H, HTTOT, SMALLH, LN, DIBCOR
     &        ,TOP, PROFILE, BLMTHT(1,PROFILE), BLMTHT(2,PROFILE),
     &        BLMTHT(3,PROFILE), BLMTHT(4,PROFILE)
320           FORMAT(5F6.1, 3X, 5F6.1, 2X,4F6.3)
           ENDIF
      ENDIF

C*****************************************************************
C    THIS IS THE END OF THE MAIN LOGIC
C*****************************************************************

      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, '(A)') ' <--Exit BLMTAP'
      ENDIF
      
      RETURN
      END
C      Input Variables:
C      D       {Real   } The "DBHOB" in inches.  Copied to <DBHOB>.
C      TTH     {Real   } Tree Height in FEET.  NOT currently used.
C      TLH     {Real   } If <0.0> Then Height Variable Unit is Feet,
C                                 Else Height Variable Unit is Logs.
C      TLH     {Real   } Tree Height in LOGs (iff not zero).  Fixed for a tree.
C      HT2     {Real   } Subject log number.  Set in <BLMVol> before invocation.
C      D17     {Real   } DIB at 17.8 feet OR (Form_Class * DBHOB / 100)
C      Profile {Integer} The Stem Model Number, Derived from the Species Code.
C
C      Output Variables:
C      TOP     {Real   } Passed from "6A" record but set to 5.0" if < 5.0.
C      D2      {Real   } The computed diameter-inside-bark @ 17 feet
C                        if <TLH> is NOT equal to 1.00.

C
C      Modifications:
C      20 Sep 93; R. Miller, OSO 931.5
C      ====> Replace the method used to compute the "A" co-efficient.
C
C      BLM STEM PROFILE MODEL USES BEHRE'S HYPERBOLA.

C      D      - SINGLE PRECISION VALUE FOR DBHOB
C      D2     - SINGLE PRECISION VALUE FOR DIBCOR
C      DBHOB    - DOUBLE PRECISION VALUE FOR DBHOB
C      D17    - DIB AT 17.8 FEET OR (FORM CLASS * DBHOB / 100)
C      DIBCOR - DOUBLE PRECISION COMPUTED DIAMETER SECOND STAGE
C      DIBUP  - DOUBLE PRECISION COMBUTED DIAMETER FIRST STAGE MODEL
C      HT1    - SINGLE PRECISION FOR TOTAL TREE HT FROM GROUND TO TIP
C      HT2    - SINGLE PRECISION VALUE FOR UPPER STEM HT FROM GROUND
C      HTTOT  - DOUBLE PRECISION VALUE FOR HT1
C      HTUP   - DOUBLE PRECISION VALUE FOR HT2
C      BLMBA  - BEHRE'S HYPERBOLA "B" COFFICIENTS FOR THE "A" COEFFICIENT
C      Profile- SUBSCRIPT TO USE FOR SPECIES COEFFICIENTS in Array <BLMBA>

C      SPECIES SUBSCRIPTS ARE AS FOLLOWS
C      1 = DOUGLAS-FIR COAST
C      2 = DOUGLAS-FIR CASCADE
C      3 = DOUGLAS-FIR SOUTHWEST      & Eastern Oregon
C      4 = DOUGLAS-FIR EAST SIDE      NOT USED, 20 Sep 93, RJM
C      5 = REDWOOD
C      6 = DOUGLAS-FIR CALIFORNIA     NOT USED, 20 Sep 93, RJM
C     10 = PONDEROSA PINE (BLACKJACK) NOT USED, 20 Sep 93, RJM
C     11 = PONDEROSA PINE (YELLOW)
C     12 = JEFFREY PINE
C     13 = SUGAR PINE
C     14 = WESTERN WHITE PINE
C     15 = LODGEPOLE PINE
C     20 = PACIFIC YEW
C     21 = TAN OAK
C     22 = RED ALDER
C     23 = OREGON MYRTLE
C     24 = BIG LEAF MAPLE
C     25 = PACIFIC MADRONE
C     26 = GOLDERN CHINQUAPIN
C     27 = OREGON ASH
C     28 = BLACK COTTONWOOD
C     29 = OAK SPECIES
C     30 = WHITE FIR EAST SIDE        NOT USED, 20 Sep 93, RJM
C     31 = WHITE FIR WEST SIDE
C     32 = RED SHASTA FIR
C     33 = GRAND FIR
C     34 = PACIFIC SILVER FIR
C     35 = NOBLE FIR
C     41 = ENGELMANN SPRUCE
C     42 = SITKA SPRUCE
C     48 = HEMLOCK
C     51 = INCENSE CEDAR
C     52 = ALASKA YELLOW CEDAR
C     53 = PORT ORFORD CEDAR
C     54 = WESTERN RED CEDAR
C     55 = WESTERN LARCH
C     56 = MISCELLANEOUS SPECIES
