C----------
C VOLUME $Id$
C----------
!== last modified  4-9-2002
      SUBROUTINE SF_ZERO(X1, F1, X2, F2, MODE, TOL)   
c           Finds the zero of a univariate single precision function  SF_ZERO
c           Adopted from SZERO found in MATH77 Subr Library of
c               Language Systems   Sterling, VA.
c      
c           Code is Copyright by Language Systems.
c           Source, object and executable MAY be distributed with SF_TEST.

c>> 1996       SZERO  JWF     Removed calls AMACH , MESS, SMESS and UMESS  
c>> 1993-04-27 SZERO  Krogh   Additions for Conversion to C.
c>> 1993-04-13 SZERO  Krogh   Minor change for new MESS.
c>> 1992-04-08 SZERO  Krogh   Unused label 400 removed.
c>> 1992-01-09 SZERO  Krogh   Moved calc. of XXMXO up (for error msg.)
c>> 1991-11-26 SZERO  Krogh   Converted to new error processor.
c>> 1988-08-14 SZERO  Krogh   Labels runumbered.
c>> 1988-03-07 SZERO  Krogh   Initial code.
c ========== This program has been specialized ==========
c SUBROUTINE TO FIND A BOUNDED ZERO
c
c analysis and coding by Fred T.Krogh at the Jet Propulsion
c Laboratory, Pasadena, Calif.  April 25, 1972.
c Modified for portability, April 1984 by Krogh.
c Algorithmic changes, vars. added to save stmt., Sept. 1987 by Krogh
c
c Parameters in the calling sequence are defined as follows:
c
c  X1  = independent variable
c  F1  = dependent variable --  initially   F1=F(X1).
c        When MODE=1 (or 5) the user is to compute F(X1) given X1
c  X2  = second value of independent variable
c  F2  = F(X2) on the initial entry.  When MODE = 2-4, F2=F(X2) and
c        F1*F2 .le. 0.
c  MODE  is a parameter used for communication between this
c        subroutine and the user. (The user should set MODE
c        only to initialize it to 0 before the first call)
c      =1  compute F(X1) and call $ZERO
c      =2  F(X1) is approximately 0, the iteration is finished
c          and the error criterion is satisfied.
c      =3  same as MODE=2, except the error criterion can
c          not be satisfied.
c      =4  apparently the function has a discontinuity
c          between X1 and X2 -- No zero can be found
c      =5  F1*F2 was greater than zero on the first call, and an attempt
c          to bound the zero on both sides have failed.
c      =6  fatal error -- $ZERO was called after mode was set .ge.2.
c          If $ZERO is called again, the program will be stopped.
c          (Unless MODE is set to 0)  
c      <0  If MODE is set <0 and $ZERO is called, no action is taken
c          except that print is turned on for -MODE calls to $ZERO.
c          This print gives all values of X and F used in the iteration.
c  TOL    is the error tolerance
c     TOL.GT.0  Iterate until values of X1 and X2 are known
c              for which abs(X1-X2) .le. tol and F1*F2 .le. 0.
c     TOL.LT.0  Iterate until a value of X1 is found for which
c              abs(F1) .le. abs(TOL).
c     TOL  = 0  Iterate until the zero is determined as
c              precisely as possible.  MODE = 3 is impossible
c              in this case.
c
c Parameters in the calling sequence have the following types
c
      integer MODE
      real             X1, X2, F1, F2, TOL
c
c Usage is as follows (of course, variations are possible.)
c         Somehow one has available X1, F1, X2, and F2 such
c         that F1 = F(X1), F2 = F(X2) and F1*F2 .le. 0.
c         In addition, one should assign a value to TOL.
c     MODE = 0
c***  In the statement below, $ is replaced by an 'S' for single
c***  precision and a 'D' for double.
c XXX call $ZERO(X1,F1,X2,F2,MODE,TOL)
c     go to  (N1,N2,N3,N4,N5,N6), MODE
c  N1 COMPUTE  F1=F(X1)
c     go to XXX
c
c  N4 continue
c  N5 continue
c  N6 stop
c  N3 If you want to -- print results to note that error
c                       is too big.
c  N2 zero is found, do whatever you want to with it.
c
c End of comments explaining usage.
c
c ************************* Usage of internal variables ****************
c
c C0     Parameter = 0.
c C1     Parameter = 1.
c C1P01  Parameter = 1.01
c C1P25  Parameter = 1.25
c C2     Parameter = 2.
c C4     Parameter = 4.
c CP01   Parameter = 0.01
c CP125  Parameter = 1.25
c CP25   Parameter = 0.25
c CP5    Parameter = 0.5
c CP75   Parameter = 0.75
c CP99   Parameter = 0.99
c R1MACH Gets constants associated with floating point arithmetic.
c DFDXXX = (XXMXL/FFMFL) * (est. deriv. of f w.r.t. x at x = XX).  All
c   derivatives are base on a second degree polynonial that interpolates
c   the last three points generated.
c DFDXXX = (XXMXO/FFMFL) * (est. deriv. of f w.r.t. x at x = X0).
c DIV    If artificial subdivision of the interval is used, determines
c   the amount of the sudivision.  (-XXMXOO * DIV / (1. + DIV))
c SMESS  Prints error messages.
c DXDFFF = (FFMFL/XXMXL) * (est. deriv. of x w.r.t. f at f = FF).
c DXDFFO = (FFMFO/XXMXL) * (est. deriv. of x w.r.t. f at f = FO).
c F1     (formal arg.) The last value of F computed, on return the value
c   of F(X1).
c F2     (formal arg.) The other initial value provided for F.  Set to
c   the value of F(X2) on returns.
c FDAT   Temporary storage for floating point values for messages.
c FF     Value of F1 after F is computed.
c FFDFO  FF / FO
c FFMFB  FFMFL + FLMFB = FF - value of FF 2 iterations back.
c FFMFL  FF - FL
c FL     Value of FF from the previous iteration.   
c FLMFB  Value of FFMFL from the previous iteration
c FO     F(XO)
c I      Comments for LCHNG define how I is set.
c IDAT   Temporary storage for integer values for messages.
c INDIC  Internal value to be assigned to MODE on exit.  Equivalenced to
c   MACT(3) and set to 0 initially.
c J      This is 1 if FF .le. 0., and is 2 if FF > 0.
c KNKP   KNKP(J) (see J above) is set to 0 whenever there are signs of
c   decent convergence.  It is counted up when convergence is slow.
c KS     =-1  initially,
c        = 0  whenever F changes sign, otherwise
c        = number of times the sign of F has remained the same
c KTYP   = 1 if interpolation was used to get the last iterate, = 0 if
c   an artificial subdivision was used.
c LCHG  the J-th continuation in the data statement for LCHG below gives
c new states for the case when the state number is J-1.  State 0 is the
c initial state.  The I-th entry on a row gives the state for case on I
c as follows:  (TP is the ratio (new f) / (last f of same sign)
c    I = 1   TP < 0.01
c    I = 2   .01 <= TP < 1
c    I = 3   TP = 1
c    I = 4   1 < TP <= 4
c    I = 5   TP > 4.
c States are as follows:
c    0   initial state, or big increase, or small increase in state 0
c    1   after big decrease, perhaps followed by small decreases
c    2   after one or more small decreases from state 0
c    3   one or more small increases from state 2
c    4   one or more small decreases from state 3
c    5   decision made that noise is a problem on this side of zero.
c LINIT  - the maximum number of iterations that can be taken with out
c   getting a sign change in F.
c LMODE  The value of MODE the last time in this routine.
c LNLP  
c LTXTxx Names of this form are used in setting up data statements for
c   error messages.  These names are generated automatically by PMESS,
c   the program that makes up these messages.
c MACT   This array difines the actions to be taken by the error message
c   program.  See comments in MESS for details.
c MACT1  As for MACT except used for the diagnostic print.
c MExxxx Parameters defining constants used for interaction with the
c   error message program MESS.  See comments there for definitions.
c MLOC   Contains locations in MTXTAA for error messages.
c MODE   (formal) See comments above.
c MTXTAA Text for error messages.
c MTXTAB Text for diagnostic message.
c MTXTAC Text for diagnostic message.
c NP     If > 0, gives number of iterations till diagnostic print stops.
c QFM   
c QXM   
c RND    Largest relative difference between succesive floating point
c   numbers.
c SMALL  .5 / (RND * largest floating point number)
c TOL    (Formal) See description above.
c TOLX   Actually tolerance required for accuracy in X.  Usually =
c   max(TOL, XRND).  It can be cut by a factor of 2 for use in setting
c   bounds on an acceptable interval.
c TP     Ordinarily the ratio (FF / prev. FF of the same sign.
c TP1    Used for temporary storage.
c X1     (Formal) Value of x where F is to be computed, and value
c   returned for the zero after convergence.
c X2     (Formal) Initially other value of x where F is given.  After
c   convergence gives the other closest x which gives an F of opposite
c   sign from that given by x1.
c XL     Value of XX from the previous iteration.
c XLMXB  Value of XXMXL from the previous iteration.
c XO     Value of x on opposite side of the zero from the current x.
c XRND   Best accuracy that one could hope for based on the finite
c   precision of floating point numbers.
c XX     Current x, the last value of X1 where F was computed.
c XXMXL  XX - XL
c XXMXO  XX - XO = length of interval in which 0 lies.
c XXMXOL Value of XXMXO from a previous iteration.
c
      parameter (LINIT = -40)
      integer KNKP(2), LCHG(30), LMODE, LNLP(2), NP
      real             XX, XO, XL, FF, FO, FL, FFDFO
      real             DIV, QFM, QXM, TP, TP1, XXMXO, XXMXOL
      real             RND, XRND, SMALL, TOLX
      real             XXMXL, XLMXB, FFMFL, FFMFB, FLMFB
      real             DXDFFF, DXDFFO, DFDXXX, DFDXXO
      real             C0, C1, C2, C4, CP125, CP25, CP5, CP75, C1P25
      real             C8, CP01, CP99, C1P01, CP001, C1P031
      real             R1MACH
c
      parameter (C0 = 0.E0, C1 = 1.E0, C2 = 2.E0, C4 = 4.E0)
      parameter (C8 = 8.E0)
      parameter (CP125 = 0.125E0, CP25 = 0.25E0, CP75 = 0.75E0)
      parameter (CP5 = 0.5E0)
      parameter (C1P25 = 1.25E0)
      parameter (CP01 = 0.01E0)
      parameter (CP001 = 0.001E0)
      parameter (CP99 = 0.99E0)
      parameter (C1P01 = 1.01E0)
      parameter (C1P031 = 1.03125E0)
c
c                      Declarations for error message processing.
c     
      real             FDAT(4)
      integer MACT(5), MLOC(4), IDAT(2)
      equivalence (INDIC, MACT(3))
      save DIV, FL, FLMFB, FO, KNKP, KS, KTYP, LCHG, LMODE,
     1   LNLP, MACT, NP, RND, SMALL, XL, XLMXB, XO, XX, XXMXOL
      parameter (MERET  =51)
      parameter (MEEMES =52)
      parameter (METEXT =53)
c
c ********* Error message text ***************
c[Last 2 letters of Param. name]  [Text generating message.]
cAA SZERO$B
cAB Best bound for zero is [$F, $F], but tolerance is $F.$E
cAC Apparent discontinuity in function near X = $F.$E
cAD Can not find a sign change: X1=$F, X2=$F, F1=$F, F2=$F$E
cAE Called with MODE = $I.$E
c   $
cAF In SZERO -- X1=$F F1=$(E11.4) KTYP=$I DIV=$G KS=$I$E
c   $
cAG             X2=$F F2=$G$E
      integer LTXTAA,LTXTAB,LTXTAC,LTXTAD,LTXTAE,LTXTAF,LTXTAG
      parameter (LTXTAA=  1,LTXTAB=  8,LTXTAC= 63,LTXTAD=112,LTXTAE=168,
     * LTXTAF=  1,LTXTAG=  1)
C      character MTXTAA(1) * (191)
C      character MTXTAB(1) * (52)
C      character MTXTAC(1) * (25)
C      data MTXTAA/'SZERO$BBest bound for zero is [$F, $F], but tolerance
C     * is $F.$EApparent discontinuity in function near X = $F.$ECan not 
C     *find a sign change: X1=$F, X2=$F, F1=$F, F2=$F$ECalled with MODE =
C     * $I.$E'/
C      data MTXTAB/'In SZERO -- X1=$F F1=$(E11.4) KTYP=$I DIV=$G KS=$I$E'
C     * /
C      data MTXTAC/'            X2=$F F2=$G$E'/
c
c                      1  2  3  4      5
      data MACT / MEEMES, 0, 0, 0, MERET /
C      data MACT1 / METEXT, MERET /
      data MLOC / LTXTAB, LTXTAC, LTXTAD, LTXTAE /
c
      data RND / C0 /
      data KS, KTYP, LMODE, DIV / 0, 0, 2, C0 /
      data LCHG /
     1   1, 2, 0, 0, 0,
     2   1, 1, 4, 5, 0,
     3   1, 2, 3, 3, 0,
     4   1, 4, 4, 3, 0,
     5   1, 4, 5, 5, 0,
     6   1, 5, 5, 5, 0 /
      data NP / 0 /
 
c
c INITIALIZE
c
      if (MODE .lt. 0) then
         NP = -1 - MODE
         return
      ENDIF
      if (NP .gt. 0) then
         NP = NP - 1
         FDAT(1) = X1
         FDAT(2) = F1
         FDAT(3) = DIV
         IDAT(1) = KTYP
         IDAT(2) = KS
         GO TO 1000
c         call SMESS(MACT1, MTXTAB, IDAT, FDAT)
c         if (MODE .ne. 0) if (LMODE - 1) 70, 80, 450
c         FDAT(1) = X2
c         FDAT(2) = F2
c         call SMESS(MACT1, MTXTAC, IDAT, FDAT)
      else if (MODE .ne. 0) then
         if (LMODE - 1) 70, 80, 450
      ENDIF
c
      if (RND .eq. C0) then
         RND = R1MACH(4)
         SMALL = CP5 / (RND * R1MACH(2))
      ENDIF
      XL = X2
      FL = F2
   30 TP = C1
      MODE = 1
      INDIC = 2
      XXMXOL = C0
      KNKP(1) = 0
      KNKP(2) = 0
      LNLP(1) = 0
      LNLP(2) = 0
      KS = -1
      XX = X1
      FF = F1
      if (FL) 40, 75, 50
   40 if (FF) 60, 230, 100
   50 if (FF) 100, 230, 60
   60 LMODE = 0
c             Take care of points on same side of zero.
   70 FF = F1
      XX = X1
      TP = FF / FL
      if (TP .lt. C0) go to 30
      LMODE = LMODE - 1
      if (LMODE .lt. LINIT) then
         INDIC = 5
         FDAT(1) = XX
         FDAT(2) = XL
         FDAT(3) = FF
         FDAT(4) = FL
         go to 250
      ENDIF
      if (TP .gt. C1) then
         FF = FL
         XX = XL
         FL = F1
         XL = X1
      ENDIF
      if (abs(FF) .ge. C8 * abs(FL-FF)) then
         TP = C8
      else
         TP = max(-CP25*real(LMODE), FF / (FL - FF))
      ENDIF
      FL = FF
      XO = XL
      XL = XX
      if (XX .eq. XO) XO = C1P031 * XX + sign(CP001, XX)
      XX = XX + TP * (XX - XO)
      X1 = XX
      MODE = 1
      return
c
   75 X1 = XL
      F1 = FL
      go to 250
c END OF INITIALIZATION
c
c
c ENTRY AFTER COMPUTING F FOR THE LAST ITERATE
   80 FF = F1
      TP = FF / FL
      if (TP) 90, 230, 110
   90 TP = FF / FO
      KS = 0
  100 FO = FL
      XO = XL
      go to 120
  110 KS = KS + 1
  120 J = 1
      if (FF .gt. C0) J = 2
      if (TP - C1) 150, 140, 130
  130 I = 4
      if (TP .gt. C4) I = 5
      go to 160
  140 I = 3
      go to 160
  150 I = 2
      if (TP .lt. CP01) I = 1
      if (TP .lt. CP99) go to 170
  160 KNKP(J) = KNKP(J) + 1
      go to 180
  170 KNKP(J) = 0
  180 XXMXO = XX - XO
      LNLP(J) = LCHG(5*LNLP(J) + I)
      if (LNLP(J) .ge. 4) then
         if (LNLP(3 - J) .ge. 4) go to 210
      ENDIF
c XXMXO GIVES THE LENGTH OF THE INTERVAL INSIDE WHICH
c THE ZERO IS KNOWN TO LIE.
      if (C2 * abs(XXMXO) .lt. abs(XXMXOL)) then
         KNKP(1) = 0
         KNKP(2) = 0
      ENDIF
      XXMXOL = XXMXO
      XRND = RND * (abs(XX) + abs(XO) + SMALL)
c
c TEST FOR CONVERGENCE
      if (TOL) 190, 200, 200
  190 continue
      if (abs(FF) .le. abs(TOL)) go to 220
  200 continue
      TOLX = max(TOL, XRND)
      if (abs(XXMXO) .gt. TOLX) go to 310
c
c CONVERGENCE -- PREPARE FOR FINAL EXIT
  210 if ((abs(XXMXO) .gt. TOL) .and. (TOL .ne. C0)) then
         INDIC = 3
         FDAT(3) = TOL
         if (XXMXO .gt. 0) then
            FDAT(2) = XX
            FDAT(1) = XO
         else
            FDAT(1) = XX
            FDAT(2) = XO
         ENDIF
      ENDIF
c SET FINAL VALUES FOR X1,F1,X2,AND F2
  220 continue
      if (abs(FF) .le. abs(FO)) go to 240
      F1 = FO
      X1 = XO
  230 FO = FF
      XO = XX
  240 X2 = XO
      F2 = FO
c TEST FOR DISCONTINUITY
      if ((KNKP(1) .gt. 5) .or. (KNKP(2) .gt. 5)) then
        INDIC = 4
        FDAT(1) = XX
      ENDIF
  250 MODE = INDIC
      if (INDIC - 2) 420, 420, 430
c END OF CODE FOR FINAL EXIT
c
c F NOT DECREASING (OR THE FIRST ITERATE)
c PREPARE TO DIVIDE THE INTERVAL
  260 TP = C1
      if (KS) 370, 280, 270
  270 if (KTYP .eq. 0) go to 290
  280 DIV = C2
  290 continue
      DIV = max(DIV, FFDFO)
c KTYP=0 IF AND ONLY IF THE INTERVAL WAS DIVIDED (USING DIV)
c ON THE LAST ITERATION
      if (KTYP .eq. 0) DIV = DIV * (C1P25 / (C1P25 - TP))
c DIVIDE THE INTERVAL AS SPECIFIED BY DIV
  300 TP1 = -XXMXO * (DIV/(DIV+C1))
      KTYP = 0
      go to 410
c
  310 continue
      XXMXL = XX - XL
      FFMFL = FF - FL
      FFDFO = abs(FF / FO)
      TOLX = CP5 * TOLX
      if (TP .ge. C1) go to 260
c DIVIDE THE INTERVAL IF F HAS HAD THE SAME SIGN FOR
c FOUR OR MORE TIMES IN SUCCESSION
      if (KS - 4) 320, 340, 290
  320 continue
      if (FLMFB .eq. C0) go to 340
c BEGINNING OF CODE TO DETERMINE IF INVERSE QUADRATIC
c INTERPOLATION IS TO BE USED.
      FFMFB = FFMFL + FLMFB
      if (FFMFB .eq. C0) go to 330
      QFM = C1 - (FFMFL / FLMFB) * (XLMXB / XXMXL)
      QXM = C1 - (XXMXL / XLMXB) * (FLMFB / FFMFL)
      DXDFFF = C1 + (FFMFL / FFMFB) * QFM
      DXDFFO = DXDFFF + C2 * ((FO - FF) / FFMFB) * QFM
      TP1 = XXMXL + XLMXB
      DFDXXX = C1 + (XXMXL / TP1) * QXM
      DFDXXO = DFDXXX + C2 * ((XO - XX) / TP1) * QXM
      TP1 = DXDFFF * DFDXXX
      if ((TP1 .le. CP25) .or. (TP1 .ge. C4)) go to 330
      TP1 = DXDFFO * DFDXXO
      if ((TP1 .gt. CP25) .and. (TP1 .lt. C4)) go to 380
c
c DERIVATIVES DO NOT MATCH WELL ENOUGH
  330 continue
      if (KS .eq. 0) if (FFDFO - C1) 350, 370, 360
  340 continue
      if ((KTYP .eq. 0) .and. (TP .ge. CP75)) go to 290
      continue
      TP = C1 - TP
      if (TP .le. FFDFO) go to 280
      FFDFO = FFDFO / TP
      DIV = CP125
      go to 290
  350 continue
      DIV = CP5 * max(max(CP25, FFDFO), TP / (C1P25 - min(TP, C1)))
      go to 300
  360 continue
      DIV = min(C4, CP5 * FFDFO)
      go to 300
c INTERPOLATE WITH SECANT METHOD
  370 TP1 = -XXMXL
      go to 390
c
c DERIVATIVES MATCH UP PRETTY WELL.
  380 continue
c INTERPOLATE USING THE INVERSE QUADRATIC
      TP1 = XXMXL * (QFM * (FL / FFMFB) - C1)
  390 TP1 = (FF/FFMFL) * TP1
      KTYP = 1
c
c EXIT TO GET F(X)
  410 continue
      FL = FF
      FLMFB = FFMFL
      XLMXB = XXMXL
      XL = XX
c COMPUTE X1, INSURING THAT IT IS NOT TOO CLOSE TO THE
c ENDS OF THE INTERVAL
      XX = min(max(XL + TP1, min(XL, XO) + TOLX), max(XL, XO) - TOLX)
      X1 = XX
  420 LMODE = MODE
      return
c
  430 MACT(2) = 11*INDIC  - 9
  440 MACT(4) = MLOC(INDIC-2)
      GO TO 1000
c     call SMESS(MACT, MTXTAA, IDAT, FDAT)
c      go to 420
c
c A CALL TO THE SUBROUTINE HAS BEEN MADE WITH MODE.NE.1
  450 IDAT(1) = MODE
      INDIC = 6
      MODE = 6
      if (LMODE .ne. 6) go to 430
      MACT(2) = 99
      go to 440
c
1000  MODE=6
      RETURN

c$$   END OF -ZERO SUBROUTINE
      end
      subroutine AMACH(MODE, I, I1, R1, D1)
c>> 1992-04-07 AMACH  Oken    Removed ^Z at EOF (error found by VAX compile)
c>> 1992-02-20 AMACH  Snyder  Added Cray-YMP stuff, q.v.
c>> 1990-06-11 AMACH  Snyder  Added Apollo DN-10000 stuff, q.v.
c>> 1990-12-14 AMACH  Lawson  Changed to eliminate ENTRY statements.
c>> 1990-08-21 AMACH  Krogh   No test was getting done for bad machine.
c>> 1990-02-28 AMACH  Krogh   Correct missing DOUBLE PRECISION AMSUB1
c>> 1989-08-14 AMACH  Krogh   Parameterized everything -- Massive change
c>> 1989-03-30 AMACH  Snyder  Correct missing "/" line 921
c>> 1989-01-30 AMACH  Snyder  Incorporate more constants from NETLIB.
C>> 1988-05-19 AMACH  Lawson  Initial code.
c File AMACH.FOR contains user-callable functions I1MACH, D1MACH, and
c R1MACH, plus second-level subroutines AMACH, AMTEST, and AMSUB1.
c Appropriate lines must be switched between comment and non-comment
c status when this code is moved to a different computer system.
c     These changes can be done with any text editor, however the "c++"
c lines permit automation of the change using the MARVEL processor.
c Note that when the MARVEL processor activates a line it shifts
c Columns 2-72 to 1-71 and puts a blank in Column 72.  When it inactiv-
c ates a line it shifts Columns 1-71 to 2-72 and puts a C in Column 1.
c     The possible choices using MARVEL are:
c              c++ SET SYS = IEEE
c              c++ SET SYS = AMDAHL 
c              c++ SET SYS = APOLLO-10000
c              c++ SET SYS = BUR1700 
c              c++ SET SYS = BUR5700 
c              c++ SET SYS = BUR67-7700 
c              c++ SET SYS = CDC60-7000 
c              c++ SET SYS = CONVEXC-1 
c              c++ SET SYS = CRAY1 
c              c++ SET SYS = CRAY1-SD (Sngl prec.arith. used for dble.)
c              c++ SET SYS = CRAY1-64 (64 bit integers)
c              c++ SET SYS = CRAY1-SD-64 (64 bit int, SP used for DP)
c              c++ SET SYS = CRAY-YMP
c              c++ SET SYS = DG-S2000 
c              c++ SET SYS = HARRIS220 
c              c++ SET SYS = HON600-6000 
c              c++ SET SYS = HON-DPS-8-70 
c              c++ SET SYS = IBM360-370 
c              c++ SET SYS = INTERDATA-8-32 
c              c++ SET SYS = PDP10-KA 
c              c++ SET SYS = PDP10-KB 
c              c++ SET SYS = PDP11 
c              c++ SET SYS = PRIME50 
c              c++ SET SYS = SEQ-BAL-8000 
c              c++ SET SYS = UNIVAC 
c              c++ SET SYS = VAX 
c     The current choice is:
c++ SET SYS = IEEE
c
C  I/O UNIT NUMBERS:
C
C    IM1 = I1MACH( 1) = THE STANDARD INPUT UNIT.
C    IM2 = I1MACH( 2) = THE STANDARD OUTPUT UNIT.
C    IM3 = I1MACH( 3) = THE STANDARD PUNCH UNIT.
C    IM4 = I1MACH( 4) = THE STANDARD ERROR MESSAGE UNIT.
C
C  WORDS:
C
C    IM5 = I1MACH( 5) = THE NUMBER OF BITS PER INTEGER STORAGE UNIT.
C    IM6 = I1MACH( 6) = THE NUMBER OF CHARACTERS/INTEGER STORAGE UNIT.
C
C  INTEGERS:
C
C    ASSUME INTEGERS ARE REPRESENTED IN THE S-DIGIT, BASE-A FORM
C
C               SIGN ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) )
C
C               WHERE 0 .LE. X(I) .LT. A FOR I=0,...,S-1.
C
C    IM7 = I1MACH( 7) = A, THE BASE.
C    IM8 = I1MACH( 8) = S, THE NUMBER OF BASE-A DIGITS.
C    IM9 = I1MACH( 9) = A**S - 1, THE LARGEST MAGNITUDE.
C
C  FLOATING-POINT NUMBERS:
C
C    ASSUME FLOATING-POINT NUMBERS ARE REPRESENTED IN THE T-DIGIT,
C    BASE-B FORM
C
C               SIGN (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
C
C               WHERE 0 .LE. X(I) .LT. B FOR I=1,...,T,
C               0 .LT. X(1), AND EMIN .LE. E .LE. EMAX.
C
C    IM10 = I1MACH(10) = B, THE BASE.
C
C  SINGLE-PRECISION:
C
C    IM11 = I1MACH(11) = T, THE NUMBER OF BASE-B DIGITS.
C    IM12 = I1MACH(12) = EMIN, THE SMALLEST EXPONENT E.
C    IM13 = I1MACH(13) = EMAX, THE LARGEST EXPONENT E.
C
C  DOUBLE-PRECISION:
C
C    IM14 = I1MACH(14) = T, THE NUMBER OF BASE-B DIGITS.
C    IM15 = I1MACH(15) = EMIN, THE SMALLEST EXPONENT E.
C    IM16 = I1MACH(16) = EMAX, THE LARGEST EXPONENT E.
C
C  CONVERSION FROM FUNCTIONAL TO STRUCTURAL FLOATING POINT CONSTANTS
C
C    IM17 = CONSTANT SUCH THAT IM14 + IM17 = ACTUAL NUMBER OF BASE-B
C           DIGITS IN DOUBLE PRECISION, USED FOR CHECKING THAT CORRECT
C           VERSION OF THIS PROGRAM IS INSTALLED.  (SEE DEFINITION OF
C           DM6, AND THE USE OF DM6 IN CALLING AMTEST.)
C
C  TO ALTER THIS FUNCTION FOR A PARTICULAR ENVIRONMENT,
C  THE DESIRED SET OF PARAMETER STATEMENTS SHOULD BE ACTIVATED BY
C  REMOVING THE C FROM COLUMN 1.  ALSO, THE VALUES OF
C  IM1 - IM4 SHOULD BE CHECKED FOR CONSISTENCY
C  WITH THE LOCAL OPERATING SYSTEM.
c     -----------------------------------------------------------------
c     Original design and code due to P. A. Fox, A. D. Hall, and
c     N. L. Schryer, Bell Laboratories.  See ACM TOMS, 4,(1978),177-188.
c     Adapted to Univac 1100 by Kris Stewart, JPL, 7/30/81.
c     Adapted for the JPL MATH77 library by C. L. Lawson and F. T. Krogh
c     Sept, 1987.
c     1989-08-14 AMACH  Krogh   Parameterized everything. Major changes.
C     1990 Dec. CLL reorganized code to avoid using ENTRY statements
c     for functions of different types.  Also added save statements.
c     -----------------------------------------------------------------
c     On the first call to this function, tests are done to verify that
c     IM10 and IM14 are not grossly wrong for the host environment.
c     This gives some protection against using the wrong version of this
c     subprogram.
c     -----------------------------------------------------------------
      integer MODE, I, I1
      real R1
      double precision D1, TEST
c
      integer IMACH(16)
      integer IM1, IM2, IM3, IM4, IM5, IM6, IM7, IM8, IM9, IM10, IM11,
     1   IM12, IM13, IM14, IM15, IM16, IM17
      real             RMACH(5), RM1, RM2, RM3, RM4, RM5,
     1                 RMA, RMB, RBASE
      double precision DMACH(5), DM1, DM2, DM3, DM4, DM5, DM6,
     1                 DMA, DMB, DBASE
      save TEST, IMACH, RMACH, DMACH
C     -----------------------------------------------------------------
C     Machine constants for IEEE standard binary floating-point
c     processors.  This includes PC's and work-stations using the
c     Intel 8087, 80287, 80387, ... processors or the
c     Motorola 68881, 68882, ... processors.
c     Note:  We are setting the "most negative exponent" (IMACH(12) and
c     IMACH(15)) to be the exponent of the smallest normalized number.
c     An IEEE processor actually handles smaller numbers before
c     underflowing, however these "unnormalized" numbers have
c     diminished precision.
c
c++ Code for SYS = IEEE is ACTIVE
       PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
       PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
       PARAMETER (IM10 =2, IM11 =24, IM12 =-125, IM13 =128)
       PARAMETER (IM14 =53, IM15 =-1021, IM16 =1024, IM17=0)
C     -----------------------------------------------------------------
c++ Code for SYS = AMDAHL is INACTIVE
CC     MACHINE CONSTANTS FOR AMDAHL MACHINES.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
C      PARAMETER (IM10 =16, IM11 =6, IM12 =-64, IM13 =63)
C      PARAMETER (IM14 =14, IM15 =-64, IM16 =63, IM17=0)
C      -----------------------------------------------------------------
c++ Code for SYS = APOLLO-10000 is INACTIVE
cc     MACHINE CONSTANTS FOR APOLLO DN-10000 MACHINES.
cc     The only difference from IEEE is IM13.  This difference has
cc     nothing to do with the arithmetic or representation used by the
cc     machine.  It is caused by a bug in the compiler:  The right-hand
cc     side of RM2 (below) is apparently evaluated in double precision.
cc     When the compiler is ready to store the resulting value into its
cc     internal data structures, it compares it to an incorrect value
cc     of the overflow limit.  It appears the incorrect value has the
cc     correct exponent, but the fraction is 1.5 instead of 2-2**(-p),
cc     where p is the precision in bits.  You can get the correct result
cc     by changing IM13 to 128, changing RM2 from a parameter to a
cc     variable, and changing the parameter statement that assigns a
cc     value to RM2 into an ordinary assignment statement.
CC
c      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
c      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
c      PARAMETER (IM10 =2, IM11 =24, IM12 =-125, IM13 =127)
c      PARAMETER (IM14 =53, IM15 =-1021, IM16 =1024, IM17 =0)
CC     -----------------------------------------------------------------
c++ Code for SYS = BUR1700 is INACTIVE
CC     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM.
CC
C      PARAMETER (IM1 =7, IM2 =2, IM3 =2, IM4 =2)
C      PARAMETER (IM5 =36, IM6 =4, IM7 =2, IM8 =33)
C      PARAMETER (IM10 =2, IM11 =24, IM12 =-256, IM13 =255)
C      PARAMETER (IM14 =60, IM15 =-256, IM16 =255, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = BUR5700 is INACTIVE
CC     MACHINE CONSTANTS FOR THE BURROUGHS 5700 SYSTEM.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =48, IM6 =6, IM7 =2, IM8 =39)
C      PARAMETER (IM10 =8, IM11 =13, IM12 =-50, IM13 =76)
C      PARAMETER (IM14 =26, IM15 =-50, IM16 =76, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = BUR67-7700 is INACTIVE
CC     MACHINE CONSTANTS FOR THE BURROUGHS 6700/7700 SYSTEMS.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =48, IM6 =6, IM7 =2, IM8 =39)
C      PARAMETER (IM10 =8, IM11 =13, IM12 =-50, IM13 =76)
C      PARAMETER (IM14 =26, IM15 =-32754, IM16 =32780, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = CDC60-7000 is INACTIVE
CC     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =60, IM6 =10, IM7 =2, IM8 =48)
C      PARAMETER (IM10 =2, IM11 =47, IM12 =-929, IM13 =1070)
C      PARAMETER (IM14 =94, IM15 =-929, IM16 =1069, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = CONVEXC-1 is INACTIVE
CC     MACHINE CONSTANTS FOR CONVEX C-1.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
C      PARAMETER (IM10 =2, IM11 =24, IM12 =-128, IM13 =127)
C      PARAMETER (IM14 =53, IM15 =-1024, IM16 =1023, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = CRAY1 is INACTIVE
CC     MACHINE CONSTANTS FOR THE CRAY 1, XMP, 2, AND 3.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =102, IM4 =6)
C      PARAMETER (IM5 =64, IM6 =8, IM7 =2, IM8 =46)
C      PARAMETER (IM10 =2, IM11 =47, IM12 =-8189, IM13 =8190)
C      PARAMETER (IM14 =94, IM15 =-8099, IM16 =8190, IM17=2)
CC     -----------------------------------------------------------------
c++ Code for SYS = CRAY-YMP is INACTIVE
CC     MACHINE CONSTANTS FOR THE CRAY YMP
CC     Cray claims the overflow exponent (IM13 and IM16) is 8189, and
CC     the underflow exponent (IM12 and IM15) is -8189, but these values
CC     don't seem to work in cf77:  the underflow limit underflows, and
CC     the overflow limit overflows when using Cray's values.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =102, IM4 =6)
C      PARAMETER (IM5 =64, IM6 =8, IM7 =2, IM8 =46)
C      PARAMETER (IM10 =2, IM11 =47, IM12 =-8188, IM13 =8189)
C      PARAMETER (IM14 =94, IM15 =-8188, IM16 =8189, IM17=2)
CC     -----------------------------------------------------------------
c++ Code for SYS = CRAY1-SD is INACTIVE
CC     MACHINE CONSTANTS FOR THE CRAY 1, XMP, 2, AND 3, WHEN DOUBLE
CC     PRECISION IS TO USE SINGLE PRECISION ARITHMETIC.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =102, IM4 =6)
C      PARAMETER (IM5 =64, IM6 =8, IM7 =2, IM8 =46)
C      PARAMETER (IM10 =2, IM11 =47, IM12 =-8189, IM13 =8190)
C      PARAMETER (IM14 =47, IM15 =-8189, IM16 =8190, IM17=1)
CC     -----------------------------------------------------------------
c++ Code for SYS = CRAY1-64 is INACTIVE
CC     MACHINE CONSTANTS FOR THE CRAY 1, XMP, 2, AND 3.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =102, IM4 =6)
C      PARAMETER (IM5 =64, IM6 =8, IM7 =2, IM8 =63)
C      PARAMETER (IM10 =2, IM11 =47, IM12 =-8189, IM13 =8190)
C      PARAMETER (IM14 =94, IM15 =-8099, IM16 =8190, IM17=2)
CC     -----------------------------------------------------------------
c++ Code for SYS = CRAY1-SD-64 is INACTIVE
CC     MACHINE CONSTANTS FOR THE CRAY 1, XMP, 2, AND 3, WHEN DOUBLE
CC     PRECISION IS TO USE SINGLE PRECISION ARITHMETIC.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =102, IM4 =6)
C      PARAMETER (IM5 =64, IM6 =8, IM7 =2, IM8 =63)
C      PARAMETER (IM10 =2, IM11 =47, IM12 =-8189, IM13 =8190)
C      PARAMETER (IM14 =47, IM15 =-8189, IM16 =8190, IM17=1)
CC     -----------------------------------------------------------------
c++ Code for SYS = DG-S2000 is INACTIVE
CC     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200
CC
C      PARAMETER (IM1 =11, IM2 =12, IM3 =8, IM4 =10)
C      PARAMETER (IM5 =16, IM6 =2, IM7 =2, IM8 =15)
C      PARAMETER (IM10 =16, IM11 =6, IM12 =-64, IM13 =63)
C      PARAMETER (IM14 =14, IM15 =-64, IM16 =63, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = HARRIS220 is INACTIVE
CC     MACHINE CONSTANTS FOR THE HARRIS 220, SLASH 6, SLASH 7.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =0, IM4 =6)
C      PARAMETER (IM5 =24, IM6 =3, IM7 =2, IM8 =23)
C      PARAMETER (IM10 =2, IM11 =23, IM12 =-127, IM13 =127)
C      PARAMETER (IM14 =38, IM15 =-127, IM16 =127, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = HON600-6000 is INACTIVE
CC     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000 SERIES.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =43, IM4 =6)
C      PARAMETER (IM5 =36, IM6 =6, IM7 =2, IM8 =35)
C      PARAMETER (IM10 =2, IM11 =27, IM12 =-127, IM13 =127)
C      PARAMETER (IM14 =63, IM15 =-127, IM16 =127, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = HON-DPS-8-70 is INACTIVE
CC     MACHINE CONSTANTS FOR THE HONEYWELL DPS 8/70 SERIES.
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =43, IM4 =6)
C      PARAMETER (IM5 =36, IM6 =4, IM7 =2, IM8 =35)
C      PARAMETER (IM10 =2, IM11 =27, IM12 =-127, IM13 =127)
C      PARAMETER (IM14 =63, IM15 =-127, IM16 =127, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = IBM360-370 is INACTIVE
CC     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,
CC     THE XEROX SIGMA 5/7/9 AND THE SEL SYSTEMS 85/86.
C
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
C      PARAMETER (IM10 =16, IM11 =6, IM12 =-64, IM13 =63)
C      PARAMETER (IM14 =14, IM15 =-64, IM16 =63, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = INTERDATA-8-32 is INACTIVE
CC     MACHINE CONSTANTS FOR THE INTERDATA 8/32
CC     WITH THE UNIX SYSTEM FORTRAN 77 COMPILER.
CC 
C      PARAMETER (IM1 =5, IM2 =6, IM3 =6, IM4 =6)
C      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
C      PARAMETER (IM10 =16, IM11 =6, IM12 =-64, IM13 =62)
C      PARAMETER (IM14 =14, IM15 =-64, IM16 =62, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = PDP10-KA is INACTIVE
CC     MACHINE CONSTANTS FOR THE PDP-10 (KA PROCESSOR).
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =36, IM6 =5, IM7 =2, IM8 =35)
C      PARAMETER (IM10 =2, IM11 =27, IM12 =-128, IM13 =127)
C      PARAMETER (IM14 =54, IM15 =-101, IM16 =127, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = PDP10-KB is INACTIVE
CC     MACHINE CONSTANTS FOR THE PDP-10 (KI PROCESSOR).
CC
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =36, IM6 =5, IM7 =2, IM8 =35)
C      PARAMETER (IM10 =2, IM11 =27, IM12 =-128, IM13 =127)
C      PARAMETER (IM14 =62, IM15 =-128, IM16 =127, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = PDP11 is INACTIVE
CC     MACHINE CONSTANTS FOR PDP-11 FORTRAN'S SUPPORTING
CC     16-BIT INTEGER ARITHMETIC.
C
C      PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C      PARAMETER (IM5 =16, IM6 =2, IM7 =2, IM8 =15)
C      PARAMETER (IM10 =2, IM11 =24, IM12 =-127, IM13 =127)
C      PARAMETER (IM14 =56, IM15 =-127, IM16 =127, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = PRIME50 is INACTIVE
CC     MACHINE CONSTANTS FOR THE PRIME 50 SERIES SYSTEMS
CC     WITH 32-BIT INTEGERS AND 64V MODE INSTRUCTIONS,
CC     SUPPLIED BY IGOR BRAY.
C
C      PARAMETER (IM1 =1, IM2 =1, IM3 =2, IM4 =1)
C      PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
C      PARAMETER (IM10 =2, IM11 =23, IM12 =-127, IM13 =127)
C      PARAMETER (IM14 =47, IM15 =-32895, IM16 =32637, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = SEQ-BAL-8000 is INACTIVE
CC     MACHINE CONSTANTS FOR THE SEQUENT BALANCE 8000.
CC
C      PARAMETER (IM1 =0, IM2 =0, IM3 =7, IM4 =0)
C      PARAMETER (IM5 =32, IM6 =1, IM7 =2, IM8 =31)
C      PARAMETER (IM10 =2, IM11 =24, IM12 =-125, IM13 =128)
C      PARAMETER (IM14 =53, IM15 =-1021, IM16 =1024, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = UNIVAC is INACTIVE
CC     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES.
CC
CC     NOTE THAT THE PUNCH UNIT, I1MACH(3), HAS BEEN SET TO 1
CC     WHICH IS APPROPRIATE FOR THE UNIVAC-FTN SYSTEM.
CC     IF YOU HAVE THE UNIVAC-FOR SYSTEM, SET IT TO 7.
CC     IM6 = 4 for FTN (4 chars per word), 6 for FOR (6 chars per word).
Cc
C      PARAMETER (IM1 =5, IM2 =6, IM3 =1, IM4 =6)
C      PARAMETER (IM5 =36, IM6 =4, IM7 =2, IM8 =35)
C      PARAMETER (IM10 =2, IM11 =27, IM12 =-128, IM13 =127)
C      PARAMETER (IM14 =60, IM15 =-1024, IM16 =1023, IM17=0)
CC     -----------------------------------------------------------------
c++ Code for SYS = VAX is INACTIVE
C     MACHINE CONSTANTS for the VAX/VMS
C     and for PDP-11 FORTRAN SUPPORTING 32-BIT INTEGER ARITHMETIC.
C
C     PARAMETER (IM1 =5, IM2 =6, IM3 =7, IM4 =6)
C     PARAMETER (IM5 =32, IM6 =4, IM7 =2, IM8 =31)
C     PARAMETER (IM10 =2, IM11 =24, IM12 =-127, IM13 =127)
C     PARAMETER (IM14 =56, IM15 =-127, IM16 =127, IM17=0)
c++ end
C     -----------------------------------------------------------------
C
      PARAMETER (IM9 = 2 * (2**(IM8-1) - 1) + 1)
C
C Real parameters
C
C  RM1 = R1MACH(1) = B**(EMIN-1), The smallest positive number, i.e.,
c                    the underflow limit.
C  RM2 = R1MACH(2) = B**EMAX*(1 - B**(-T)), The largest number, i.e.,
c                    the overflow limit.
C  RM3 = R1MACH(3) = B**(-T), The smallest relative spacing, i.e., the
c                    difference between 1.0 and the next smaller number.
C  RM4 = R1MACH(4) = B**(1-T), The largest relative spacing, i.e., the
c                     difference between 1.0 and the next larger number.
C  RM5 = R1MACH(5) = LOG10(B).  When B = 2 this value is
c              Log10(2) = 0.30102_99956_63981_19521_37388_94724
C
C Parameter RMA and RMB are selected so that for values of the base =
C 2, 8, 16, 10, RMA has the values 1, 3, 4, 0, and RMB has the values 0,
C 0, 0, 1.  These values are used in computing RM5.
C $$$$ Note that if other bases are to be supported, the calculation of
C $$$$ RMA and RMB will have to be generalized.
C
      PARAMETER (RMA = ((IM10 - 10) * (-3 + ((IM10 - 2) * (-77 +
     1    12 * (IM10 - 8))) / 14)) / 24)
      PARAMETER (RMB = ((IM10 - 2) * (IM10 - 8) * (16 - IM10)) / 96)
      PARAMETER (RBASE = IM10)
C
C     Weird subterfuges below are NECESSARY to compute DM1 and DM2 on
C     some systems.  DON'T SIMPLIFY THEM.  We compute RM1 and RM2 using
C     these subterfuges so it will be clear we're computing the REAL and
C     DOUBLE PRECISION characteristics in the same way.
      PARAMETER (RM1 = (RBASE**(IM12/2)) * (RBASE**(IM12-IM12/2-1)))
      PARAMETER (RM2 = RBASE**(IM13-IM11) * ((RBASE**IM11 - RBASE)
     1               + (RBASE - 1.0E0)))
      PARAMETER (RM3 = RBASE**(-IM11))
      PARAMETER (RM4 = RBASE**(1-IM11))
c     PARAMETER (RM5 = RMA*0.30102 99956 63981 19521 37388 94724E0+RMB)
      PARAMETER (RM5 = RMA*0.301029995663981195213738894724E0+RMB)
C
C Double precision paramters -- (Defined like the real ones.)
C
      PARAMETER (DMA = ((IM10 - 10) * (-3 + ((IM10 - 2) * (-77 +
     1    12 * (IM10 - 8))) / 14)) / 24)
      PARAMETER (DMB = ((IM10 - 2) * (IM10 - 8) * (16 - IM10)) / 96)
      PARAMETER (DBASE = IM10)
C
C     Weird subterfuges below are NECESSARY to compute DM1 and DM2 on
C     some systems.  DON'T SIMPLIFY THEM.
      PARAMETER (DM1 = (DBASE**(IM15/2)) * (DBASE**(IM15-IM15/2-1)))
      PARAMETER (DM2 = DBASE**(IM16-IM14) * ((DBASE**IM14 - DBASE)
     1               + (DBASE - 1.0D0)))
      PARAMETER (DM3 = DBASE**(-IM14))
      PARAMETER (DM4 = DBASE**(1-IM14))
c     PARAMETER (DM5 = DMA*0.30102 99956 63981 19521 37388 94724D0+DMB)
      PARAMETER (DM5 = DMA*0.301029995663981195213738894724D0+DMB)
C DM6 and TEST are used in checking that the correct constants have been
C selected.
      PARAMETER (DM6 = DBASE**(-IM14-IM17))
      data TEST / 0.D0 /
C
c     DATA IMACH / IM1, IM2, IM3, IM4, IM5, IM6, IM7, IM8, IM9, IM10,
c    1   IM11, IM12, IM13, IM14, IM15, IM16 /
c     DATA RMACH / RM1, RM2, RM3, RM4, RM5 /
c     DATA DMACH / DM1, DM2, DM3, DM4, DM5 /
C     -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      if (TEST .eq. 0.0D0) then
         IMACH(1) = IM1
         IMACH(2) = IM2
         IMACH(3) = IM3
         IMACH(4) = IM4
         IMACH(5) = IM5
         IMACH(6) = IM6
         IMACH(7) = IM7
         IMACH(8) = IM8
         IMACH(9) = IM9
         IMACH(10) = IM10
         IMACH(11) = IM11
         IMACH(12) = IM12
         IMACH(13) = IM13
         IMACH(14) = IM14
         IMACH(15) = IM15
         IMACH(16) = IM16
         RMACH(1) = RM1
         RMACH(2) = RM2
         RMACH(3) = RM3
         RMACH(4) = RM4
         RMACH(5) = RM5
         DMACH(1) = DM1
         DMACH(2) = DM2
         DMACH(3) = DM3
         DMACH(4) = DM4
         DMACH(5) = DM5
         CALL AMTEST (TEST, DM6)
      ENDIF

      if (MODE .eq. 0) then
         I1=IMACH(I)
      else if (MODE .eq. 1) then
         R1=RMACH(I)
c                                  Here we assume MODE = 2.
      else
         D1=DMACH(I)
      endif
      return
      end
c     ==================================================================
      integer function I1MACH(I)
      integer I, I1
      real R1
      double precision D1
c      IF (I .LT. 1  .OR.  I .GT. 16) THEN
c         PRINT*,'I1MACH.. Bad argument: I =',I
c         STOP 'I1MACH error'
c      ENDIF
      call AMACH (0, I, I1, R1, D1)
      I1MACH = I1
      return
      end
c     ==================================================================
c
      real function R1MACH(I)
      integer I, I1
      real R1
      double precision D1
c      IF (I .lt. 1  .or.  I .gt. 5) THEN
c         print*,'R1MACH.. Bad argument: I = ',I
c         stop 'R1MACH error'
c      ENDIF
      call AMACH (1, I, I1, R1, D1)
      R1MACH = R1
      RETURN
      end
c     ==================================================================
c
      double precision function D1MACH(I)
      integer I, I1
      real R1
      double precision D1
c      IF (I .lt. 1  .or.  I .gt. 5) THEN
c         print*,'D1MACH.. Bad argument: I = ',I
c         stop 'D1MACH error'
c      ENDIF
      call AMACH (2, I, I1, R1, D1)
      D1MACH = D1
      RETURN
      END
c     ==================================================================
c
      SUBROUTINE AMTEST (TEST, D6)
c Verifies that D6 is an appropriate values for DM6.
      DOUBLE PRECISION AMSUB1, D6, TEST
      TEST = AMSUB1(1.D0 + D6)
C
C The comparison with 1.875E0*D6 in the line below is to guard
C against the possibility that TEST is > 0 as a result of rounding
C up in the addition of D6 to 1.
C
      IF ((TEST .eq. 0.D0) .or. (TEST .gt. 1.875D0*D6)) THEN
         TEST = (D6 + D6) + 1.D0
         IF (AMSUB1(TEST) .ne. 0.D0) RETURN
      ENDIF
c      print*,'AMACH has bad parameters for current environment.'
c      stop
      RETURN
      END
c     ==================================================================
c
      DOUBLE PRECISION FUNCTION AMSUB1 (TEST1)
      DOUBLE PRECISION TEST1
C     Returns the value of TEST1 - 1.
      AMSUB1 = TEST1 - 1.0D0
      RETURN
      END
