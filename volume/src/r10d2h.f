!== last modified  9-19-2006
      SUBROUTINE r10d2h(VOLEQ,DBHOB,HTTOT,VOL,CUTFLG,CUPFLG,BFPFLG,
     >                                                          ERRFLAG)

C    subroutine implements volume equations developed by Larsen and Winterberger.
c            PNW-RN-478.
c    aug, 2000.  KLC

      CHARACTER*10 VOLEQ
      REAL DBHOB,HTTOT,VOL(15)
      INTEGER CUTFLG,CUPFLG,BFPFLG,ERRFLAG

      REAL D2H,VOLM
      
      ERRFLAG = 0
      IF(DBHOB.LE.1.0)THEN
        ERRFLAG = 3
        RETURN
      ENDIF
      IF(HTTOT.LE.0)THEN
        ERRFLAG = 4
        RETURN
      ENDIF

      D2H = DBHOB*DBHOB * HTTOT

      if(voleq(8:10) .eq. '094') then
        IF(CUTFLG .EQ. 1)THEN
           VOLM = 0.65559+0.00191*D2H
           VOL(1) = VOLM
        ENDIF

        IF(CUPFLG .EQ. 1)THEN
           VOLM = -0.21849 + 0.00189*D2H
           VOL(4) = VOLM
           IF(VOL(4) .LT. 0) VOL(4) = 0
        ENDIF

	   IF(BFPFLG.EQ. 1)THEN
	      VOLM = 0.000136 * (D2H**1.40338)
	      VOL(2) = VOLM
         ENDIF
      ENDIF

      RETURN
      END
