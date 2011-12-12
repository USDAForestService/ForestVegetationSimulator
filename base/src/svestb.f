      SUBROUTINE SVESTB (I1)
      IMPLICIT NONE
C----------
C  **SVESTB--BASE  DATE OF LAST REVISION: 04/20/08
C----------
C     STAND VISUALIZATION GENERATION
C     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998
C     J.J.MARCHINEK -- RMRS MOSCOW -- MARCH 1999
C     D. ROBINSON   -- ESSA        -- MAY 2005
C
C     I1= 0 IS A CALL FROM SVSTART
C         1 IS A CALL FROM GRADD
C
C     NOTE THAT THE DBH, PROB, ITRE (POINT ID) FOR ALL THE TREES
C     MUST BE SET PRIOR TO CALLING THIS ROUTINE FOR THE NEW TREE.

COMMONS

      INCLUDE 'PRGPRM.F77'

      INCLUDE 'ARRAYS.F77'

      INCLUDE 'CONTRL.F77'

      INCLUDE 'SVDATA.F77'

      INCLUDE 'SVDEAD.F77'

      INCLUDE 'PLOT.F77'

      INCLUDE 'WORKCM.F77'

      INCLUDE 'METRIC.F77'

COMMONS

C     PPP     - SUM OF PROB PER PLOT
C     IPLCNT  - SUM OF OBJECT TREES PER PLOT
C     NOBTS   - SUM OF OBJECT TREES PER TREE RECORD
C     NTOADD  - SUM OF NUMBER TO ADD PER TREE RECORD
C     RMVSUM  - INCREASING SUM OF (ADD/NEW) PROB
C     FRACAD  - FRACTIONAL TREES PER ACRE OF NEW TREES (ie <1)
C     NTRPLT  - A LIST OF INDICIES OF THE TREES ON THE CURRENT PLOT
C     ITARGN  - TIME AFTER (SINCE) REGENERATION
C     IESCYC  - ESTAB CYCLE
C     ISNADD - VECTOR OF SNAGS THAT NEED TO BE ADDED

      INTEGER I1,II,I,ISVOBJ,ITOTAL,IPLT,N,IX,NTA,ITOADD,IADD,NN,
     >        NP,IPT,IT,NCKS,KODE,ICNT,J,ITEMP,NSNAGS,IYEAR
      REAL    TPPP,X,START,DIAI,Y,DIAOBJ,YS,XS,DIFF,TDIFF,
     >        TOADD,XINT,XM

      REAL PPP(MAXPLT)
      INTEGER IPLCNT(MAXPLT)
      INTEGER NTRPLT(MAXTRE)
      INTEGER NOBTS(MAXTRE)
      REAL FRACAD(MAXTRE)
      REAL RMVSUM(MAXTRE)
      INTEGER NTOADD(MAXTRE)
      INTEGER ISNADD(MXSVOB)
      CHARACTER*3 SBLANK
      INTEGER*4 IDCMP1,IDCMP2, IESCYC, ITARGN(MAXTRE)
      LOGICAL DEBUG
      DATA IDCMP1,IDCMP2/10000000,20000000/
      CALL DBCHK (DEBUG,'SVESTB',6,ICYC)

      IF (DEBUG) WRITE (JOSTND,5) JSVOUT,I1,ICYC,IREC2,ISVINV
    5 FORMAT (/' IN SVESTB:  JSVOUT=',I3,' I1=',I6,' ICYC=',I3,
     >  ' IREC2=',I5,' ISVINV=',I5)

      IF (JSVOUT.EQ.0) RETURN

C     IF THERE ARE NO LIVE TREES, SKIP TO PROCESS DEAD TREES.

      IF (ITRN.EQ.0) GOTO 200

C     INITIALIZATION OF VECTORS

      DO II = 1,MAXPLT
        PPP(II) = 0
        IPLCNT(II) = 0
      ENDDO
      DO I = 1,MAXTRE
        NOBTS(I) = 0
        NTOADD(I) = 0
        FRACAD(I) = 0
        ITARGN(I) = -1
      ENDDO

      TPPP=0
      DO II = 1,ITRN
        I = ITRE(II)
        IF (IMETRIC.EQ.0) THEN
          PPP(I) = PROB(II) + PPP(I)
        ELSE
          PPP(I) = (PROB(II)/ACRtoHA) + PPP(I)
        ENDIF
      ENDDO

      DO II=1, ISVINV
        TPPP = PPP(II)+TPPP
      ENDDO

      IF (NSVOBJ.GT.0) THEN
        DO ISVOBJ = 1,NSVOBJ
          II = IS2F(ISVOBJ)
          IF(II.GT.0 .AND. IOBJTP(ISVOBJ).EQ.1) THEN
            I = ITRE(II)
            IPLCNT(I) = IPLCNT(I) + 1
          ENDIF
        ENDDO

C       LOAD NUMBER OF OBJECTS PER TREE RECORD IN NOBTS

        DO ISVOBJ =  1,NSVOBJ
          IF (IOBJTP(ISVOBJ).EQ.1 .AND. IS2F(ISVOBJ).GT.0) THEN
            NOBTS(IS2F(ISVOBJ)) = NOBTS(IS2F(ISVOBJ)) + 1
          ENDIF
        ENDDO
      ENDIF

C     ADD INTEGER VALUE TREE RECORDS AND FIND FRACTIONAL VALUES

      ITOTAL = 0
      IESCYC = 0
      DO I = 1,ITRN

        IF (IDTREE(I) .LT. IDCMP2) THEN

C       RECORD IS FROM NEW ESTABLISHMENT

          IESCYC = IDTREE(I) - IDCMP1
          IESCYC = (IESCYC - MOD(IESCYC, 10000))/10000
          ITARGN(I) = ICYC - IESCYC
        ENDIF

        IF (DEBUG) WRITE (JOSTND,*) 'I=',I,
     >    ' IESCYC=',IESCYC, ' ITARGN',ITARGN(I),
     >    ' IDTREE = ', IDTREE(I)

        IF (I1.EQ.0 .OR. ITARGN(I).EQ.0) THEN

          IF (IMETRIC.EQ.0) THEN
            XM = PROB(I)
          ELSE
            XM = PROB(I)/ACRtoHA
          ENDIF

          IF(NOBTS(I) .LT. XM) THEN
            IF(NOBTS(I) .LE. (XM-1.)) THEN
              NTOADD(I) = IFIX(XM - NOBTS(I))
            ENDIF
            FRACAD(I) = XM - NOBTS(I) - NTOADD(I)
          ELSE
            FRACAD(I) = 0
          ENDIF

        ENDIF
        ITOTAL = NTOADD(I) + ITOTAL + NOBTS(I)
      ENDDO

      IF (DEBUG) THEN
        X = 0
        DO I = 1,ITRN
          X = NTOADD(I) + X
        ENDDO
        WRITE (JOSTND,*) 'NONFRACTIONAL TOTAL TO HAVE=',
     >    X, ITOTAL
      ENDIF

C     USING THE FRACTIONAL NUMBER OF TREES CALCULATE AND LOAD
C     THE NUMBER OF TREES TO ADD PER TREE RECORD PER PLOT

      DO IPLT = 1,ISVINV
        IF (DEBUG) WRITE (JOSTND,*) '--------------ADD IPLT=',
     >    IPLT,'----------------'

C       INITALIZE RMVSUM

        DO I = 1,ITRN
          RMVSUM(I) = 0
        ENDDO

C       FIND TOTAL NUMBER OF TREES RECORDS FOR EACH PLOT

        N = 0
        DO I = 1,ITRN
          IF(ITRE(I).EQ.IPLT) THEN
            N = N + 1
            NTRPLT(N) = I
          ENDIF
        ENDDO

C       IF THERE ARE NO TREES ON THIS PLOT GO TO NEXT PLOT

        IF (N.EQ.0) GOTO 20

C       SORT NTRPLT ON DBH

        CALL RDPSRT(N,DBH,NTRPLT,.FALSE.)

C       LOAD RMVSUM WITH FRACTIONAL NEW TREES IN ORDER BY NTRPLT

        IX = NTRPLT(1)
        RMVSUM(IX) = FRACAD(IX)
        IF (DEBUG) WRITE (JOSTND,*) 'RMVSUM(',IX,')=',RMVSUM(IX)
        IF(N.GT.1) THEN
          DO II = 2,N
            I = NTRPLT(II)
            RMVSUM(I) = FRACAD(I) + RMVSUM(IX)
            IX = I
            IF (DEBUG) WRITE (JOSTND,*) 'RMVSUM(',I,')=',RMVSUM(I)
          ENDDO
        ENDIF

C       CALCULATE THE NUMBER OF TREES ADDED TO THIS PLOT

        NTA = 0
        DO I = 1,N
          NTA = NTOADD(NTRPLT(I)) + NTA
        ENDDO

C       SETUP INFORMATION TO START DECIDING WHAT TREES SHOULD BE ADDED

        TOADD = PPP(IPLT) - FLOAT(IPLCNT(IPLT)) - NTA
        IF (TOADD.LE.0.0) GOTO 20
        ITOADD = IFIX(TOADD) + 1
        XINT = RMVSUM(NTRPLT(N)) / (TOADD)
        CALL SVRANN(X)
        START = XINT * X
        X = 0
        IF (DEBUG) WRITE (JOSTND,*) 'N=',N,'ITOADD=', ITOADD,
     >    ' XINT=',XINT,' START=',START,'NTA=', NTA
        IADD = 0

C       LOOP TO FIND WHICH TREES SHOULD BE ADDED

        NTA = NTA + IPLCNT(IPLT)
        DO II = 1,N
          IF (DEBUG) WRITE (JOSTND,*)  'NTA=',NTA, 'PPP(IPLT)=',
     >      PPP(IPLT),'ITOTAL=',ITOTAL, 'TPPP=',TPPP
          IF (ITOTAL.GE.TPPP) GOTO 30
          IF (NTA .GE. PPP(IPLT)) GOTO 20
          I = NTRPLT(II)
          IF (DEBUG) WRITE(JOSTND,*)'START=',START,
     >      ' RMVSUM(',I,')=',RMVSUM(I),' X=',X
          IF(START.GT.X .AND. START.LT.RMVSUM(I)) THEN
            IF (I1 .GT. 0) THEN
              IF (ITARGN(I).NE.0) THEN
                X = (I)
                START = START + XINT
                IF (DEBUG) WRITE (JOSTND,*) 'TOO BIG'
                GOTO 15
              ENDIF
            ENDIF
            START = START + XINT
            NTOADD(I) = NTOADD(I) + 1
            ITOTAL = ITOTAL + 1
            IADD = IADD + 1
            NTA = NTA + 1
            X = RMVSUM(I)

            IF (DEBUG) WRITE (JOSTND,*) 'IADD=',IADD,
     >        ' NTOADD(', I,')=',
     >        NTOADD(I), ' FRACAD(I)=',FRACAD(I)
            IF (IADD.GE.ITOADD) GOTO 20
          ELSE
            X = RMVSUM(I)
          ENDIF
   15     CONTINUE
        ENDDO
   20   CONTINUE
      ENDDO
   30 CONTINUE

C     USED FOR DEBUG PURPOSES

      IF (DEBUG) THEN
        X = 0
        DO I = 1,ITRN
          X = NTOADD(I) + X
          WRITE (JOSTND,*) 'I=',I,'PROB(I)=',PROB(I)/ACRtoHA,
     >      'NTOADD(I)=', NTOADD(I), 'NOBTS(I)=', NOBTS(I),
     >      'TOTAL=', NTOADD(I)+NOBTS(I)
        ENDDO
        WRITE (JOSTND,*) 'NEED ADDING=', X
      ENDIF

C     UPDATING AND CHECKING TO SEE WHERE CALL WAS MADE FROM

      IF (NSVOBJ.EQ.0) THEN
        NN = 2
      ELSE
        NN = 1
      ENDIF

      DO II = 1,NN
        DO I = 1,ITRN
          NP = NTOADD(I)
          IF (NP.GT.0) THEN

C           THE TREE WON THE TOSS...PLACE IT...NP TIMES
C           1/12 * .5 = .04166667

            IPT = ITRE(I)

            IF (IMETRIC.EQ.0) THEN
              DIAI = DBH(I) * .04166667
            ELSE
              DIAI = DBH(I) * INtoM * 0.5  ! radius in metres
            ENDIF

            DO IT = 1,NP
              NCKS=0
   40         CONTINUE
              CALL SVGTPT(IPLGEM,X1R1S(IPT),X2R2S(IPT),
     >          Y1A1S(IPT),Y2A2S(IPT),X,Y,IMETRIC)

C             IF THE OBJECT OVERLAPS, GET A NEW POINT.
C             (DON'T CHECK FOR OVERLAP WITH CWD OBJECTS)

              IF (NSVOBJ.EQ.0) GOTO 50
              IF (II.EQ.1) THEN
                IF(ITARGN(I).NE.0 .OR. NN.EQ.1) THEN
                  DO ISVOBJ = 1,NSVOBJ
                    IF (IOBJTP(ISVOBJ).EQ.4) CYCLE
                    IF (IS2F(ISVOBJ).GT.0) THEN
                      IF (ITRE(IS2F(ISVOBJ)).EQ.ITRE(I)) THEN

                        IF (IMETRIC.EQ.0) THEN
                          DIAOBJ = DBH(IS2F(ISVOBJ))*.04166667
                        ELSE
                          DIAOBJ = DBH(IS2F(ISVOBJ))*INtoM*0.5
                        ENDIF

                        CALL SVOBOL (1,X,Y,DIAI,0.,
     >                    IOBJTP(ISVOBJ),XSLOC(ISVOBJ),
     >                    YSLOC(ISVOBJ),DIAOBJ,0.,
     >                    XS,YS,KODE)

                        IF (KODE.EQ.1) THEN
                          NCKS = NCKS + 1
                          IF (NCKS.GT.40) GOTO 50
                          GOTO 40
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDDO
                ENDIF
              ENDIF
   50         CONTINUE

C             PLACE THE TREE AT X,Y

              NSVOBJ = NSVOBJ + 1
              IF (NSVOBJ.GT.MXSVOB) THEN
                WRITE (JOSTND,60)
   60           FORMAT (//'********   STAND VISUALIZATION ',
     >            'ERROR: TOO MANY OBJECTS')
                NSVOBJ = NSVOBJ -1
                CALL RCDSET (2,.TRUE.)
                RETURN
              ENDIF
              XSLOC(NSVOBJ) = X
              YSLOC(NSVOBJ) = Y
              IS2F(NSVOBJ) = I
              IOBJTP(NSVOBJ) = 1
              NTOADD(I) = NTOADD(I) - 1
            ENDDO
          ENDIF
        ENDDO
      ENDDO

C     USED FOR DEBUG PURPOSES

      IF (DEBUG) WRITE (JOSTND,*) 'NSVOBJ=',NSVOBJ
      IF (DEBUG .AND. NSVOBJ.GT.0) THEN
        X = 0
        DO I = 1,ITRN
          IF(NTOADD(I).GT.0) WRITE (JOSTND,*)
     >      'TREE =', I,',',NTOADD(I)
          X = NTOADD(I) + X
        ENDDO
        WRITE (JOSTND,*) 'LEFT NOT ADDED=', X
        TPPP = 0
        DO II = 1, MAXPLT
          PPP(II) = 0
          IPLCNT(II) = 0
        ENDDO
        DO II = 1,ITRN
          I = ITRE(II)

          IF (IMETRIC.EQ.0) THEN
            PPP(I) = PROB(II) + PPP(I)
          ELSE
            PPP(I) = (PROB(II)/ACRtoHA) + PPP(I)
          ENDIF

        ENDDO

        ICNT= 0
        DO ISVOBJ = 1,NSVOBJ
          II = IS2F(ISVOBJ)
          IF(II.GT.0) THEN
            IF (IOBJTP(ISVOBJ).EQ.1) THEN
              I = ITRE(II)
              IPLCNT(I) = IPLCNT(I) + 1
              ICNT = ICNT + 1
            ENDIF
          ENDIF
        ENDDO

        DO J=1,ITRN
          ITEMP=0
          DO ISVOBJ = 1,NSVOBJ
            II= IS2F(ISVOBJ)
            IF (II.EQ.J .AND. IOBJTP(ISVOBJ).EQ.1) ITEMP=ITEMP+1
          ENDDO

          IF (IMETRIC.EQ.0) THEN
            DIFF=PROB(J)-ITEMP
          ELSE
            DIFF=(PROB(J)/ACRtoHA)-ITEMP
          ENDIF

          DIFF=PROB(J)-ITEMP
          IF(DIFF.GT.1.OR.DIFF.LT.-1) THEN
            IF(DIFF.GT.2.OR.DIFF.LT.-2) THEN
              SBLANK=' ++'
            ELSE
              SBLANK=' **'
            ENDIF
          ELSE
             SBLANK=' '
          ENDIF
          IF(PROB(J).EQ.0) SBLANK=' '

          IF (IMETRIC.EQ.0) THEN
            XM = PROB(J)
          ELSE
            XM = PROB(J)/ACRtoHA
          ENDIF

          WRITE (JOSTND,*) '  J=',J,'PROB(',J,')=',XM,
     >      ' OBJECTL=',ITEMP,
     >      ' DIFF=',DIFF,SBLANK
        ENDDO

        TDIFF = 0
        DO II = 1, ISVINV
          DIFF = PPP(II) - IPLCNT(II)
          TDIFF = TDIFF + DIFF
          WRITE (JOSTND,*) 'PPP(',II,')=',PPP(II),
     >      'IPLCNT(',II,')=',IPLCNT(II),'DIFF=',DIFF
        ENDDO

        DO II=1, ISVINV
          TPPP = PPP(II)+TPPP
        ENDDO
        WRITE (JOSTND,*) 'TPPP=',TPPP,'ICNT=',ICNT,'TDIFF=',TDIFF
      ENDIF

  200 CONTINUE

C     IF CALLED FROM SVSTART (I1 EQ 0) THEN INITIALAIZE THE SNAG LIST
C     AND THE CWD OBJECT LIST.

      IF (I1.EQ.0) THEN
        NDEAD  = 0
        DO IX=1,MXDEAD
          ISTATUS(IX)=0
        ENDDO

        NCWD  = 0
        DO IX=1,MXCWD
          CWDDIA(IX)=0.0
        ENDDO
      ENDIF

C     PROCESS THE SNAGS THAT MAY BE IN THE TREE LIST

      IF (IREC2.LT.MAXTP1) THEN

C     INITIALIZATION OF VECTORS

        DO II = 1,MAXPLT
          PPP(II) = 0
          IPLCNT(II) = 0
        ENDDO
        DO I = 1,MAXTRE
          NOBTS(I) = 0
          NTOADD(I) = 0
          RMVSUM(I) = 0
          FRACAD(I) = 0
        ENDDO

        TPPP=0
        DO II = IREC2,MAXTRE
          I = ITRE(II)

          IF (IMETRIC.EQ.0) THEN
            PPP(I) = (PROB(II)*(FINTM/FINT)) + PPP(I)
          ELSE
            PPP(I) = (PROB(II)*(FINTM/FINT)/ACRtoHA) + PPP(I)
          ENDIF

        ENDDO

        DO II=1, ISVINV
          TPPP = PPP(II)+TPPP
        ENDDO

C       NOTE: NOBTS IS ALWAYS 0 HERE FOR THE GIVEN RECORD

        ITOTAL = 0
        DO I = IREC2,MAXTRE
          IF(I1.EQ.0) THEN

            IF (IMETRIC.EQ.0) THEN
             XM = PROB(I)*(FINTM/FINT)
            ELSE
             XM = PROB(I)*(FINTM/FINT)/ACRtoHA
            ENDIF

            IF(NOBTS(I) .LT. XM) THEN
             IF(NOBTS(I) .LE. XM - 1.) THEN
                NTOADD(I) = IFIX(XM-NOBTS(I))
             ENDIF
             FRACAD(I) = XM-NOBTS(I)-NTOADD(I)
            ELSE
             FRACAD(I) = 0
            ENDIF
          ENDIF
          ITOTAL = NTOADD(I) + ITOTAL + NOBTS(I)
        ENDDO

C       USING THE FRACTIONAL NUMBER OF TREES CALCULATE AND LOAD
C       THE NUMBER OF TREES TO ADD PER TREE RECORD PER PLOT

        DO IPLT = 1,ISVINV
          IF (DEBUG) WRITE (JOSTND,*) '--------------ADD IPLT=',
     >      IPLT,'----------------'

C         INITALIZE RMVSUM

          DO I = IREC2,MAXTRE
            RMVSUM(I) = 0
          ENDDO

C         FIND TOTAL NUMBER OF TREES PER THIS PLOT

          N = 0
          DO I = IREC2,MAXTRE
            IF(ITRE(I).EQ.IPLT) THEN
              N = N + 1
              NTRPLT(N) = I
            ENDIF
          ENDDO

C         IF THERE ARE NO TREES ON THIS PLOT GO TO NEXT PLOT

          IF (N.EQ.0) GOTO 220

C         SORT NTRPLT ON DBH

          CALL RDPSRT(N,DBH,NTRPLT,.FALSE.)

C         LOAD RMVSUM WITH FRACTIONAL NEW TREES IN ORDER BY NTRPLT

          IX = NTRPLT(1)
          RMVSUM(IX) = FRACAD(IX)
          IF (DEBUG) WRITE (JOSTND,*) 'RMVSUM(',IX,')=',RMVSUM(IX)
          IF(N.GT.1) THEN
            DO II = 2,N
              I = NTRPLT(II)
              RMVSUM(I) = FRACAD(I) + RMVSUM(IX)
              IX = I
            ENDDO
          ENDIF

C         CALCULATE THE NUMBER OF TREES ADDED TO THIS PLOT

          NTA = 0
          DO I = 1,N
            NTA = NTOADD(NTRPLT(I)) + NTA
          ENDDO

C         SETUP INFORMATION TO START DECIDING WHAT TREES SHOULD BE ADDED

          TOADD = PPP(IPLT) - FLOAT(IPLCNT(IPLT)) - NTA
          IF (TOADD.LE.0.0) GOTO 220
          ITOADD = IFIX(TOADD) + 1
          XINT = RMVSUM(NTRPLT(N)) / (TOADD)
          CALL SVRANN(X)
          START = XINT * X
          X = 0
          IADD = 0

C         LOOP TO FIND WHICH TREES SHOULD BE ADDED

          NTA = NTA + IPLCNT(IPLT)
          DO II = 1,N
            IF (ITOTAL.GE.TPPP) GOTO 230
            IF (NTA .GE. PPP(IPLT)) GOTO 220
            I = NTRPLT(II)
            IF(START.GT.X .AND. START.LT.RMVSUM(I)) THEN
              IF (I1 .GT. 0) THEN
                X = (I)
                START = START + XINT
                GOTO 215
              ENDIF
              START = START + XINT
              NTOADD(I) = NTOADD(I) + 1
              ITOTAL = ITOTAL + 1
              IADD = IADD + 1
              NTA = NTA + 1
              X = RMVSUM(I)
              IF (IADD.GE.ITOADD) GOTO 220
            ELSE
              X = RMVSUM(I)
            ENDIF
  215       CONTINUE
          ENDDO
  220     CONTINUE
        ENDDO
  230   CONTINUE

C       UPDATING AND CHECKING TO SEE WHERE CALL WAS MADE FROM

        NN = 2
        NSNAGS=0
        DO II = 1,NN
          DO I = IREC2,MAXTRE
            NP = NTOADD(I)
            IF (NP.GT.0) THEN

C             THE TREE WON THE TOSS...PLACE IT...NP TIMES
C             1/12 * .5 = .04166667

              IPT = ITRE(I)

              IF (IMETRIC.EQ.0) THEN
                DIAI = DBH(I) * .04166667
              ELSE
                DIAI = DBH(I) * INtoM * 0.5
              ENDIF

              DO IT = 1,NP
                NCKS=0
  240           CONTINUE
                CALL SVGTPT(IPLGEM,X1R1S(IPT),X2R2S(IPT),
     >            Y1A1S(IPT),Y2A2S(IPT),X,Y,IMETRIC)

C               IF THE OBJECT OVERLAPS, GET A NEW POINT.
C               (DON'T CHECK FOR OVERLAP WITH CWD OBJECTS)

                IF (NSVOBJ.EQ.0) GOTO 250
                IF (II.EQ.1) THEN
                  IF(NN.EQ.1) THEN
                    DO ISVOBJ = 1,NSVOBJ
                      IF (IOBJTP(ISVOBJ).EQ.4) CYCLE
                      IF (IS2F(ISVOBJ).GT.0) THEN
                        IF (ITRE(IS2F(ISVOBJ)).EQ.ITRE(I)) THEN

                          IF (IMETRIC.EQ.0) THEN
                            DIAOBJ = DBH(IS2F(ISVOBJ))*
     >                        .04166667
                          ELSE
                            DIAOBJ = DBH(IS2F(ISVOBJ))*
     >                        INtoM * 0.5
                          ENDIF

                          CALL SVOBOL (1,X,Y,DIAI,0.,
     >                      IOBJTP(ISVOBJ),XSLOC(ISVOBJ),
     >                      YSLOC(ISVOBJ),DIAOBJ,0.,
     >                      XS,YS,KODE)

                          IF (KODE.EQ.1) THEN
                            NCKS = NCKS + 1
                            IF (NCKS.GT.40) GOTO 250
                            GOTO 240
                          ENDIF
                        ENDIF
                      ENDIF
                    ENDDO
                  ENDIF
                ENDIF
  250           CONTINUE

C               PLACE THE TREE AT X,Y

                NSVOBJ = NSVOBJ + 1
                NSNAGS = NSNAGS + 1
                IF (NSVOBJ.GT.MXSVOB) THEN
                  WRITE (JOSTND,260)
  260             FORMAT (//'********   STAND VISUALIZATION ',
     >              'ERROR: TOO MANY OBJECTS')
                  NSVOBJ = NSVOBJ -1
                  CALL RCDSET (2,.TRUE.)
                  RETURN
                ENDIF
                XSLOC(NSVOBJ) = X
                YSLOC(NSVOBJ) = Y
                IS2F(NSVOBJ) = I
                ISNADD(NSNAGS) = NSVOBJ
                IOBJTP(NSVOBJ) = 0
                NTOADD(I) = NTOADD(I) - 1
              ENDDO
            ENDIF
          ENDDO
        ENDDO

        IYEAR=IY(1)
        CALL SVSNAD(IYEAR,ISNADD,ITOTAL,0)

      ENDIF

      RETURN
      END
