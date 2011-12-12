      SUBROUTINE SVRMOV (REMOVE,ISWTCH,SSNG,DSNG,CTCRWN,ICURYEAR)
      IMPLICIT NONE
C----------
C  **SVRMOV--BASE  DATE OF LAST REVISION: 11/15/10
C----------
C
C     STAND VISUALIZATION GENERATION
C     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998
C     J.J.MARCHINEK -- RMRS MOSCOW -- JANUARY 1999
C     A.H.DALLMANN  -- RMRS MOSCOW -- JANUARY 2000
C     D. ROBINSON   -- ESSA        -- MAY 2005
C
C     REMOVES TREES FOR EITHER SVMORT OR SVCUTS
C
C     UPDATES THE VISUALIZATION FOR HARVEST, WRITES THE POST
C     CUTTING REPORT, AND CLEANS UP THE LIST OF REFERENCES
C     TO "REMOVED" TREE RECORDS.
C
C     CALLED FROM SVCUTS AND SVMORT.
C     INPUT:
C     REMOVE  = VECTOR OF MORTALITY OR REMOVAL TREES/ACRE
C     ISWTCH = 1 IF SVMORT CALLED THIS SUBROUTINE, FIRE-CAUSED MORTALITY
C            = 2 IF SVMORT CALLED THIS SUBROUTINE, NORMAL MORTALITY
C            = 3 IF SVMORT CALLED THIS SUBROUTINE, WESTWIDE PINE BEETLE MORT
C            = 4 IF SVCUTS CALLED THIS SUBROUTINE
C----------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'

      INCLUDE 'ARRAYS.F77'

      INCLUDE 'CONTRL.F77'

      INCLUDE 'SVDATA.F77'

      INCLUDE 'PLOT.F77'

      INCLUDE 'SVDEAD.F77'

      INCLUDE 'METRIC.F77'

C
COMMONS
C
C     WPP     - SUM OF REMOVE PER PLOT
C     PPP     - SUM OF PROB PER PLOT
C     IPLCNT  - SUM OF OBJECT TREES PER PLOT
C     NOBTS   - SUM OF OBJECT TREES PER TREE RECORD
C     NTORMV  - SUM OF NUMBER TO REMOVE PER TREE RECORD
C     RMVSUM  - INCREASING SUM OF REMOVE
C     REMOVE  - TREES PER ACRE OF MORTALITY/CUT
C     NTRPLT  - A LIST OF INDICES OF THE TREES ON THE CURRENT PLOT
C     NTOADD  - SUM OF NUMBER TO ADD PER TREE RECORD
C     FRACAD  - FRACTIONAL TREES PER ACRE OF NEW TREES (ie <1)
C     SCORE   - A VECTOR BASED ON ITRE, AND DBH
C     ISNADD  - VECTOR OF SNAGS THAT NEED TO BE ADDED
C
      REAL WPP(MAXPLT)
      REAL PPP(MAXPLT)
      INTEGER IPLCNT(MAXPLT)

      INTEGER NOBTS(MAXTRE)
      INTEGER NTORMV(MAXTRE)
      REAL RMVSUM(MAXTRE)
      REAL REMOVE(MAXTRE)
      INTEGER NTRPLT(MAXTRE)
      INTEGER NTOADD(MAXTRE)
      REAL FRACAD(MAXTRE)
      REAL SCORE(MAXTRE)

      INTEGER ISNADD(MXSVOB)

      INTEGER ICURYEAR,ISWTCH,II,I,ITOTAL,ISVOBJ,IPLT,N,I1,NTA,ITOADD,
     >        IADD,K,IDEL,IBOT,ITOP,INUM,IPT,NCKS,IT,KODE,IX,ISI,J,
     >        ITEMP
      REAL    RMV,X,WRMV,TOADD,XINT,START,DIAI,Y,DIAOBJ,XS,YS,DIFF,XM

C     SSNG  = SNAGS/ACRE CREATED BY CUTTING, BUT LEFT STANDING
C     DSNG  = SNAGS/ACRE CREATED BY CUTTING, DOWN LEFT IN STAND
C     CTCRWN= TREES/ACRE REMOVED BUT WHOSE CROWNS ARE STILL IN THE
C             STAND.

      REAL SSNG(*),DSNG(*),CTCRWN(*)
      CHARACTER*3, SBLANK
      LOGICAL DEBUG

      CALL DBCHK (DEBUG,'SVRMOV',6,ICYC)

      IF (DEBUG) WRITE (JOSTND,*) 'IN SVRMOV: JSVOUT=',JSVOUT,
     >  ' ISWTCH=',ISWTCH

      IF (JSVOUT.EQ.0) RETURN

C     INITIALIZE THE PLOT-BASED VECTORS

      DO II=1, MAXPLT
        WPP(II) = 0
        PPP(II) = 0
        IPLCNT(II) = 0
      ENDDO

C     INITIALIZE THE TREE-RECORD BASED VECTORS

      DO II=1,ITRN
        SCORE(II) = FLOAT(ISP(II))*1000+DBH(II)
        I = ITRE(II)
        IF (IMETRIC.EQ.0) THEN
          WPP(I) = REMOVE(II) + WPP(I)
          PPP(I) = PROB(II) + PPP(I)
        ELSE
          WPP(I) = (REMOVE(II)/ACRtoHA) + WPP(I)
          PPP(I) = (PROB(II)/ACRtoHA) + PPP(I)
        ENDIF
      ENDDO

C     CLEAR OUT THE ISNADD VECTOR

      DO I = 1, MXSVOB
        ISNADD(I) = 0
      ENDDO

C     CLEAR OUT OTHER VECTORS

      DO I = 1,MAXTRE
        NTOADD(I) = 0
        FRACAD(I) = 0
        NOBTS(I) = 0
        NTORMV(I) = 0
        RMVSUM(I) = 0
      ENDDO

C     LOAD NUMBER OF OBJECTS PER TREE RECORD IN NOBTS
C     LOOPS THROUGH THE TOTAL NUMBER OF DRAWN OBJECTS (NSVOBJ) AND ADDS
C     ONE TO THE NOBTS ARRAY

      ITOTAL = 0
      DO ISVOBJ = 1,NSVOBJ
        IF (IOBJTP(ISVOBJ).EQ.1 .AND. IS2F(ISVOBJ).GT.0) THEN

C           THIS IS A FVS TREE OBJECT WITH A CORRESPONDING TREE RECORD

          NOBTS(IS2F(ISVOBJ)) = NOBTS(IS2F(ISVOBJ)) + 1
          ITOTAL = ITOTAL + 1
        ENDIF
      ENDDO

C     ADD INTEGER VALUE TREE RECORDS

      DO I = 1,ITRN
        IF (ISWTCH.EQ.4) THEN
          RMV=0.
        ELSE
          RMV = REMOVE(I)
        ENDIF

        IF (IMETRIC.EQ.0) THEN
          XM = PROB(I)-RMV
        ELSE
          XM = (PROB(I) - RMV)/ACRtoHA
        ENDIF

        IF(XM .GE. 0) THEN

C         THERE ARE A SUFFICENT NUMBER OF TREES IN PROB TO HANDLE THE
C         DESIRED REMOVAL.

          IF(XM+1. .GT. 0.) THEN

C           THERE IS AT LEAST ONE FULL TREE TO ADD TO NTOADD(I)

            NTOADD(I) = IFIX(XM)
          ENDIF

C         ADD THE FRACTIONAL PORTION LEFT OVER TO FRACAD(I)

          FRACAD(I) = XM - FLOAT(NTOADD(I))
          IF (FRACAD(I) .LT. 0.00001) FRACAD(I)=0.0
        ELSE
          FRACAD(I) = 0.
        ENDIF
   10   CONTINUE
      ENDDO

C     DEBUG ROUTINE TO SEE HOW MANY TREES WE NOW HAVE.

      IF (DEBUG) THEN
        X=0
        DO I=1,ITRN
          X=NTOADD(I)+X
        ENDDO
        WRITE (JOSTND,*) 'NONFRACTIONAL TOTAL TO HAVE=', X
      ENDIF

C     USING THE FRACTIONAL NUMBER OF TREES CALCULATE AND LOAD
C     THE NUMBER OF TREES TO ADD PER TREE RECORD PER PLOT

      DO IPLT = 1,ISVINV
        IF (DEBUG) WRITE (JOSTND,*) '-------ADD IPLT=',IPLT

C       INITALIZE RMVSUM

        DO I = 1,ITRN
          RMVSUM(I) = 0
        ENDDO

C       FIND THE TREES ON THIS PLOT "IPLT" AND STORES A LINK NTRPLT(N) TO
C       THE CORRESPONDING TREE IN THE OVERALL TREE-RECORDS LIST "ITRE".
C       N= THE NUMBER OF TREES ON THIS PLOT

        N = 0
        DO I=1,ITRN
          IF(ITRE(I).EQ.IPLT) THEN
            N=N+1
            NTRPLT(N)=I
          ENDIF
        ENDDO

C       IF THERE ARE NO TREES ON THIS PLOT GO TO NEXT PLOT

        IF (N.EQ.0) GOTO 25

C       SORT NTRPLT ON DESCENDING SCORE (WHERE SCORE IS BASED ON AVERAGE
C       TREE DIAMETER)

        CALL RDPSRT(N,SCORE,NTRPLT,.FALSE.)

C       LOAD RMVSUM WITH FRACTIONAL NEW TREES IN ORDER BY NTRPLT

        I1=NTRPLT(1)
        RMVSUM(I1)=FRACAD(I1)
        IF (DEBUG) WRITE (JOSTND,*) 'RMVSUM(',I1,')=',RMVSUM(I1)
        IF(N.GT.1) THEN
          DO II=2,N
            I=NTRPLT(II)
            RMVSUM(I)=FRACAD(I)+RMVSUM(I1)
            I1=I
            IF (DEBUG) WRITE (JOSTND,*) 'RMVSUM(',I,')=',RMVSUM(I)
          ENDDO
        ENDIF

C       IF THERE IS NO ACCUMULATED FRACTIONAL REMOVAL, GO TO NEXT PLOT.

        IF (RMVSUM(NTRPLT(N)).EQ.0) GOTO 25

C       CALCULATE THE NUMBER OF TREES ADDED FROM THIS PLOT

        NTA = 0
        DO I=1,N
          NTA = NTOADD(NTRPLT(I)) +NTA
        ENDDO

C       SETUP INFORMATION TO START DECIDING WHAT TREES SHOULD BE ADDED

        IF (ISWTCH.EQ.4) THEN
          WRMV = 0
        ELSE
          WRMV = WPP(IPLT)
        ENDIF
        TOADD=PPP(IPLT) - WRMV  - FLOAT(NTA)
        IF (DEBUG) WRITE (JOSTND,*) 'N=',N,'TOADD=',TOADD,
     >    'PPP(IPLT)=',PPP(IPLT),'NTA=', NTA
        IF (TOADD.LT. .001) GOTO 25       ! TOO FEW TO BOTHER WITH.
        ITOADD=IFIX(TOADD+.5)
        XINT= RMVSUM(NTRPLT(N))/TOADD
        CALL SVRANN(X)
        START = XINT * X

        IF (DEBUG) WRITE (JOSTND,*) 'ITOADD=',
     >    ITOADD,' XINT=',XINT,' START=',START
        X = 0
        IADD = 0

C       LOOP TO FIND WHICH TREES SHOULD BE ADDED

        DO II=1,N
          I=NTRPLT(II)
          IF (DEBUG) WRITE(JOSTND,*) 'START=',START,
     >      ' RMVSUM(',I,')=', RMVSUM(I),' X=',X
          IF(START.GT.X .AND. START.LT.RMVSUM(I)) THEN
            START = START+XINT
            NTOADD(I)= NTOADD(I)+1
            IADD = IADD + 1
            X = RMVSUM(I)
            IF (DEBUG) WRITE (JOSTND,*) 'IADD=',IADD,
     >        ' NTOADD(', I,')=',
     >        NTOADD(I), ' FRACAD(I)=',FRACAD(I)
            IF (IADD.GE.ITOADD) GOTO 25
          ELSE
            X=RMVSUM(I)
          ENDIF
   15     CONTINUE
        ENDDO
   25 CONTINUE
      ENDDO
   35 CONTINUE

C     FINDING OUT HOW MUCH NTORMV NEEDS TO BE

      DO I = 1,ITRN
        NTORMV(I) = NOBTS(I) - NTOADD(I)
      ENDDO

C     USED FOR DEBUG PURPOSES

      IF (DEBUG) THEN
        X=0
        DO I=1,ITRN
         X=NTOADD(I)+X
         IF (NTOADD(I) .GT. 0) THEN
           WRITE (JOSTND,*) 'NTOADD(',I,')=', NTOADD(I)
         ENDIF
        ENDDO
        WRITE (JOSTND,*) 'TOTAL NEEDED=', X
        DO I=1, ITRN
          IF(NTORMV(I).LT.-1) THEN
            IF(NTORMV(I).LT.-2) THEN
              SBLANK=' ++'
            ELSE
              SBLANK=' **'
            ENDIF
          ELSE
            SBLANK= ' '
          ENDIF
          WRITE (JOSTND,*) 'I=',I,' NOBTS(I)=', NOBTS(I),
     >      ' NTOADD(I)=',NTOADD(I), ' NTORMV(I)=', NTORMV(I),
     >      ' DBH(I)=', DBH(I), SBLANK
        ENDDO
      ENDIF

C     END OF DEBUG

C     IF NTORMV IS NEGATIVE, THEN THERE IS A DIFFICIENT FOR THAT
C     TREE RECORD (IE, TOO MANY ALREADY TAKEN OUT).  THIS FINDS
C     ANOTHER SUITABLE TREE FOR THE DIFFICIENT TREE RECORD.

C     RERUN THE SORT...TRY TO KEEP TREES REPRESENTING LIKE TREES.

      IF (ITRN .GT. 0) CALL RDPSRT(ITRN,SCORE,NTRPLT,.TRUE.)

      DO 100 K=1,ITRN
        I=NTRPLT(K)
        IF (NTORMV(I).LT.0) THEN
          IDEL=-1
          IBOT=0
          ITOP=0
   40     CONTINUE

c            IF (DEBUG) WRITE (JOSTND,*) 'I=',I,' ITOP=',ITOP,
c     >                 ' IBOT=',IBOT,' IDEL=',IDEL

          IF (ITOP+IBOT.EQ.2) GOTO 50
          IF (IDEL.LT.0) THEN

C              LOOK AT THE TREES "ABOVE" THIS TREE, IF ANY

            IDEL = -IDEL
            IF (IDEL+K.GT.ITRN) ITOP=1
            IF (ITOP.EQ.1) GOTO 40
          ELSE

C              LOOK AT THE TREE "BELOW" THIS TREE, IF ANY.

            IDEL = -(IDEL+1)
            IF (IDEL+K.LT.1) IBOT=1
            IF (IBOT.EQ.1) GOTO 40
          ENDIF

C           SEARCH FOR A TREE THAT REPRESENT THIS TREE FOLLOWING THE
C           SORTED ORDER.  II NOW REFERENCES A CANIDATE TREE.

            II = NTRPLT(K+IDEL)

C            II = I+IDEL
c            IF (DEBUG) WRITE (JOSTND,*) 'I=',I,' IDEL=',IDEL,
c     >                 ' II=',II

C           RMV IS THE NUMBER OF TREES TO SUBTRACT FROM PROB...
C           NEEDED HERE TO INSURE THAT THE CANIDATE TREE HAS A
C           SURPLUS OF OBJECTS...

          IF (ISWTCH.EQ.4) THEN
            RMV = 0.
          ELSE
            RMV = REMOVE(II)
          ENDIF

C         CAN'T USE THE CANIDATE UNLESS IT HAS A SURPLUS OF OBJECTS

          IF (NTORMV(II).LT.1) GOTO 40

C         IT MUST BE THE THE SAME SPECIES

          IF (ISP(II) .NE. ISP(I)) GOTO 40

C         HAVE A DIAMETER WITHIN 25%

          IF (MIN(DBH(II),DBH(I)).LT.
     >      MAX(DBH(II),DBH(I))*.75) GOTO 40

          IF (DEBUG) WRITE (JOSTND,45)
     >      II,DBH(II),ISP(II),I,DBH(I),ISP(I)
   45     FORMAT (' ASSIGN TREE',I4,' DBH=',F7.2,' ISP=',
     >      I3,' TO REPRESENT TREE',I4,' DBH=',F7.2,' ISP=',I3)

C         OK, WE CAN USE THE TREE.

          NTORMV(II)=NTORMV(II)-1
          NTORMV(I )=NTORMV(I) +1

C         IF WE STILL HAVE A DEFICIT, KEEP WORKING!

          IF (NTORMV(I).LT.0) GOTO 40
          GOTO 100

   50     CONTINUE
          IF (DEBUG) WRITE (JOSTND,*) 'ADD TREE I=',I,' DBH=',DBH(I),
     >      ' ISP(I)=',ISP(I),' PROB=',PROB(I)

C         IF NTORMV(I) IS STILL LESS THAN 0, AND WE ARE OUT
C         OF TREE (COULD NOT FIND A CANIDATE), THEN ADD THE TREE.

          IF (NTORMV(I).LT.0 .AND. ISWTCH.EQ.4) THEN
            INUM= -NTORMV(I)
            NTORMV(I) = 0
            IPT=ITRE(I)

            IF (IMETRIC.EQ.0) THEN
              DIAI = DBH(I) * .04166667
            ELSE
              DIAI = DBH(I) * INtoM * 0.5
            ENDIF

            NCKS = 0
            DO IT = 1,INUM
              NCKS=0
   60         CONTINUE
              CALL SVGTPT(IPLGEM,X1R1S(IPT),X2R2S(IPT),
     >          Y1A1S(IPT),Y2A2S(IPT),X,Y,IMETRIC)

C             IF THE OBJECT OVERLAPS, GET A NEW POINT.
C             (DON'T CHECK FOR OVERLAP WITH CWD OBJECTS)

              IF (NSVOBJ.EQ.0) GOTO 70
              DO ISVOBJ=1,NSVOBJ
                IF (IOBJTP(ISVOBJ).EQ.4) CYCLE
                IF (IS2F(ISVOBJ).GT.0) THEN
                  IF (ITRE(IS2F(ISVOBJ)).EQ.ITRE(I)) THEN

                    IF (IMETRIC.EQ.0) THEN
                      DIAOBJ = DBH(IS2F(ISVOBJ)) * .04166667
                    ELSE
                      DIAOBJ = DBH(IS2F(ISVOBJ)) * INtoM * 0.5
                    ENDIF

                    CALL SVOBOL (1,X,Y,DIAI,0.,
     >                IOBJTP(ISVOBJ),XSLOC(ISVOBJ),
     >                YSLOC(ISVOBJ),DIAOBJ,0.,
     >                XS,YS,KODE)

                    IF (KODE.EQ.1) THEN
                      NCKS=NCKS+1
                      IF (NCKS.GT.40) GOTO 70
                      GOTO 60
                    ENDIF
                  ENDIF
                ENDIF
              ENDDO
   70         CONTINUE

C             PLACE THE TREE AT X,Y

              NSVOBJ=NSVOBJ+1
              IF (NSVOBJ.GT.MXSVOB) THEN
                WRITE (JOSTND,72)
   72           FORMAT (//'********   STAND VISUALIZATION ',
     >            'ERROR: TOO MANY OBJECTS')
                NSVOBJ = NSVOBJ -1
                CALL RCDSET (2,.TRUE.)
              ENDIF
              XSLOC(NSVOBJ)=X
              YSLOC(NSVOBJ)=Y
              IS2F(NSVOBJ) =I
              IOBJTP(NSVOBJ)=1
            ENDDO
          ENDIF
        ENDIF
  100 CONTINUE

C     LOAD SCORE NOW WITH A RANDOM NUMBER IF THERE IS SOMETHING FOR
C     THAT TREE TO BE REMOVED.

      DO II=1,ITRN
        IF (NTORMV(II) .GT. 0) THEN
          CALL SVRANN(X)
          SCORE(II)=X
        ELSE
          SCORE(II)=0.
        ENDIF
      ENDDO

C     NOW REMOVE THE TREES THAT NEED TO BE REMOVED.

      IX=0
      DO ISVOBJ=1,NSVOBJ
        IF ( IOBJTP(ISVOBJ) .EQ. 4 ) CYCLE
        ISI = IS2F(ISVOBJ)
        IF(ISI.GT.0 )THEN
          IF (NTORMV(ISI).GT.0) THEN
              IF (DEBUG) WRITE (JOSTND,*) 'REMOVE(',ISI,')=',
     >            REMOVE(ISI),' DBH=',DBH(ISI),' ISP=',ISP(ISI),
     >           'NTORMV(',ISI,')=', NTORMV(ISI),
     >           'TOTAL REMOVED=', IX
              IF(IOBJTP(ISVOBJ).EQ.1 .AND. NTORMV(ISI).GT.0) THEN
                IF (ISWTCH .EQ. 4) THEN
                  IF (DEBUG) WRITE (JOSTND,*) 'SNAG ISI=',ISI,
     >               'SSNG=', SSNG(ISI),'DSNG=', DSNG(ISI),
     >               'CTCRWN=', CTCRWN(ISI)
                  IF (SCORE(ISI) .LE. CTCRWN(ISI)-DSNG(ISI)) THEN
                    IOBJTP(ISVOBJ) = 3
                    NTORMV(ISI) = NTORMV(ISI) - 1
                  ELSEIF (SCORE(ISI) .LE. CTCRWN(ISI)) THEN
                    IOBJTP(ISVOBJ) = -2
                    NTORMV(ISI) = NTORMV(ISI) - 1
                    IX=IX+1
                    ISNADD(IX)=ISVOBJ
                  ELSEIF (SCORE(ISI) .LE. SSNG(ISI)+CTCRWN(ISI)) THEN
                    IOBJTP(ISVOBJ) = -1
                    NTORMV(ISI) = NTORMV(ISI) - 1
                    IX=IX+1
                    ISNADD(IX)=ISVOBJ
                  ELSE
                    NTORMV(ISI) = NTORMV(ISI) - 1
                    IOBJTP(ISVOBJ) = 3
                  ENDIF
                  SCORE(ISI)=SCORE(ISI)+1
                ELSE
                  IOBJTP(ISVOBJ) = 0
                  NTORMV(ISI) = NTORMV(ISI) - 1
                  IX=IX+1
                  ISNADD(IX)=ISVOBJ
                ENDIF
              ENDIF
          ENDIF
        ENDIF
      ENDDO

      IF (IX.GT.0) THEN
        CALL SVSNAD(ICURYEAR,ISNADD,IX,ISWTCH)
      ENDIF

C     DEBUG TO FIND HOW MANY TREES NEED REMOVED, AND HOW MANY
C     WERE NOT REMOVED.

      IF (DEBUG) THEN
        K=0
        DO I=1,ITRN
          K=NTORMV(I)+K
        ENDDO
        WRITE (JOSTND,*) 'TOTAL REMOVED=',IX,' TOTAL LEFT=',K,
     >    ' NEG NUM=TOO FEW OBJECTS, POS=TOO MANY OBJS'
      ENDIF

C     IF DEBUG...

      IF (DEBUG) THEN
        DO J=1,ITRN
          ITEMP=0
          DO ISVOBJ = 1,NSVOBJ
            II= IS2F(ISVOBJ)
            IF (II.EQ.J .AND. IOBJTP(ISVOBJ).EQ.1) ITEMP=ITEMP+1
          ENDDO
          IF (ISWTCH.EQ.2) THEN
            RMV=REMOVE(J)
          ELSE
            RMV=0
          ENDIF
          DIFF=(PROB(J)-RMV)-ITEMP
          SBLANK=' '
          IF(ABS(DIFF).GT.1) THEN
            IF(ABS(DIFF).GT.2) THEN
              SBLANK=' ++'
            ELSE
              SBLANK=' **'
            ENDIF
          ENDIF
          WRITE (JOSTND,105) J,PROB(J),DBH(J),RMV,ITEMP,DIFF,SBLANK
  105     FORMAT (' J=',I5,' PROB=',F8.4,' DBH=',F7.2,
     >        ' REMOVE=',F8.4,' OBJECTL=',I3,' DIFF=',F8.3,A)
        ENDDO
        K=0
        IF (ISWTCH.EQ.2) K=1
        CALL SVCDBH(REMOVE,K)
      ENDIF

      RETURN
      END
