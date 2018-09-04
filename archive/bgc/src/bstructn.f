      SUBROUTINE BSTRUCTN (FVS_CYC)
C  Amended 6/30/03.  adding call to new subroutine.  THe new subroutine--
C  "REDOLAI" recalculates the distribution of Leaf area and leaf biomass.
C  Heretofore, they were solely a function of bole volume, no crown info was
C  used.  New routine prorates leaf area and biomass as by crown volume.
C
      INCLUDE 'ENTITY.F77'
      INCLUDE 'SITE.F77'

      INTEGER FVS_CYC

      CALL CROWNCF
      CALL CROWNHT
      CALL GEOMETRY
      IF(S(7).EQ.0.) THEN  !individual tree crown zones
         CALL ZONES
C--------------------------
C  CALL THE IDSORT SUBROUTINE
C  SORTS AN INDEX OF ZONE BOUNDARIES, DEFINED IN SUBROUTINE 'ZONE', IN 
C  ASCENDING ORDER. IF I1=INDEX(L) THEN ZONE(I1) IS THE HEIGHT FROM GROUND TO
C  ZONE BOUNDARY.--NOTE: THIS WAS MOVED 12/93, DWC.
C--------------------------
         CALL IDSORT(NB*2,ZONE,INDEX,.TRUE.)
      ELSE
         CALL STRATA
         CALL ZONES2
         CALL IDSORT(NUM2,ZONE,INDEX,.TRUE.)
      ENDIF
C
C***************************************************************************
C  New subroutine: recalculate leaf area and biomass distribution among
C  entities ONLY in year 1, cycle 1.  added 6/30/03 ajm.
C  !!NOTE!! I am temporarily "co-opting" BETA.dat parameter B2(9)--currently
C  it not used--as a "flag" variable, so that one can "turn on" or "turn off"
C  this logic via the BETA.dat file.  Set B2(9) = to 1 to invoke this logic.
C Another change 7/23 added third way to do this, flag(via B2(9)) with "-3"
C
      IF(FVS_CYC.EQ.1.AND.YRFLAG.EQ.1.AND.
     &  (B2(9).EQ.1.0 .OR. B2(9) .EQ. -1.0 .OR. B2(9) .EQ.-3))
     &  CALL REDOLAI
C
C***************************************************************************
C
      CALL LAIJ
      RETURN
      END

      

      SUBROUTINE CROWNCF

C-------------------------------------------------------------------
C   THIS SUBROUTINE CALCULATES THE CROWN COMPETITION FACTOR
C   FOR THE STAND BASED ON PROGNOSIS MODEL (see p.49-51 in manual).
C!!!NOTE: CCF IS IN ENGLISH UNITS FOR PROGNOSIS.
C      -- DWC, 1/24/94.
C!!! CCF FOR SMALL TREES (<1.3m) USES BASAL DIAMETER IN CALCULATION
C!!! modified 3/2/96 KSM
C-------------------------------------------------------------------

      INTEGER L
      REAL A1(11), A2(11), A3(11)
      REAL C1(11), C2(11)
      INCLUDE 'ENTITY.F77'

C------------------
C LOAD COEFFICIENTS FOR CCF EQUATIONS FOR FOLLOWING SPECIES
C                 PP      DF      WL      ES      LP      AF
C                 GF      BP      WP      RC      WH      
      DATA A1 /.03, .11, .02, .03, .02, .03,
     +         .04, .03, .03, .03, .03/
      DATA A2 /.0180, .0333, .0148, .0173, .0168, .0216,
     +         .0270, .0167, .0167, .0238, .0215/
      DATA A3 /.00281, .00259, .00338, .00259, .00325, .00405,
     +         .00405, .00230, .00230, .00490, .00363/
      DATA C1 /.00781, .01730, .00724, .00788, .00919, .01140,
     +         .01525, .00988, .00988, .00892, .01111/
      DATA C2 /1.7680, 1.5571, 1.8182, 1.7360, 1.7600, 1.7560,
     +         1.7333, 1.6667, 1.6667, 1.7800, 1.7250/
C------------------

      DO 10 I=1,NB
         L=BGCISP(I)
         IF(ID(I).EQ.'T') THEN
           IF(D(I).GE.25.4) THEN       !if dbh >= 10 inches
             CCF(I)=(EXPAND(I)/2.47) * (A1(L) + A2(L) * (D(I)/2.54) +
     +              A3(L) * (D(I)/2.54)**2.)
           ELSE IF(D(I).LT.25.4.AND.D(I).GT.0.0) THEN  !if 0<dbh<10 in.
             CCF(I)=(EXPAND(I)/2.47) * C1(L) * (D(I)/2.54)**C2(L)
C for small trees use BD(I) which is in cm.
           ELSE IF(D(I).EQ.0.0) THEN  !if dbh=0.0 in.
             CCF(I)=(EXPAND(I)/2.47) * C1(L) * (BD(I)/2.54)**C2(L)
           ENDIF
           SUMCCF=SUMCCF + CCF(I)
C          WRITE(*,*) I, BD(I), D(I), CCF(I), SUMCCF
         ENDIF
   10 CONTINUE
      RETURN
      END
      

      
      SUBROUTINE CROWNHT
C-------------------------
C  CALCULATES THE CROWN TOP AND CROWN BOTTOM BY ENTITY.
C-------------------------
      INCLUDE 'ENTITY.F77'

      MAXCRHT=0.
      MINCRHT=999.
      DO 10 I=1,NB
         IF (ID(I).EQ.'T') THEN
            CTOP(I)=H(I)
            CBOT(I)=H(I) - ( H(I)*CR(I) )
         ELSE IF (ID(I).EQ.'S') THEN
            CTOP(I)=H(I)
            CBOT(I)=0.
         ELSE IF (ID(I).EQ.'G') THEN
            CTOP(I)=H(I)
            CBOT(I)=0.0
         ENDIF
         MAXCRHT=MAX(MAXCRHT,CTOP(I))
         MINCRHT=MIN(MINCRHT,CBOT(I))
C          WRITE(72,*) 'I= ',I,' ID= ',ID(I),' CR= ',CR(I),
C     + ' CROWN TOP= ',CTOP(I),'  CROWN BOT= ',CBOT(I),' HT =',H(I)
   10 CONTINUE
      RETURN
      END
      

      SUBROUTINE GEOMETRY
C------------------
C     FOR INDIVIDUAL CONIFER ENTITIES ASSUME RIGHT CIRCULAR CONICAL CROWNS
C     WITH MAXIMUM WIDTH AT BASE OF CROWN. FOR SHRUB AND GRASS ENTITIES
C     ASSUME A CYLINDER. RETURN CROWN WIDTH AND BETA. BETA IS ANGLE AT
C     VERTEX OF CONE FOR CONIFERS; RADIUS OF CIRCLE OF 1ha AREA FOR GRASS
C     AND SHRUBS.  SUBROUTINE CALLED AS EACH TREE IS READ IN, OR AS TREES
C     ARE BEING UPDATED.
C------------------
       
      REAL BT1(11), BT3(11), ST1(11), CL
      INCLUDE 'ENTITY.F77'

C------------------
C LOAD COEFFICIENTS FOR CROWN WIDTH EQUATIONS FOR FOLLOWING SPECIES
C                 PP      DF      WL      ES      LP      AF
C                 GF      BP      WP      RC      WH      
      DATA BT1 /1.62635,3.02271,2.31359,3.76535,1.06804,1.74558,
     +         2.20611,-0.91984,4.308,2.79784,1.32772/
      DATA BT3 /-0.68098,-1.00486,-0.80919,-1.18257,-0.55987,-0.73972
     +         ,-0.76936,-0.07299,-1.37265,-0.89666,-0.52554/
      DATA ST1 /0.3638,0.32874,0.23846,0.33089,0.26342,0.33722,
     +         0.38503,0.07049,0.37031,0.46452,0.25622/
C------------------      
C DEFINE CROWN WIDTH FUNCTIONS FOR LARGE AND SMALL TREES.
C MELINDA MOEUR. 1981. 
C------------------
C XCWBT IS FOR LARGE TREES
      XCWBT(X,Y,Z,K)=EXP( BT1(K) + 1.08137*LOG(X/2.54) +
     +               BT3(K)*LOG(Y*3.28084) + 
     +               0.29786*LOG(Z*3.28084) )
C------------------
C XCWST IS FOR SMALL TREES
      XCWST(X,Y,Z,K)=EXP( ST1(K)*LOG(X*3.28084) +
     +               0.28283*LOG(Y*3.28084) +
     +               0.04032*LOG(Z*10.76391) )
C------------------
C     calculate canopy thickness (crown length) for all entities
      DO 10 I=1,NB
         CL=CTOP(I) - CBOT(I)
         IF(ID(I).EQ.'T') THEN
C     do geometry for trees      
            IF(D(I).GT.8.89) THEN
C           calculate crown width for Big Trees
               CW(I)=XCWBT(D(I), CTOP(I), CL, BGCISP(I)) / 3.28084
            ELSE
C           calculate crown width for Small Trees
               CW(I)=XCWST(CTOP(I), CL, TOTBA, BGCISP(I)) / 3.28084
C              WRITE(*,*) 'SMALL TREE CW ',I,CL,TPH(I),CW(I)
            ENDIF
C=================================================================
C Adjust CW to reflect perceived bias in Melinda's equations.
C
C         CW(I)=2.0*CW(I)
C=================================================================
C        calculate the slope of the line defining the vertex angle
C        of a cone for trees    
            BETA(I)=( 0.5*CW(I) ) / CL 
C           BETA(I)=.5
         ELSE
C        do geometry for grass and shrub entities
C        calculate the radius of circle with area=1ha-bare ground
            BETA(I)=( (AREA-BARE) / 3.14159 )**0.5
         ENDIF
   10 CONTINUE
      RETURN
      END
      
      
      SUBROUTINE ZONES
C-------------------------
C  CALCULATES ZONE BOUNDARY. A ZONE BOUNDARY IS
C  LOCATED AT EACH ENTITIES CANOPY TOP AND BOTTOM.
C-------------------------
      INCLUDE 'ENTITY.F77'

      DO 10 I=1,NB
         ZONE(I)=CTOP(I)
         ZONE(I+NB)=CBOT(I)
   10 CONTINUE
      NUM=2*NB-1
      NUM2=2*NB
      RETURN
      END


      SUBROUTINE LAIJ
C--------------------------
C     LAIJ RETURNS LEAF AREA INDEX FOR EACH ENTITY BY CANOPY ZONE
C--------------------------
 
      INTEGER MORE, J, I1, I2
      REAL A, B, RAT, TOTVOL, FRUSTVOL, TIP, BOT
      INCLUDE 'ENTITY.F77'

C-------------------------
C  XCONE IS GENERAL VOLUME FUNCTION FOR A CONE OR FRUSTUM OF A CONE
C  THAT USES THE ANGLE, BETA, OF THE CONE VERTEX AND TWO 
C  DISTANCES, A & B, FROM THE CONE TIP.
C-------------------------
C  coefficient on X was corrected from 3 to 2 3/16/96. KSM.
      XCONE(X,Y,Z)=((3.1415*X**2.)/3.)*(Z**3.-Y**3.)
C-------------------------
C  XCYL IS A FUNCTION FOR VOLUME OF A CYLINDER OR FRUSTUM THEREOF.
C  USES THE RADIUS, BETA, OF THE CYLINDER AND TWO DISTANCES, A & B,
C  FROM THE TOP OF THE CONE.
C-------------------------
      XCYL(X,Y,Z)=(3.1415*X**2.)*(Z-Y)
C-------------------------
C  XBASE CALCULATES THE AREA OF A CROSS SECTION OF A RIGHT CIRCULAR CONE
C  FROM THE VERTEX ANGLE, BETA, AND THE DISTANCE FROM THE CONE TIP
C-------------------------
      XBASE(X,Y)=(((Y*X)*1.)**2.)*3.1415
C--------------------------
C     CALCULATE, FOR EACH ENTITY, THE DISTANCE FROM TREE TIP TO ZONE BOUNDARY
      DO 10 I=1,NB
         MORE=1
         !calculate total crown volume
         IF(ID(I).EQ.'T') THEN
            TIP=0.0
            BOT=CTOP(I) - CBOT(I)
            TOTVOL=XCONE(BETA(I),TIP,BOT)
            FOLDENS(I)=LEAF(I)/TOTVOL   ! used in CROWN.FOR
C           WRITE (*,*)I, FOLDENS(I)
         ELSE
            TIP=0.0
            BOT=CTOP(I) - CBOT(I)
            TOTVOL=XCYL(BETA(I),TIP,BOT)
         ENDIF
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C ADDING NEW WRITE-FILE HERE.  ANALYZE TREE GEOMETRY.
         IF(ID(I).EQ.'T') THEN
         WRITE(77,15)Z1(1)+1,I,TREENO(I),CTOP(I),CBOT(I),
     &               CTOP(I)-CBOT(I),CW(I),BETA(I),TOTVOL,FOLDENS(I),
     &               LEAF(I),LA(I),LA(I)/(((CW(I)/2.)**2)*3.14159)
   15    FORMAT(F4.0,1X,I4,1X,I6,1X,4(F5.1,1X),F5.2,1X,5(F9.2,1X))
         ENDIF
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         DO 20 L=NUM,1,-1
            I1=INDEX(L)
            I2=INDEX(L+1)
            J=I1
            IF(CTOP(I).LE.ZONE(I1)) GOTO 20
            IF(CTOP(I).LT.ZONE(I2)) THEN
               A=0.0
            ELSE
               A=CTOP(I)-ZONE(I2)
            ENDIF
            IF(ZONE(I1).LE.CBOT(I)) THEN
               B=CTOP(I)-CBOT(I)
               MORE=0
            ELSE
               B=CTOP(I)-ZONE(I1)
            ENDIF
C                 WRITE(*,*) I,J,A,B,ZONE(I1)
            IF(ID(I).EQ.'T') THEN
               FRUSTVOL=XCONE(BETA(I),A,B)
               BASE(I,J)=XBASE(BETA(I),B)
            ELSE
               FRUSTVOL=XCYL(BETA(I),A,B)
               BASE(I,J)=(AREA - BARE)
            ENDIF
            RAT=FRUSTVOL/TOTVOL
            LLA(I,J)=RAT*LA(I)
            LAI(I,J)=LLA(I,J)/BASE(I,J)
C**************************************************************************
C Commenting out writing of file "TEST" for release version.  ajm 11/02
c	   WRITE(72,*) 'I= ',I,' TREELA= ',LA(I),' FRUSTVOL= ',FRUSTVOL,
c     +    ' BASE= ',BASE(I,J),' TOTVOL= ',TOTVOL
C**************************************************************************
C sum cover and LA by zone. EXPAND = TPH for trees, else = 1
            ZCOVER(J)=ZCOVER(J) + BASE(I,J) * EXPAND(I)
            ZONELA(J)=ZONELA(J)+LLA(I,J)*EXPAND(I)
C               WRITE(*,*) I, J, BETA(I), LLA(I,J), ZONE(I1)
            IF(MORE.EQ.0) GOTO 10 !RETURN
   20    CONTINUE
   10 CONTINUE
C write out cover by zone to see where crown closure happens
C      DO 30 L=NUM,1,-1
C          I1=INDEX(L)
C          I2=INDEX(L+1)
C          J=I1
C          CLOSURE=ZCOVER(J)/10000.
C          WRITE(*,*) J, ZONE(J), ZCOVER(J), CLOSURE, ZONELA(J)
C   30    CONTINUE
C      PAUSE
      RETURN
      END

      
      SUBROUTINE IDSORT (N,A,INDEX,LSEQ)
C----------
C  **IDSORT DATE OF LAST REVISION:  10:35:00 02/10/87
C----------
C
C  CHARACTER*4 INDEX SORT.
C
C  IDSORT USES THE VECTOR INDEX TO INDIRECTLY ADDRESS THE ARRAY A.
C  THE FIRST N ELEMENTS OF THE ARRAY INDEX REPRESENT INDICES OF
C  ELEMENTS OF ARRAY A TO BE SORTED OVER.  THE FIRST N ELEMENTS OF THE
C  INDEX ARRAY ARE REARRANGED SUCH THAT FOR EACH I FROM 1 TO N-1,
C  A(INDEX(I)) IS LESS THAN A(INDEX(I+1)).  THE PHYSICAL ARRANGEMENT
C  OF ARRAY A IS NOT ALTERED.
C
C  IF LSEQ IS PASSED IN AS TRUE, THE VECTOR INDEX IS INITIALLY LOADED
C  WITH VALUES FROM 1 TO N INCLUSIVE.  THIS SORTS OVER THE FIRST
C  N ELEMENTS OF ARRAY A.
C
C  IF LSEQ IS PASSED IN AS FALSE, THE FIRST N ELEMENTS OF INDEX ARE
C  ASSUMED TO BE THE INDICES OF A TO BE SORTED OVER.
C
C
C  THIS ALGORITHM IS AN ADAPTATION OF THE TECHNIQUE DESCRIBED IN:
C
C       SCOWEN, R.A. 1965. ALGORITHM 271; QUICKERSORT. COMM ACM.
C                    8(11) 669-670.
C
C----------
C  DECLARATIONS:
C----------
      LOGICAL LSEQ
      INTEGER INDEX,IPUSH,IL,IP,IU,INDIL,INDIP,INDIU,INDKL,INDKU,
     &        ITOP,JL,JU,KL,KU
C----------
C  DIMENSIONS:
C----------
      DIMENSION INDEX(N),A(N),IPUSH(33)
C----------
C  LOAD IND WITH VALUES FROM 1 TO N.
C----------
      IF (LSEQ) THEN
      DO 10 I=1,N
   10 INDEX(I)=I
      ENDIF
C----------
C  RETURN IF FEWER THAN TWO ELEMENTS IN ARRAY A.
C----------
      IF(N.LT.2) RETURN
C----------
C  BEGIN THE SORT.
C----------
      ITOP=0
      IL=1
      IU=N
   30 CONTINUE
      IF(IU.LE.IL) GO TO 40
      INDIL=INDEX(IL)
      INDIU=INDEX(IU)
      IF(IU.GT.IL+1) GO TO 50
      IF(A(INDIL).LE.A(INDIU)) GO TO 40
      INDEX(IL)=INDIU
      INDEX(IU)=INDIL
   40 CONTINUE
      IF(ITOP.EQ.0) RETURN
      IL=IPUSH(ITOP-1)
      IU=IPUSH(ITOP)
      ITOP=ITOP-2
      GO TO 30
   50 CONTINUE
      IP=(IL+IU)/2
      INDIP=INDEX(IP)
      T=A(INDIP)
      INDEX(IP)=INDIL
      KL=IL
      KU=IU
   60 CONTINUE
      KL=KL+1
      IF(KL.GT.KU) GO TO 90
      INDKL=INDEX(KL)
      IF(A(INDKL).LE.T) GO TO 60
   70 CONTINUE
      INDKU=INDEX(KU)
      IF(KU.LT.KL) GO TO 100
      IF(A(INDKU).LT.T) GO TO 80
      KU=KU-1
      GO TO 70
   80 CONTINUE
      INDEX(KL)=INDKU
      INDEX(KU)=INDKL
      KU=KU-1
      GO TO 60
   90 CONTINUE
      INDKU=INDEX(KU)
  100 CONTINUE
      INDEX(IL)=INDKU
      INDEX(KU)=INDIP
      IF(KU.LE.IP) GO TO 110
      JL=IL
      JU=KU-1
      IL=KU+1
      GO TO 120
  110 CONTINUE
      JL=KU+1
      JU=IU
      IU=KU-1
  120 CONTINUE
      ITOP=ITOP+2
      IPUSH(ITOP-1)=JL
      IPUSH(ITOP)=JU
      GO TO 30
      END


      SUBROUTINE STRATA
C--------------------------
C     STRATA CALCULATES THE NUMBER OF STRATA BASED ON THE CROWN ZONE
C     DEPTH, S(8), SPECIFIED BY THE USER. -- DWC, 5/30/95.
C     NOTE: NUMBER OF STRATA < 2 * NUMBER OF ENTITIES.
C--------------------------
 
      INCLUDE 'ENTITY.F77'
      INCLUDE 'SITE.F77'

      NSTR=INT((MAXCRHT - MINCRHT) / S(8)) + 1
        WRITE(*,*) 'NUMBER OF STRATA= ',NSTR
C       WRITE(*,*) 'MAX CROWN HT= ',MAXCRHT
C       WRITE(*,*) 'MIN CROWN HT= ',MINCRHT
      RETURN
      END


      SUBROUTINE ZONES2
C--------------------------
C     ZONES2 CREATES THE CROWN ZONE BOUNDARIES BASED ON THE NUMBER OF 
C     STRATA AND CROWN DEPTH, S(8). -- DWC, 5/30/95.
C--------------------------
 
      REAL TEMP
      INCLUDE 'ENTITY.F77'
      INCLUDE 'SITE.F77'

C     TEMP=MINCRHT
C     DO 10 K=NSTR,1,-1
C        IF(K.EQ.1) THEN
C           ZONE(K)=MAXCRHT
C        ELSE
C           ZONE(K)=TEMP
C        ENDIF
C           WRITE(*,*) 'ZONE DEPTHS= ',ZONE(K),'  STRATA= ',K
C        TEMP=TEMP + S(8)
C        NUM=NSTR-1
C        NUM2=NSTR
C  10 CONTINUE
C     RETURN
C     END
C     
C REVISED ALGORITHM  --- KSM 7/5/95. 
C ORIGINAL MISSES A ZONE IN SOME CASES
C DEFINE ZONES FROM THE TOP, RATHER THAN THE BOTTOM.      
      TEMP=MAXCRHT
      NUM=NSTR
      NUM2=NSTR+1
      DO 10 K=1,NUM2
            ZONE(K)=TEMP
C           WRITE(*,*) 'ZONE DEPTHS= ',ZONE(K),'  STRATA= ',K
            TEMP=TEMP - S(8)
C           IF(TEMP.LT.0) TEMP=0.0
   10 CONTINUE
      RETURN
      END

