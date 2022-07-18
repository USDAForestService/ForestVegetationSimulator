      SUBROUTINE DMCYCL
      IMPLICIT NONE
C----------
C  $Id$
C----------
C **DMCYCL -- NISI  Date of last revision April 7 1994
C----------------------------------------------------------------------
C Purpose:
C   This routine creates the mistletoe life history, independent of
C the cycle length (normally 10 years). For each crownthird of each
C record four life history compartments are maintained:
C
C   IMMATURE:      Young infections not yet large enough to produce
C                   flowers
C   LATENT         Infections large enough to flower, but suppressed
C                   by low light conditions
C   FLOWERING      Flowering infections that contribute to spread and
C                   intensification
C   NON-FLOWERING  Once-flowering infections that have been
C                   suppressed by low light conditions (swellings)
C
C These compartments are equilibrated when the routine is first
C entered, by doing a 30 year iteration of the pools, followed by
C scaling back to the initial condition. During normal timesteps.
C the pools are iterated on an annual basis. The interesting part of
C this is the role of light in moving latent and non-flowering
C infections to flowering, and flowering infections to non-flowering.
C Users are allowed to define a 4-piece linear function (using the
C DMLIGHT keyword) that describes the shape of the forward
C ( -> flowering) and backward ( <- flowering) reactions. Last of
C all, annual mortality is added to each of the life history pools.
C These keywords can be specified for each species.
C
C ** NOTE ON UNITS **
C
C   The units of infection are traditionally density: "DMR/MESH**3"
C In this routine, infection is occasionally transformed into an
C absolute number, by multiplying by the crownthird volume, and then
C transformed back again at the end.
C
C----------------------------------------------------------------------
C
C Called by:
C
C     DMTREG
C
C Other routines called:
C
C     [none]
C
C Argument list definitions:
C
C     [none]
C
C Local variable definitions:
C
C     INTEGER i        Index of tree record
C     INTEGER j        Loop counter for crownthird
C     INTEGER k        Loop counter to piecewise light function
C     INTEGER m        Loop counter for annual timestep
C     INTEGER r        Loop counter to breakpoint array
C     INTEGER s        Loop counter for MESH bands in breakpoints
C     INTEGER ISPC     Loop counter for species
C     INTEGER i1       Index to first record of each species
C     INTEGER i2       Index to last record of each species
C     INTEGER i3       Index to species-sorted array of records
C     INTEGER LHt      MESH band in which lower breakpoint lies
C     INTEGER UHt      MESH band in which upper breakpoint lies
C     INTEGER MidHt    MESH band for midpoint of crownthird
C     INTEGER Spin     Startup time delay (30 years)
C     INTEGER EPoint   Last year of timestep loop (usually 10)
C     INTEGER LghtIndx height-index position of each crownthird
C     REAL    SpSurv   Annual survival rate of mistletoe
C     REAL    New      New S+I addition to crownthird pool
C     REAL    FProp    Prop'n of light-suppressed becoming Flowering
C     REAL    FProp2   Prop'n of Immature becoming Latent
C     REAL    BProp    Prop'n of Flowering becoming light-suppressed
C     REAL    ImmLat   Amount of Immature becoming Latent
C     REAL    LatAct   Amount of Latent becoming Flowering
C     REAL    ActSpr   Amount of Flowering becoming Nonflowering
C     REAL    Spr      Amount of light-suppressed becoming Flowering
C     REAL    xImm     Size of Immature pool
C     REAL    xLat     Size of Latent pool
C     REAL    xSpr     Size of light-suppressed pool
C     REAL    xAct     Size of Flowering pool
C     REAL    xImmBC   Size of biocontrol suppressed immature pool
C     REAL    xLatBC   Size of biocontrol suppressed latent pool
C     REAL    xSprBC   Size of biocontrol suppressed nonflowering pool
C     REAL    xActBC   Size of biocontrol suppressed flowering pool
C     REAL    xDedBC   Size of biocontrol killed pool
C     REAL    mult     Multiplier used during equilibration
C     REAL    x        Initial Flowering+Nonflowering pool size
C     REAL    y        Volume (MESH**3) of each crownthird
C     REAL    TVol     Array of crownthird volumes (MESH**3)
C     REAL    FvecX    X-points array for forward light reaction
C     REAL    FvecY    Y-points array for forward light reaction
C     REAL    BvecX    X-points array for backward light reaction
C     REAL    BvecY    Y-points array for backward light reaction
C
C Common block variables and parameters:
C
C     MAXSP   PRGPRM
C     ISCT    CONTRL
C     IND1    ARRAYS
C     MXHT    DMCOM
C     BPCNT   DMCOM
C     VOLUME  DMCOM
C     CRTHRD  DMCOM
C     DMTINY  DMCOM
C     IMMAT   DMCOM
C     LATENT  DMCOM
C     SUPRSD   DMCOM
C     ACTIVE  DMCOM
C     DMFLWR  DMCOM
C     DMSURV  DMCOM
C     DMLtnp  DMCOM
C     DMLtRx  DMCOM
C     BrkPnt  DMCOM
C     DMRDMX  DMCOM
C     DMINF   DMCOM
C     ZPDn    DMCOM
C     DMCAP   DMCOM
C
C**********************************************************************

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'DMCOM.F77'
      INCLUDE 'PLOT.F77'

C Local variables.

      LOGICAL LOK
      INTEGER i, j, k, m, N, L, r, s
      INTEGER ISPC, ISPP, i1, i2, i3, BCT, IHT
      INTEGER LHt, UHt, MidHt(MAXTRE,CRTHRD)
      INTEGER Spin, EPoint
      INTEGER IYR,JYR,NTODO,IACTK,NPAR,MYACTS(1)

      REAL    SpSurv
      REAL    New, FProp2, HtWt
      REAL    ImmLat, LatAct, ActSpr,SprAct
      REAL    xImm, xLat, xSpr, xAct, xDedBC
      REAL    xImmBC(MAXBC), xLatBC(MAXBC)
      REAL    xSprBC(MAXBC), xActBC(MAXBC)
      REAL    ImmImmBC(MAXBC),LatLatBC(MAXBC)
      REAL    SprSprBC(MAXBC),ActActBC(MAXBC)
      REAL    ImmBCImm(MAXBC),LatBCLat(MAXBC)
      REAL    SprBCSpr(MAXBC),ActBCAct(MAXBC)
      REAL    SprDedBC,ActDedBC
      REAL    mult,x,y,XTiny
      REAL    xOriginal(MAXTRE,CRTHRD),TVol(MAXTRE,CRTHRD)
      REAL    FProp(MAXTRE,CRTHRD),BProp(MAXTRE,CRTHRD)
      REAL    FvecX(4), FvecY(4), BvecX(4), BvecY(4)
      REAL    BCMORT(MAXSP,ACTIVE,MXHT)
      REAL    BCSUPP(MAXSP,ACTIVE,MXHT,MAXBC)
      REAL    PRM(3),ATTEN(MXHT)

      REAL    ALGSLP


      DATA Spin / 30 /
      DATA MYACTS /2004/

      XTiny = LOG(DMTINY)

C     IF THIS IS THE FIRST ENTRY TO THIS ROUTINE, THEN SPIN WHEELS TO
C     FILL UP ALL THE POOLS (OTHERWISE THEY ARE ZERO ON FIRST ENTRY. THE
C     FLAG 'ZPDN' IS SET TO .TRUE. AT THE END OF THE ROUTINE.

      IF (ZPDn) THEN
        EPoint = IFINT
      ELSE
        EPoint = IFINT + Spin
      ENDIF

C     THIS LOOP WALKS THROUGH THE CROWN THIRD IN MESH METER BANDS FROM
C     'LHT' TO 'UHT'. IT IS ASSUMED THAT THE COVER MODEL-DERIVED 'VOLUME'
C     PARAMETER IS CORRECTLY WEIGHTED FOR BREAKPOINTS CROSSING MESH BANDS.
C     THE ASSUMPTION HAS BEEN INSPECTED AND LOOKS OK. 'Y' ADDS UP THE
C     CONTRIBUTIONS FROM EACH BAND; THEN THEY ARE ADDED INTO 'TVOL' TO
C     GIVE THE VOLUME (MESH**3) OF EACH CROWNTHIRD.

      DO i = 1,ITRN
        DO r = 2,BPCNT
          UHt = INT(BrkPnt(i,r-1)) + 1
          LHt = INT(BrkPnt(i,r  )) + 1
          y = 0.0
          TVol(i,r-1) = 0.0
          DO 800 s = LHt,UHt
            IF (s .GT. MXHT) GOTO 800
            IF ((LHt .EQ. s) .AND. (r .LT. BPCNT)) THEN
              HtWt = 1.0 - (BrkPnt(i,r) - INT(BrkPnt(i,r)))
            ELSE IF ((UHt .EQ. s) .AND. (r .GT. 2)) THEN
              HtWt = BrkPnt(i,r-1) - INT(BrkPnt(i,r-1))
            ELSE
              HtWt = 1.0
            END IF
            y =  y + HtWt * DMRDMX(i,s,VOLUME)
  800     CONTINUE
          TVol(i,r-1) = y
        ENDDO
      ENDDO

C     STEP 2: FIGURE OUT FORWARD AND BACKWARD RATES
C
C     CALCULATE LIGHT LEVEL AS FOUND AT THE MIDDLE CELL OF EACH CROWN
C     THIRD. PROPORTIONS GOING FORWARD AND BACKWARD DEPEND UPON THE LIGHT
C     LEVEL. THE COEFFICIENTS ARE UNDER USER CONTROL THROUGH THE DMLIGHT
C     KEYWORD.IT WOULD BE MORE ACCURATE TO USE A WEIGHTED MEASURE OF THE
C     VOLUME OF INFECTION IN EACH MESH.
C
C     THERE IS PROBABLY A CLEVER WAY OF ORGANIZING DMLTRX() SO THAT IT
C     CAN BE PASSED DIRECTLY TO ALGSLP. HELP YOURSELF IF YOU ARE FEELING
C     CLEVER TODAY.

      DO 98 ISPC = 1,MAXSP

        I1 = ISCT(ISPC,1)
        IF (I1 .EQ. 0) GOTO 98
        I2 = ISCT(ISPC,2)

        DO k = 1,DMLtnp(ISPC,1)
          FvecX(k) = DMLtRx(ISPC,1,1,k)
          FvecY(k) = DMLtRx(ISPC,1,2,k)
        ENDDO
        DO k = 1,DMLtnp(ISPC,2)
          BvecX(k) = DMLtRx(ISPC,2,1,k)
          BvecY(k) = DMLtRx(ISPC,2,2,k)
        ENDDO

C       COMPUTE THE LIGHT-DRIVEN FORWARD AND BACKWARD REACTION
C       FOR EACH TREE AND CROWNTHIRD. STORE THE INDEX POSITION
C       FOR THE CROWNTHIRD. STORE ORIGINAL ACTIVE POOL FOR USE
C       IN CYCLE 1

        DO I3 = I1,I2
          I = IND1(I3)
          DO j = 1,CRTHRD
            IHT = INT((BrkPnt(i,j) + BrkPnt(i,j+1)) / 2.) + 1
            IHT = MAX(1,MIN(IHT,MXHT))
            FProp(i,j) =
     >        ALGSLP(FLOAT(IHT),FvecX,FvecY,DMLtnp(ISPC,1))
            BProp(i,j) =
     >        ALGSLP(FLOAT(IHT),BvecX,BvecY,DMLtnp(ISPC,2))
            MidHt(I,J) = IHT
            xOriginal(I,J) = DMINF(i,j,ACTIVE)
          ENDDO
        ENDDO
   98 CONTINUE

C     MAIN LOOP OVER YEARS. IN MOST CYCLES EPOINT IS THE CYCLE LENGTH;
C     IN THE FIRST CYCLE THERE ARE 'SPIN' FOR TURNS TO HELP EQUILIBRATE
C     THE INITIAL POOLS

      DO m = 1,EPoint

C       ZERO BC-MORTALITY, BC-SUPPRESSION THIS YEAR

        DO J = 1,MAXSP
            DO K = 1,ACTIVE
              DO L = 1,MXHT
              BCMORT(J,K,L) = 0.0  ! MORTALITY
                DO N = 1,MAXBC
                BCSUPP(J,K,L,N) = 0.0  ! SUPPRESSION
              ENDDO
            ENDDO
          ENDDO
        ENDDO

C       PICK UP BIO-CONTROL (BC) KEYWORD PARAMETERS. IN SPITE OF THE VARIABLE NAME,
C       'BCMORT' STORES *SURVIVAL* AT THE POINT AND 'BCSUPP' STORES NON-SUPPRESSED
C       THESE ARE CONVERTED TO THEIR COMPLEMENTS LATER ON.

        IF (ZPDn) THEN
          IYR = IY(ICYC)+M-1
        ELSE
          IYR = IY(ICYC)+M-1-Spin
        ENDIF

        NTODO = 0
        LOK = .TRUE.
        CALL OPFIND(1,MYACTS,NTODO)
        IF (NTODO .NE. 0) THEN
          DO I = 1,NTODO
            CALL OPGET(I,3,JYR,IACTK,NPAR,PRM)
            IF (JYR .EQ. IYR) THEN
              BCT = IFIX(PRM(1))
              IF (BCT .LE. 0 .OR. BCT .GT. MAXBC) THEN
                LOK = .FALSE.
                GOTO 11
              ENDIF
              ISPP = BC(BCT)%Spp
              IF (ISPP .LT. 1 .OR. ISPP .GT. MAXSP) LOK = .FALSE.
   11         IF (LOK) THEN
                PRM(2) = MAX(0.0, PRM(2))
                PRM(3) = MAX(0.0, MIN(FLOAT(MXHT*MESH), PRM(3)))
                IHT    = MAX(1, MIN(MXHT, 1+IFIX(PRM(3)/FLOAT(MESH))))
                ATTEN(IHT) = 1.0
                DO L = IHT+1,MXHT
                  ATTEN(L) = ATTEN(L-1) * (1.0 - SHADE(L-1))
                ENDDO
                DO L = IHT-1,1,-1
                  ATTEN(L) = ATTEN(L+1) * (1.0 - SHADE(L+1))
                ENDDO
                DO L = 1,MXHT
                  DO N = 1,ACTIVE
                    BCMORT(ISPP,N,L) = BCMORT(ISPP,N,L) +
     >                    ATTEN(L) * PRM(2) * LOG(MAX(DMTINY,
     >                    1.0 - (BC(BCT)%Mort(N)/100.0)))
                    BCSUPP(ISPP,N,L,BCT) = BCSUPP(ISPP,N,L,BCT) +
     >                    ATTEN(L) * PRM(2) * LOG(MAX(DMTINY,
     >                    1.0 - (BC(BCT)%Suprs(N)/100.0)))
                  ENDDO
                ENDDO
                CALL OPDONE(I,IYR)
              ELSE
                CALL OPDEL1(I)
              ENDIF
            ENDIF
          ENDDO
        ENDIF

        DO 99 ISPC = 1,MAXSP

          I1 = ISCT(ISPC,1)
          IF (I1 .EQ. 0) GOTO 99
          I2 = ISCT(ISPC,2)

          SpSurv = DMSURV(ISPC)

C         COMPUTE IMMATURE->LATENT FORWARD REACTIONS THE DMFLWR (NOMINALLY
C         4 YEAR) LAG MEANS ABOUT ONE QUARTER MATURE EACH YEAR, ASSUMING A FLAT
C         AGE-STRUCTURE (NOT QUITE RIGHT).

          FProp2 = 1.0/FLOAT(DMFLWR(ISPC))

C         COMPUTE MULTIPLICATIVE MORTALITY AND SUPPRESSION FOR EACH SPECIES;
C         CONVERT FROM SUM(LOG(SURVIVAL)) TO MORTALITY; SAME FOR SUPPRESSION
C         BOUND FOR NUMERICAL CONTROL

          DO N = 1,ACTIVE
            DO L = 1,MXHT
              BCMORT(ISPC,N,L) = 1.0 -
     >          EXP(MAX(XTiny,BCMORT(ISPC,N,L)))
              DO R = 1,MAXBC
                BCSUPP(ISPC,N,L,R) = 1.0 -
     >            EXP(MAX(XTiny,BCSUPP(ISPC,N,L,R)))
              ENDDO
            ENDDO
          ENDDO

          DO I3 = I1,I2
            I = IND1(I3)
            DO j = 1,CRTHRD

C             CONVERT THE NEW INPUTS INTO DENSITY AND ADD TO EXISTING POOLS.
C             TVOL(I,J) "SHOULD" NEVER BE ZERO, BUT IN PRACTICE SOMETIMES IS.

              IF (TVol(i,j) .GT. DMTINY) THEN
                New = (NewSpr(i,j) + NewInt(i,j)) / TVol(i,j)
              ELSE
                New = DMTINY
              ENDIF

C SUM ALL THE LIVE MATERIAL INTO 'x'; RESCALE x SO THAT IT LOADING IS ASYMPTOTIC
C TO DMCAP (nominally 3.0); r=1/3
C
C   DM' = DM + [3 - DM] * [1 - EXP(-r * New)]
C
C CARRYING CAPACITY IS SET TO 3 DM UNITS !!  New IS RESCALED SO THAT THE
C TOTAL LIVE POOL SIZE IS CONSTRAINED TO 3. LUCKILY ALL THE RESCALING
C HAPPENS IN ONE POOL ONLY, SO THERE IS NO NEED TO PARTITION IT.

              x = 0.0
              DO L = 1,ACTIVE
                x = x + DMINF(i,j,L)
                DO N = 1,MAXBC
                  x = x + DMINF_BC(i,j,L,N)
                ENDDO
              ENDDO
              New = (DMCAP(ISPC) - x) *
     >          (1.0 - EXP((-1.0/DMCAP(ISPC)) * New))

C             MESH-POSITION OF EACH CROWNTHIRD: FOR BCMORT, BCSUPP INDEX

              IHT = MIDHT(I,J)

C             ASSIGN TEMPORARY VARIABLES
              xImm = DMINF(i,j,IMMAT)
              xLat = DMINF(i,j,LATENT)
              xSpr = DMINF(i,j,SUPRSD)
              xAct = DMINF(i,j,ACTIVE)
              DO L = 1,MAXBC
                xImmBC(L) = DMINF_BC(i,j,IMMAT, L)
                xLatBC(L) = DMINF_BC(i,j,LATENT,L)
                xSprBC(L) = DMINF_BC(i,j,SUPRSD,L)
                xActBC(L) = DMINF_BC(i,j,ACTIVE,L)
              ENDDO
              xDedBC = DMINF(i,j,DEAD_BC)

C             COMPUTE AND REMOVE BC-KILL: IMM,LAT,SPR,ACT
              xImm = xImm * (1.0 - BCMORT(ISPC,IMMAT,IHT))
              xLat = xLat * (1.0 - BCMORT(ISPC,LATENT,IHT))
              SprDedBC = xSpr * BCMORT(ISPC,SUPRSD,IHT)
              ActDedBC = xAct * BCMORT(ISPC,ACTIVE,IHT)
              xSpr     = xSpr - SprDedBC
              xAct     = xAct - ActDedBC

C             COMPUTE AND REMOVE TRANSITIONS TO BC-SUPPRESSED: IMM,LAT,SPR,ACT
              DO L = 1,MAXBC
                ImmImmBC(L) = xImm * BCSUPP(ISPC,IMMAT, IHT,L)
                LatLatBC(L) = xLat * BCSUPP(ISPC,LATENT,IHT,L)
                SprSprBC(L) = xSpr * BCSUPP(ISPC,SUPRSD,IHT,L)
                ActActBC(L) = xAct * BCSUPP(ISPC,ACTIVE,IHT,L)
                xImm        = xImm - ImmImmBC(L)
                xLat        = xLat - LatLatBC(L)
                xSpr        = xSpr - SprSprBC(L)
                xAct        = xAct - ActActBC(L)
              ENDDO

C             COMPUTE TRANSITIONS: ALL CATEGORIES
              ImmLat = xImm * FProp2
              LatAct = xLat * FProp(i,j)
              SprAct = xSpr * FProp(i,j)
              ActSpr = xAct * BProp(i,j)
              DO L = 1,MAXBC
                ImmBCImm(L) = xImmBC(L) * (1.0 - BC(L)%HfLf(IMMAT))
                LatBCLat(L) = xLatBC(L) * (1.0 - BC(L)%HfLf(LATENT))
                SprBCSpr(L) = xSprBC(L) * (1.0 - BC(L)%HfLf(SUPRSD))
                ActBCAct(L) = xActBC(L) * (1.0 - BC(L)%HfLf(ACTIVE))
              ENDDO

C             REMOVE TRANSITIONS: ALL CATEGORIES
              xImm   = xImm - ImmLat
              xLat   = xLat - LatAct
              xSpr   = xSpr - SprAct
              xAct   = xAct - ActSpr
              DO L = 1,MAXBC
                xImmBC(L) = xImmBC(L) - ImmBCImm(L)
                xLatBC(L) = xLatBC(L) - LatBCLat(L)
                xSprBC(L) = xSprBC(L) - SprBCSpr(L)
                xActBC(L) = xActBC(L) - ActBCAct(L)
              ENDDO

C             ADD TRANSITIONS: ALL CATEGORIES
              xLat   = xLat + ImmLat
              xSpr   = xSpr + ActSpr
              xAct   = xAct + LatAct
              xAct   = xAct + SprAct
              DO L = 1,MAXBC
                xImm      = xImm      + ImmBCImm(L)
                xLat      = xLat      + LatBCLat(L)
                xSpr      = xSpr      + SprBCSpr(L)
                xAct      = xAct      + ActBCAct(L)
                xImmBC(L) = xImmBC(L) + ImmImmBC(L)
                xLatBC(L) = xLatBC(L) + LatLatBC(L)
                xSprBC(L) = xSprBC(L) + SprSprBC(L)
                xActBC(L) = xActBC(L) + ActActBC(L)
              ENDDO
              xDedBC   = xDedBC + SprDedBC
              xDedBC   = xDedBC + ActDedBC

C             REMOVE NATURAL MORTALITY: ALL CATEGORIES
              xImm   = xImm * SpSurv
              xLat   = xLat * SpSurv
              xSpr   = xSpr * SpSurv
              xAct   = xAct * SpSurv
              DO L = 1,MAXBC
                xImmBC(L) = xImmBC(L) * SpSurv
                xLatBC(L) = xLatBC(L) * SpSurv
                xSprBC(L) = xSprBC(L) * SpSurv
                xActBC(L) = xActBC(L) * SpSurv
              ENDDO
              xDedBC   = xDedBC * SpSurv

C             ADD NEW RECRUITS: IMM
              xImm   = xImm + New         ! new S+I

C             AFTER SPINNING TO PRIME THE POOLS, ADJUST PARAMETERS BACK SO
C             THAT 'XACT' EQUALS THE ORIGINAL VALUE. IN CASES WHERE THE ORIGINAL
C             VALUE 'XORIGINAL'IS ZERO, THE MULTIPLIER TERM IS ADJUSTED SO THAT
C             THE ADJUSTED POOL VALUES ARE ARBITRARILY SET TO 5% OF THE POST-SPIN
C             VALUE.

              IF (.NOT. ZPDn .AND. m .EQ. Spin) THEN
                y = xAct + xSpr + xDedBC
                DO L = 1,MAXBC
                  y = y + xSprBC(L) + xActBC(L)
                ENDDO
                IF ((xOriginal(i,j) * y) .GT. DMTINY) THEN
                  mult = xOriginal(i,j) / y
                ELSE
                  mult = 0.05
                END IF
                xImm = xImm * mult
                xLat = xLat * mult
                xSpr = xSpr * mult
                xAct = xAct * mult
                DO L = 1,MAXBC
                  xImmBC(L) = xImmBC(L) * mult
                  xLatBC(L) = xLatBC(L) * mult
                  xSprBC(L) = xSprBC(L) * mult
                  xActBC(L) = xActBC(L) * mult
                ENDDO
                xDedBC = xDedBC * mult
              END IF

C             ASSIGN PERMANENT VARIABLES
              DMINF(i,j,IMMAT)  = xImm
              DMINF(i,j,LATENT) = xLat
              DMINF(i,j,SUPRSD) = xSpr
              DMINF(i,j,ACTIVE) = xAct
              DO L = 1,MAXBC
                DMINF_BC(i,j,IMMAT,L)  = xImmBC(L)
                DMINF_BC(i,j,LATENT,L) = xLatBC(L)
                DMINF_BC(i,j,SUPRSD,L) = xSprBC(L)
                DMINF_BC(i,j,ACTIVE,L) = xActBC(L)
              ENDDO
              DMINF(i,j,DEAD_BC) = xDedBC

C 'DMCAP' PREVENTS IMMENSE NUMBERS OF INFECTIONS IN A "MESH" VOLUME OF CANOPY.

              x = DMINF(i,j,ACTIVE) + DMINF(i,j,SUPRSD)
     >          + DMINF(i,j,DEAD_BC)
              DO L = 1,MAXBC
                x = x + DMINF_BC(i,j,ACTIVE,L) + DMINF_BC(i,j,SUPRSD,L)
              ENDDO
              IF (x .GT. DMCAP(ISPC)) THEN
                y = DMCAP(ISPC) / x
                DMINF(i,j,SUPRSD) = DMINF(i,j,SUPRSD) * y
                DMINF(i,j,ACTIVE) = DMINF(i,j,ACTIVE) * y
                DO L = 1,MAXBC
                  DMINF_BC(i,j,SUPRSD,L) = DMINF_BC(i,j,SUPRSD,L) * y
                  DMINF_BC(i,j,ACTIVE,L) = DMINF_BC(i,j,ACTIVE,L) * y
                ENDDO
                DMINF(i,j,DEAD_BC) = DMINF(i,j,DEAD_BC) * y
              END IF

            ENDDO     ! end of j CRTHRD loop
          ENDDO     ! end of i3 loop
   99   CONTINUE  ! end of 99 ISPC loop
      ENDDO     ! end of IFINT year loop

      ZPDn = .TRUE.

      RETURN
      END
