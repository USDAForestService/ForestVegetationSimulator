      SUBROUTINE ESADDT(ICALL)
      IMPLICIT NONE
C----------
C  **ESADDT-SEI DATE OF LAST REVISION:  06/01/04
C----------

C     ADD NEW TREES FROM A FILE OR EXTERNAL DATA BASE.

C     COMMONS

      INCLUDE 'PRGPRM.F77'
	INCLUDE 'FMPARM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'OUTCOM.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'ESHAP.F77'
      INCLUDE 'ESHAP2.F77'
      INCLUDE 'VARCOM.F77'
	INCLUDE 'FMCOM.F77'
      INCLUDE 'METRIC.F77'
	INCLUDE 'BCPLOT.F77'

      INTEGER       ICALL

      LOGICAL       DEBUG,DOTALLY
      CHARACTER*250 CMDLN
      CHARACTER*250 FNAM
      CHARACTER*4   CYR
      CHARACTER*7   VVER
	CHARACTER*6   SLPOSN(3),DISTRB(5),DSTR
      INTEGER       I,J,KODE,IKEEP,IPYR
      INTEGER       MYACT(1),MYACT2(3),NTODO,ITODO,NP,IACTK,IDT
      INTEGER       KDT1,KDT2
	INTEGER       IDT2,JDT2,ISTAT
      INTEGER       IMETH
      INTEGER       FNO,NPRMS
      INTEGER       REBIN(5),PBIN(3)
	INTEGER       JDSDAT,JYRLRM,INDX(3)
      REAL          PRMS(3),PRMST(1),SUMSP(6),X,Y,Z,XY(3)

      DATA MYACT  /432/
      DATA MYACT2 /491,492,493/
      DATA FNO    /101/

C     MAP 5 FVS IPHYS PLOT ZONES TO 3 (LOWER, MID, UPPER)
C       SLOPE POSITION AS DUMMIES:  LOWER, MIDDLE, AND UPPER
C       (IPHYS)   PYHSIOGRAPHY
C       -------   ------------
C          1      BOTTOM      -> 1 = LOWER
C          2      LOWER SLOPE -> 1 = LOWER
C          3      MID SLOPE   -> 2 = MIDDLE
C          4      UPPER SLOPE -> 3 = UPPER
C          5      RIDGE       -> 3 = UPPER

	DATA REBIN  / 1,1,2,3,3 /
	DATA SLPOSN / 'LOWER','MIDDLE','UPPER' /
	DATA DISTRB / 'BURN','BRUSH','MECH','MIXED','NONE' /

C     SET THE DEBUG OPTION

      CALL DBCHK (DEBUG,'ESADDT',6,ICYC)

C     PROCESS ADDTREES KEYWORDS

      IDT = 0
	DOTALLY = .FALSE.

      CALL OPFIND (1,MYACT,NTODO)
      IF (DEBUG) WRITE (JOSTND,5) NTODO
    5 FORMAT (' IN ESADDT: OPTS NTODO=',I2)
      IF (NTODO .GT. 0) THEN
        KDT1 = IY(ICYC)
        KDT2 = IY(ICYC+1)-1
        DO ITODO = 1,NTODO
          CALL OPGET(ITODO,2,IDT,IACTK,NP,PRMS)

C         GET THE METHOD SWITCH

          IMETH=1
          IF (IABS(NP) .GE. 2) THEN
            IPYR =INT(PRMS(1))+KDT2 ! planting year
            IMETH=INT(PRMS(2))
          ENDIF
          IF (DEBUG) WRITE (JOSTND,6) IMETH
    6     FORMAT (' IN ESADDT, IMETH=',I2)
          
          CALL OPGETC (ITODO,CMDLN)
          IF (CMDLN .EQ. ' ') GOTO 201

          CALL OPDONE(ITODO,IDT)
          
c         Construct input filename: Keyfile_StandID_Year_BC.ES1
          j = 0
          DO I = 1,250
              FNAM(I:I) = CHAR(32)          ! put space code
              IF (KWDFIL(I:I) .NE. ' ') THEN
                  J = J+1
                  FNAM(J:J) = KWDFIL(I:I)
              ENDIF
          ENDDO
          J = J+1
          FNAM(J:J) = '_'
          DO I = 1,26
              IF (NPLT(I:I) .NE. ' ') THEN
                  J = J+1
                  FNAM(J:J) = NPLT(I:I)
              ENDIF
          ENDDO
          J = J+1
          FNAM(J:J) = '_'
          WRITE(CYR,'(I4)') KDT2
          DO I = 1,4
              IF (CYR(I:I) .NE. ' ') THEN
                  J = J+1
                  FNAM(J:J) = CYR(I:I)
              ENDIF
          ENDDO
          J = J+1
          FNAM(J:J) = '_'
          CALL VARVER(VVER)
          DO I = 1,2
              IF (VVER(I:I) .NE. ' ') THEN
                  J = J+1
                  FNAM(J:J) = VVER(I:I)
              ENDIF
          ENDDO
          FNAM(J+1:J+5) = ".ES1"

c         open and populate file with information to send to
c         external regeneration model: suffix = ".es1"

          CALL MYOPEN (FNO,TRIM(FNAM),5,150,0,1,1,0,KODE)
          IF (KODE .NE. 0) GOTO 201

c         concatenate the name of the exe and input fn
          J = LEN_TRIM(CMDLN)+1
          CMDLN(J:J) = ' '
          DO I = 1,LEN_TRIM(FNAM)
              IF (FNAM(I:I) .NE. ' ' .AND. J .LE. 250) THEN
                  J = J+1
                  CMDLN(J:J) = FNAM(I:I)
              ENDIF
          ENDDO          

          SELECT CASE (IMETH)

c           Run external program (Original ProgBC MSN model)

            CASE (1)

              ! Method (1)
              WRITE(FNO,'(I30)',ERR=201) IMETH
              ! StandID (2)
              WRITE(FNO,'(A30)',ERR=201) TRIM(NPLT)
              ! BEC (3)
              WRITE(FNO,'(A30)',ERR=201) TRIM(BEC%PrettyName)

              ! set slope position from most frequent
              ! from PLOTINFO keyword
	        DO I = 1,3
                PBIN(I) = 0
              ENDDO
              DO I=1,NPTIDS
	          J = REBIN(IPHYS(IPTIDS(I)))
	          PBIN(J) = PBIN(J) + 1
              ENDDO
	        J = PBIN(1)
	        DO I = 2,3
                IF (PBIN(I) .GT. J) J = I
              ENDDO
              WRITE(FNO,'(A30)',ERR=201) TRIM(SLPOSN(J)) ! (4)

              ! compute and write type and calendar year of most
	        ! recent disturbance: default = 20 years before inventory
	        ! with no site prep

              DO I = 1,3
	          IF (JDST(I) .LT. 0) JDST(I) = IY(1)-20
	        ENDDO

              DSTR = DISTRB(5)
	        IF (ICYC.LE.1) THEN
	          JDSDAT = IY(1)-20
	          JYRLRM = IY(1)-20
	        ELSE
                JDSDAT = MAX(IDSDAT,IY(1)-20)
	          JYRLRM = IYRLRM
                JDST(1) = MAX(JDST(1),INT(ZBURN))
                JDST(2) = MAX(JDST(2),INT(ZBRSH))
                JDST(3) = MAX(JDST(3),INT(ZMECH))
	        ENDIF
              IF (ONTREM(7).GT.0.0) JYRLRM = IY(I)

              ! CHECK FOR RECENT SITE PREP
              !  1 => 491 BURN PREP,
              !  2 => 492 BRUSH PREP,
              !  3 => 493 MECH PREP

              CALL OPFIND (3,MYACT2,NTODO)
	        IF (NTODO .GT. 0) THEN
	          DO I = 1,3
                  JDT2  = 0
	            IDT2  = 0
	            ISTAT = 0
                  CALL OPSTUS(MYACT2(I),JDSDAT,KDT2,0,JDT2,IDT2,NPRMS,
     >              ISTAT,KODE)
                  IF (KODE.EQ.0 .AND. IDT2.NE.0) THEN
                    JDST(I) = IDT2
	              DOTALLY = .TRUE.
	            ENDIF
                  CALL OPSTUS(MYACT2(I),JDSDAT,KDT2,1,JDT2,IDT2,NPRMS,
     >              ISTAT,KODE)
                  IF (KODE.EQ.0 .AND. IDT2.NE.0 .AND. IDT.GT.JDST(I))
     >              JDST(I) = IDT2
	              DOTALLY = .TRUE.
	            ENDDO
	        ENDIF

	        ! FIND MOST RECENT KIND OF DISTURBANCE
	        DO I = 1,3
	          XY(I) = FLOAT(JDST(I))
	        ENDDO
              CALL RDPSRT(3,XY,INDX,.TRUE.)

	        JDSDAT = MAX(JDST(INDX(1)),JYRLRM)

              ! DISTURBANCE IS MOST RECENT SITE PREP, UP TO 10 YRS;
	        ! BURN+BRUSH WITHIN ONE YR OF ONE-ANOTHER => 'MIXED' PREP
	        IF (INDX(1) .EQ. 1) THEN
                DSTR = DISTRB(1)
	          IF (INDX(2) .EQ. 2 .AND.
     >            ABS(JDST(1)-JDST(2)) .LE. 1) DSTR = DISTRB(4)
	        ELSEIF (INDX(1) .EQ. 2) THEN
                DSTR = DISTRB(2)
	          IF (INDX(2) .EQ. 1 .AND.
     >            ABS(JDST(1)-JDST(2)) .LE. 1) DSTR = DISTRB(4)
	        ELSEIF (INDX(1) .EQ. 3) THEN
                DSTR = DISTRB(3)
	        ENDIF
	        IF (ABS(IPYR - JDSDAT) .GT. 10) DSTR = DISTRB(5)

              ! Calendar Planting Year (5)
              WRITE(FNO,'(I30)',ERR=201) IPYR

	        ! years since disturbance (6)
              WRITE(FNO,'(I30)',ERR=201) ABS(IPYR-JDSDAT)

	        ! most recent site preparation (7)
              WRITE(FNO,'(A30)',ERR=201) TRIM(DSTR)

              WRITE(FNO,'(I30)',ERR=201) IASPEC     ! (8) aspect (deg)
              WRITE(FNO,'(I30)',ERR=201) ISLOP      ! (9) slope (%)
              WRITE(FNO,'(I30)',ERR=201) INT(ELEV * 100.0 * FtToM) ! (10) el (m)

              DO I = 1,6
                SUMSP(I) = 0.0
              ENDDO

c             add tpa and ba, excluding trees <7.5cm
              DO I = 1,ITRN
                IF (DBH(I) .LE. 7.5*CMtoIN) CYCLE
                X = PROB(I)
                Y = X * DBH(I) * DBH(I) * 0.0054542
                SELECT CASE (ISP(I))
                  CASE (4,5,6,8,9) ! shade tolerant
                    SUMSP(1) = SUMSP(1) + X
                    SUMSP(4) = SUMSP(4) + Y
                  CASE (1,3,14)    ! shade semi-tolerant
                    SUMSP(2) = SUMSP(2) + X
                    SUMSP(5) = SUMSP(5) + Y
                  CASE (2,7,10,11,12,13,15) ! shade intol
                    SUMSP(3) = SUMSP(3) + X
                    SUMSP(6) = SUMSP(6) + Y
                END SELECT
              ENDDO

c               Residual TPH of shade tolerant, semi- and intolerant (11-13)
	        DO I = 1,3
	          WRITE(FNO,'(F30.1)',ERR=201) SUMSP(I) / ACRtoHA
	        ENDDO
c               Residual BA of shade tolerant, semi- and intolerant (14-16)
	        DO I = 4,6
	          WRITE(FNO,'(F30.1)',ERR=201) SUMSP(I) * FT2pACRtoM2pHA
	        ENDDO

              WRITE(FNO,'(F30.1)',ERR=201) ATAVD * INtoCM !AT qDBH (17)
              WRITE(FNO,'(F30.1)',ERR=201) ATCCF          !AT CCF  (18)

c           External program 2 (Terry Lee / Derek Sattler model)

            CASE (2)

C             ASSEMBLE AND WRITE DATA

              ! Method (1)
              WRITE(FNO,'(I30)',ERR=201) IMETH

              ! StandID (2)
              WRITE(FNO,'(A30)',ERR=201) TRIM(NPLT)

              ! Calendar Planting Year (3)
              WRITE(FNO,'(I30)',ERR=201) IPYR

              ! BEC (4)
              WRITE(FNO,'(A30)',ERR=201) TRIM(BEC%fullname)

              ! Site Series (5)
              WRITE(FNO,'(A30)',ERR=201) TRIM(BEC%series)

              ! Elevation (m) (6)
              WRITE(FNO,'(I30)',ERR=201) INT(ELEV * 100. * FtToM)

              ! QMD (cm) (7)
              WRITE(FNO,'(F30.1)',ERR=201) ATAVD * INtoCM

              ! CCF (8)
              WRITE(FNO,'(F30.1)',ERR=201) ATCCF
             
              DO I = 1,6
                SUMSP(I) = 0.0
              ENDDO
              DO I = 1,ITRN
                X = PROB(I)
                Y = X * DBH(I) * DBH(I) * 0.0054542
                SELECT CASE (ISP(I))
                  CASE (1:6,8:10,14)   ! TPH & BA - non-PL conifers
                    SUMSP(1) = SUMSP(1) + X
                    SUMSP(4) = SUMSP(4) + Y
                  CASE (7)             ! TPH & BA - lodgepole pine
                    SUMSP(2) = SUMSP(2) + X
                    SUMSP(5) = SUMSP(5) + Y
                  CASE (11:13,15)      ! TPH & BA - deciduous
                    SUMSP(3) = SUMSP(3) + X
                    SUMSP(6) = SUMSP(6) + Y
                END SELECT
              ENDDO

              ! Residual TPH of non-pine, pine, deciduous (9-11)
	        DO I = 1,3
	          WRITE(FNO,'(F30.1)',ERR=201) SUMSP(I) / ACRtoHA
	        ENDDO
              ! Residual BA of of non-pine, pine, deciduous (12-14)
	        DO I = 4,6
	          WRITE(FNO,'(F30.1)',ERR=201) SUMSP(I) * FT2pACRtoM2pHA
	        ENDDO

              DO I = 1,3  ! zero temp vars
                SUMSP(I) = 0.0
              ENDDO
              DO I = 1,NSNAG  ! sum up hard and soft PL snags
                IF (SPS(I) .EQ. 7) THEN
	            X = 0.0
	            IF (DENIH(I) .GT. 0.0) X =     DENIH(I)
                  IF (DENIS(I) .GT. 0.0) X = X + DENIS(I)
                  Z = X * DBHS(I) * DBHS(I)
                  Y = Z * 0.0054542
                  SUMSP(1) = SUMSP(1) + X  ! sum TPH
                  SUMSP(2) = SUMSP(2) + Y  ! sum BA
	            SUMSP(3) = SUMSP(3) + Z  ! sum DBH**2
	          ENDIF
              ENDDO
	        IF (SUMSP(1).GT. 0.0) THEN
                SUMSP(3) = SQRT(SUMSP(3)/SUMSP(1))
	        ELSE
	          SUMSP(3) = 0.0
	        ENDIF

              ! TPH, BA, QMD of PL snags (15-17)
              WRITE(FNO,'(F30.1)',ERR=201) SUMSP(1) / ACRtoHA
              WRITE(FNO,'(F30.1)',ERR=201) SUMSP(2) * FT2pACRtoM2pHA
              WRITE(FNO,'(F30.1)',ERR=201) SUMSP(3) * INtoCM

            CASE (3)      ! Derek Sattler SORTIE-ND model

!c             write data
!              WRITE(FNO,'(A30)',ERR=301) TRIM(NPLT)   ! StandID (1)
!              WRITE(FNO,'(I30)',ERR=301) INT(PRMS(1))!  Sim Yrs (2)
!              WRITE(FNO,'(I30)',ERR=301) IPYR   ! Planting Year (3)
!              WRITE(FNO,'(I30)',ERR=301) INT(TLAT)   ! Latitude (4)
!              WRITE(FNO,'(I30)',ERR=301) ITRN  ! Live Tree Recs (5)
!              DO I = 1,ITRN
!                WRITE(FNO,'(A30, 3(F10.2))',ERR=301)
!     &            JSP(ISP(I)),
!     &            DBH(I)  * INtoCM,
!     &            HT(I)   * FTtoM,
!     &            PROB(I) * ACRtoHA
!              ENDDO
!              WRITE(FNO,'(I30)',ERR=301) NSNAG      ! Snag Recs (6)
!              DO I = 1,NSNAG
!                IF(HARD(I) .AND. DENIH(I) .GT. 0.01) THEN
!                  WRITE(FNO,'(A30, 3(F10.2))',ERR=301)
!     &              JSP(SPS(I)),
!     &              DBHS(I)  * INtoCM, 
!     &              HTIH(I)  * FTtoM,
!     &              DENIH(I) / ACRtoHA
!                ELSEIF (DENIS(I) .GT. 0.01) THEN
!                  WRITE(FNO,'(A30, 3(F10.2))',ERR=301)                
!     &              JSP(SPS(I)),
!     &              DBHS(I)  * INtoCM, 
!     &              HTIS(I)  * FTtoM,
!     &              DENIS(I) / ACRtoHA
!                ENDIF
!              ENDDO
           
            END SELECT

            CLOSE(FNO)

            CALL SYSTEM(CMDLN)

            J = LEN_TRIM(FNAM)
            FNAM(J-3:J) = ".ES2"
            CALL MYOPEN (FNO,TRIM(FNAM),5,150,0,1,1,0,KODE)
            IF (KODE .NE. 0) GOTO 201
            READ(FNO,'(I10)',END=201) IKEEP
            CALL OPRDAT(FNO,KODE)
            IF (KODE .GT. 0) THEN
              DOTALLY = .TRUE.
              GOTO 202
            ENDIF
  201       CALL OPDEL1(ITODO)
  202       IF (IKEEP .NE. 1) THEN
              CLOSE(FNO, STATUS = 'DELETE')
            ELSE
              CLOSE(FNO, STATUS = 'KEEP')
            ENDIF

  404   ENDDO
      ENDIF

      ! schedule TALLYONE; so that site preps are accomplished
      ! and planted trees are promoted to the treelist

      IF (DOTALLY) THEN
        PRMST(1) = IDT
        CALL OPADD (IDT,428,0,1,PRMST,KODE)
        IF (KODE .EQ. 0) THEN
          CALL OPINCR (IY,ICYC,NCYC)
	  ENDIF
	ENDIF

      RETURN
      END
