      SUBROUTINE FMDOUT (IYR)
      IMPLICIT NONE
C----------
C FIRE-BASE $Id$
C----------
*     SINGLE-STAND VERSION
*     CALLED FROM: FMMAIN
*  PURPOSE:
*     PRINT THE FUELS AND DEBRIS
*----------------------------------------------------------------------
*
*  CALL LIST DEFINITIONS:
*     IYR:     CURRENT YEAR
*
*  LOCAL VARIABLE DEFINITIONS:
*
*  COMMON BLOCK VARIABLES AND PARAMETERS:
*
***********************************************************************

C.... PARAMETER STATEMENTS.

C.... PARAMETER INCLUDE FILES.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

C.... COMMON INCLUDE FILES.
      INCLUDE 'PLOT.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'

      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'

C.... VARIABLE DECLARATIONS.
      REAL      TOTSNG(2), TOTLIV(2), TONREM
      LOGICAL   DEBUG, LPRINT, LPRINT2, LPRINT3, LMERCH
      INTEGER   IYR,JROUT,I,J,K,L,ISZ,JSZ,KSZ,
     &          KSP,IDC,ITM,ITON,JS,II,DBSKODE
      REAL      TOTDUF,TOTLIT,SNVIS,SNVIH,ACRES,TOTFOL,TOTCON,D,H,
     &          X,VT,TOTSTD,TOTFUL,TOTSUR, SMALL2, LARGE2,LARGE12,
     &          CWDDEN(2,4),V1(16),V2(14)
      LPRINT = .FALSE.
      LPRINT2 = .FALSE.
      LPRINT3 = .FALSE.
C-----------
C     CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'FMDOUT',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC
  7   FORMAT(' ENTERING FMDOUT CYCLE = ',I2)
C
C     RETREIVE THE UNIT NUMBER TO BE USED FOR THE ALL FUELS AND CARBON REPORTS.
C
      CALL GETLUN (JROUT)
C
      IF (DEBUG) WRITE(JOSTND,40) IFLALB,IFLALE,IDFLAL,
     >       JROUT,NSNAG
 40   FORMAT(' FMDOUT: IFLALB=',I5,' IFLALE=',I5,
     >       ' IDFLAL=',I5,' JROUT=',I3,' NSNAG=',I5)

C     FIRST CHECK TO SEE IF WE NEED TO DO THIS FOR FUELS REPORTING
C     IF THIS YEAR IS WITHIN THE REPORTING PERIOD, SET THE
C     PRINT FLAG TO TRUE (DEFAULT IS FALSE)

      LPRINT = .TRUE.
      IF (IYR .EQ. 0 .AND. IYR.EQ. IFLALE) GOTO 45

      IF (IYR .LT. IFLALB .OR. IYR .GT. IFLALE) LPRINT = .FALSE.

 45   CONTINUE

C     DO THIS AGAIN FOR THE DOWN WOOD VOLUME AND COVER REPORTS 

      LPRINT2 = .TRUE.
      IF (IYR .EQ. 0 .AND. IYR.EQ. IDWRPE) GOTO 46

      IF (IYR .LT. IDWRPB .OR. IYR .GT. IDWRPE) LPRINT2 = .FALSE.

 46   CONTINUE

      LPRINT3 = .TRUE.
      IF (IYR .EQ. 0 .AND. IYR.EQ. IDWCVE) GOTO 47

      IF (IYR .LT. IDWCVB .OR. IYR .GT. IDWCVE) LPRINT3 = .FALSE.

 47   CONTINUE

C
C     Zero the summation columns (index 1=3 and 4=5)
C
      DO I = 1, MXFLCL
         DO J = 1, 2
            CWD(3,I,J,5) = 0.0
         ENDDO
      ENDDO

C     Calculate summation in Pile categories

      DO I = 1, 2
         DO J = 1, MXFLCL
            DO K = 1, 2
               DO L = 1, 4
                  CWD(3,J,K,5) = CWD(3,J,K,5) + CWD(I,J,K,L)
               ENDDO
            ENDDO
         ENDDO
      ENDDO

C     ACCUMULATE SURFACE FUELS FOR PRINTING

      SMALL2 = 0.0
      LARGE2 = 0.0
      LARGE12 = 0.0
      DO ISZ = 1,3
         JSZ = ISZ + 3
         KSZ = ISZ + 6
         SMALL2 = SMALL2 + CWD(3,ISZ,1,5) + CWD(3,ISZ,2,5)
         LARGE2 = LARGE2 + CWD(3,JSZ,1,5) + CWD(3,JSZ,2,5) 
     &                   + CWD(3,KSZ,1,5) + CWD(3,KSZ,2,5)
      ENDDO
      LARGE12 = CWD(3,6,1,5)+CWD(3,6,2,5)+CWD(3,7,1,5)+CWD(3,7,2,5) 
     &         +CWD(3,8,1,5)+CWD(3,8,2,5)+CWD(3,9,1,5)+CWD(3,9,2,5) 

      TOTDUF = CWD(3,11,1,5) + CWD(3,11,2,5)
      TOTLIT = CWD(3,10,1,5) + CWD(3,10,2,5)
      TOTSUR = FLIVE(1) + FLIVE(2) + SMALL2 + LARGE2 + TOTDUF + TOTLIT

C     ACCUMULATE SNAG INFORMATION

      TOTSNG(1) = 0.0
      TOTSNG(2) = 0.0
      DO I = 1, NSNAG
         IF ((DENIH(I)+DENIS(I)) .GT. 0.0) THEN

            SNVIS = 0.0
            SNVIH = 0.0

            IF (DENIH(I) .GT. 0.0) THEN
               CALL FMSVOL (I, HTIH(I), SNVIH, DEBUG, JOSTND)
               SNVIH = SNVIH*DENIH(I)
            ENDIF

            IF (DENIS(I) .GT. 0.0) THEN
               CALL FMSVOL (I, HTIS(I), SNVIS, DEBUG, JOSTND)
               SNVIS = SNVIS*DENIS(I)
            ENDIF

            KSP = SPS(I)

            IF (DBHS(I) .LE. 3.0) THEN
               TOTSNG(1) = TOTSNG(1) + (SNVIS+SNVIH) * V2T(KSP)
            ELSE
               TOTSNG(2) = TOTSNG(2) + (SNVIS+SNVIH) * V2T(KSP)
            ENDIF

            IF (DEBUG) WRITE(JOSTND,50) I,DENIH(I),HTIH(I),SNVIH,
     >         DENIS(I),HTIS(I),SNVIS,KSP,V2T(KSP),TOTSNG(1),TOTSNG(2)
 50         FORMAT(' FMDOUT: I=',I5,' DENIH,HTIH,SNVIH=',3F10.3,
     >           ' DENIS,HTIS,SNVIS=',3F10.3,' KSP=',I3/T18,'V2T=',F6.3,
     >           ' TOTSNG(1&2)=',2F10.4)

         ENDIF
      ENDDO

C     ADD SNAG CROWNS TO THE APPROPRIATE SIZE CLASS
C     INCLUDE LITTER IN THE 0-3 CLASS, JUST SO THAT IT IS REPORTED

      DO ISZ = 0,3
         DO IDC = 1,4
            DO ITM = 1,TFMAX
               TOTSNG(1) = TOTSNG(1) + P2T *
     &              (CWD2B(IDC,ISZ,ITM) + CWD2B2(IDC,ISZ,ITM))
               IF (ISZ .GT. 0 .AND. ISZ .LT. 3)
     &              TOTSNG(2) = TOTSNG(2) + P2T *
     &              (CWD2B(IDC,ISZ+3,ITM) + CWD2B2(IDC,ISZ+3,ITM))

               IF (DEBUG)WRITE(JOSTND,55)ISZ,IDC,ITM,CWD2B(IDC,ISZ,ITM),
     >              CWD2B2(IDC,ISZ,ITM),TOTSNG(1),TOTSNG(2)
 55            FORMAT(' FMDOUT: ISZ,IDC,ITM=',3I3,' CWD2B=',F10.2,
     >              ' CWD2B2=',F10.2,' TOTSNG(1&2)=',2F10.4)

            ENDDO
         ENDDO
      ENDDO

C     GET AREA DISTRIBUTION FOR LANDSCAPE REPORT

      IF (JLOUT(1) .GT. 0) THEN

         ACRES = 0.0
Cppe     CALL SPLAAR (ISTD, ACRES, IRC)

         ITON = INT((SMALL2/10) + 1)
         IF (ITON .GT. 4) ITON = 4
         FUAREA(1,ITON) = FUAREA(1,ITON) + ACRES

         ITON = INT((LARGE2/10) + 1)
         IF (ITON .GT. 4) ITON = 4
         FUAREA(2,ITON) = FUAREA(2,ITON) + ACRES

         ITON = INT(((SMALL2+LARGE2)/10) + 1)
         IF (ITON .GT. 4) ITON = 4
         FUAREA(3,ITON) = FUAREA(3,ITON) + ACRES

         ITON = INT(((CWD(3,11,1,5)+CWD(3,11,2,5))/10) + 1)
         IF (ITON .GT. 4) ITON = 4
         FUAREA(4,ITON) = FUAREA(4,ITON) + ACRES

         ITON = INT(((TOTSNG(1)+TOTSNG(2))/10) + 1)
         IF (ITON .GT. 4) ITON = 4
         FUAREA(5,ITON) = FUAREA(5,ITON) + ACRES
      ENDIF

C     ACCUMULATE LIVE TREE INFORMATION

      TOTLIV(1) = 0.0
      TOTLIV(2) = 0.0
      TOTFOL = 0.0
      TOTCON = 0.0

      DO I = 1,ITRN

         TOTFOL = TOTFOL + CROWNW(I,0) * FMPROB(I) * P2T

C        PUT CROWN INFO INTO THE RELATED SIZE CLASSES TOO

         DO J=1,3
            TOTLIV(1) = TOTLIV(1) + (CROWNW(I,J) + OLDCRW(I,J))
     &           * P2T * FMPROB(I)
            IF (J .LT. 3) TOTLIV(2) = TOTLIV(2) + P2T * FMPROB(I) *
     &           (CROWNW(I,J+3) + OLDCRW(I,J+3))
         ENDDO

C     GET THE VOLUME OF THE FM TREES.
C     ****CHANGED MARCH 5\99 AND USES FMSVOL (FROM FMSVL2) SO THAT BASE MODE
C         VOLUME CALL IS ONLY IN ONE PLACE*** (SB)

         JS = ISP(I)
         D = DBH(I)
         H = HT(I)
         X = -1.0
         LMERCH = .FALSE.

         CALL FMSVL2(JS,D,H,X,VT,LMERCH,DEBUG,JOSTND)

         IF (DEBUG) WRITE(JOSTND,60) I,FMPROB(I),ISP(I),D,H,VT
 60      FORMAT(' FMDOUT (LIVE): I=',I5,' FMPROB=',F10.3,' ISP=',I3,
     >           ' D,H,VT=',3F10.3)

         IF (D .LE. 3.0) THEN
            TOTLIV(1) = TOTLIV(1) + FMPROB(I) * VT * V2T(JS)
         ELSE
            TOTLIV(2) = TOTLIV(2) + FMPROB(I) * VT * V2T(JS)
         ENDIF
      ENDDO

      TOTSTD = TOTLIV(1) + TOTLIV(2) + TOTFOL + TOTSNG(1) + TOTSNG(2)
      TOTFUL = TOTSTD + TOTSUR

C     ADD UP THE TOTAL AMOUNT CONSUMED

      DO II=1,MXFLCL
         TOTCON = TOTCON + BURNED(3,II)
      ENDDO
      TOTCON = TOTCON + BURNLV(1) + BURNLV(2) + BURNCR
C
C     ADD VOLUME REMOVED BY SALVAGE, HARVEST, AND CWD TRANSFER.
C     EXPRESSED AS TONS
C
      TONREM = TONRMS + TONRMH + TONRMC

C     ACCUMULATE OUTPUT VARIABLES INTO THOSE NEEDED FOR THE
C     CARBON REPORT

      BIOLIVE = TOTFOL + TOTLIV(1) + TOTLIV(2)
      BIOSNAG = TOTSNG(1) + TOTSNG(2)
      BIODDW = SMALL2 + LARGE2
      BIOFLR = TOTLIT + TOTDUF
      BIOSHRB = FLIVE(1) + FLIVE(2)
      BIOREM(1) = BIOREM(1) + TONRMS + TONRMC
      BIOREM(2) = TONREM
      BIOCON(1) = BURNED(3,10) + BURNED(3,11)
      BIOCON(2) = TOTCON - BIOCON(1)
      
      TONRMS = 0.
      TONRMH = 0.
      TONRMC = 0.

C     CALCULATE DOWN WOOD VOLUME AND COVER FOR REPORT AND EVENT MONITOR
C     FIRST CLEAR THE ARRAYS
      DO I = 1, 3
         DO J = 1, 10
            DO K = 1, 2
               DO L = 1, 5
                  CWDVOL(I,J,K,L) = 0
                  CWDCOV(I,J,K,L) = 0
               ENDDO
            ENDDO
         ENDDO
      ENDDO

C     FILL IN DENSITY VALUES TO USE IN DOWN WOOD VOLUME CALCULATIONS. 
C     CWDDEN ARRAY HOLDS VALUES IN LBS/CUFT.  CURRENT VALUES ARE BASED
C     ON SPECIFIC GRAVITY VALUE OF 0.4 FOR BOTH SOFT AND HARD MATERIAL,
C     FROM BROWN (1974), GTR-INT-16, HANDBOOK FOR INVENTORYING DOWNED WOODY MATERIAL

      DO L = 1, 4
         CWDDEN(1,L) = 18.72  ! lbs/cuft for soft down wood (SG = 0.3)
         CWDDEN(2,L) = 24.96  ! lbs/cuft for hard down wood (SG = 0.4)
      ENDDO

      DO I = 1, 2
         DO J = 1, 9
            DO K = 1, 2
               DO L = 1, 4
                  CWDVOL(I,J,K,L) = CWD(I,J,K,L)*2000/CWDDEN(K,L)                
               ENDDO
            ENDDO
         ENDDO
      ENDDO
      
C                         total stuff up 
      DO I = 1, 2
         DO J = 1, 9
            DO K = 1, 2
               DO L = 1, 4             
                 CWDVOL(3,J,K,L) = CWDVOL(3,J,K,L) + CWDVOL(I,J,K,L) 
               ENDDO
            ENDDO
         ENDDO
      ENDDO

      DO I = 1, 3
         DO J = 1, 9
            DO K = 1, 2
               DO L = 1, 4             
                 CWDVOL(I,J,K,5) = CWDVOL(I,J,K,5) + CWDVOL(I,J,K,L) 
               ENDDO
            ENDDO
         ENDDO
      ENDDO

      DO I = 1, 3
         DO J = 1, 9
            DO K = 1, 2
               DO L = 1, 5             
                 CWDVOL(I,10,K,L) = CWDVOL(I,10,K,L) + CWDVOL(I,J,K,L) 
               ENDDO
            ENDDO
         ENDDO
      ENDDO

      DO I = 3,3
         DO J = 1, 9
            DO K = 1, 2
               DO L = 5,5                  
                  SELECT CASE (J)
                    CASE (1,2,3)
                      CWDCOV(I,J,K,L) = 0
                    CASE (4)
                      CWDCOV(I,J,K,L) = 0.0166*CWDVOL(I,J,K,L)**0.8715
                    CASE (5)
                      CWDCOV(I,J,K,L) = 0.0092*CWDVOL(I,J,K,L)**0.8795                 
                    CASE (6)
                      CWDCOV(I,J,K,L) = 0.0063*CWDVOL(I,J,K,L)**0.8728
                    CASE (7)
                      CWDCOV(I,J,K,L) = 0.0069*CWDVOL(I,J,K,L)**0.8134
                    CASE (8)
                      CWDCOV(I,J,K,L) = 0.0033*CWDVOL(I,J,K,L)**0.8617
                    CASE (9)
                      CWDCOV(I,J,K,L) = 0.0949*CWDVOL(I,J,K,L)**0.5
                  END SELECT
                  CWDCOV(I,10,K,L) = CWDCOV(I,10,K,L) + CWDCOV(I,J,K,L)
               ENDDO
            ENDDO
         ENDDO
      ENDDO
      
C     STOP HERE IF WE ARE NOT PRINTING THE FUELS REPORT
      IF (.NOT. LPRINT) GOTO 750
C
C     CALL THE DBS MODULE TO OUTPUT FUEL DATA TO A DATABASE
C
      DBSKODE = 1
      CALL DBSFUELS(IYR,NPLT,TOTLIT,TOTDUF,SMALL2,LARGE2,
     &    (CWD(3,4,1,5)+CWD(3,4,2,5)),(CWD(3,5,1,5)+CWD(3,5,2,5)),
     &    LARGE12,FLIVE(1),FLIVE(2),TOTSUR,
     &    TOTSNG(1),TOTSNG(2),TOTFOL,TOTLIV(1),NINT(TOTLIV(2)),
     &    NINT(TOTSTD),NINT(TOTFUL),NINT(TOTCON),NINT(TONREM),DBSKODE)
      IF(DBSKODE.EQ.0) GOTO 750

C     IF HEADER REQUESTED AND THIS IS THE FIRST OPPORTUNITY TO PRINT
C     IT, THEN DO SO.

      IFAPAS = IFAPAS + 1
      IF (IFAPAS .EQ. 1) THEN
         WRITE(JROUT,699) IDFLAL,IDFLAL
         WRITE(JROUT,700) IDFLAL
         WRITE(JROUT,701) IDFLAL
         WRITE(JROUT,702) IDFLAL
         WRITE (JROUT,44) IDFLAL,NPLT,MGMID
         WRITE(JROUT,700) IDFLAL
         WRITE(JROUT,703) IDFLAL
         WRITE(JROUT,704) IDFLAL
         WRITE(JROUT,705) IDFLAL
         WRITE(JROUT,710) IDFLAL
         WRITE(JROUT,715) IDFLAL
         WRITE(JROUT,720) IDFLAL
         WRITE(JROUT,700) IDFLAL
  699    FORMAT(2(/1X,I5))
  700    FORMAT(1X,I5,1X,122('-'))
  701    FORMAT(1X,I5,1X,42X,'******  FIRE MODEL VERSION 1.0 ******')
  702    FORMAT(1X,I5,1X,52X,'ALL FUELS REPORT '
     &                       '(BASED ON STOCKABLE AREA)')
   44    FORMAT(1X,I5,' STAND ID: ',A26,4X,'MGMT ID: ',A4)
  703    FORMAT(1X,I5,1X,52X,'ESTIMATED FUEL LOADINGS')
  704    FORMAT(1X,I5,22X,'SURFACE FUEL (TONS/ACRE) ',
     &          26X,'STANDING WOOD (TONS/ACRE)')
  705    FORMAT(I5,7X,59('-'),2X,35('-'))
  710    FORMAT(1X,I5,21X,'DEAD FUEL ',21X,
     &       'LIVE',15X,'DEAD',12X,'LIVE')
  715    FORMAT(1X,I5,1X,5X,39('-'),
     &       '--  ---------- SURF   -----------   ---------------',
     &       '        TOTAL TOTAL BIOMASS')
  720    FORMAT(1X,I5,1X,'YEAR LITT.  DUFF  0-3"   >3"  3-6" ',
     &    '6-12"  >12"  HERB SHRUB TOTAL   0-3"   >3"   FOL  0-3" ',
     &    '  >3" TOTAL BIOMASS CONS REMOVED')
      ENDIF
C
      WRITE(JROUT,730) IDFLAL,IYR,TOTLIT,TOTDUF,
     &  SMALL2,LARGE2,
     &  (CWD(3,4,1,5)+CWD(3,4,2,5)),(CWD(3,5,1,5)+CWD(3,5,2,5)),
     &  LARGE12,(FLIVE(I),I=1,2),TOTSUR,
     &  TOTSNG(1),TOTSNG(2),TOTFOL,TOTLIV(1),NINT(TOTLIV(2)),
     &  NINT(TOTSTD),NINT(TOTFUL),NINT(TOTCON),
     &  NINT(TONREM)

C
  730 FORMAT(1X,I5,1X,I4,1X,F5.2,1X,6(F5.1,1X),2(F5.2,1X),
     &       F5.1,1X,F6.2,1X,3(F5.1,1X),I5,1X,I5,1X,I5,I6,1X,I6)
C
  750 CONTINUE
C     STOP HERE IF WE ARE NOT PRINTING THE DOWN WOOD VOLUME REPORT
      IF (.NOT. LPRINT2) GOTO 850
C
C     CALL THE DBS MODULE TO OUTPUT DOWN WOOD VOLUME REPORT TO A DATABASE
C
      DBSKODE = 1
      DO I = 1,16
        V1(I) = 0.0
      ENDDO

      V1(1) = CWDVOL(3,1,2,5)+CWDVOL(3,2,2,5)+CWDVOL(3,3,2,5)
      V1(2) = CWDVOL(3,4,2,5)
      V1(3) = CWDVOL(3,5,2,5)
      V1(4) = CWDVOL(3,6,2,5)
      V1(5) = CWDVOL(3,7,2,5)
      V1(6) = CWDVOL(3,8,2,5)
      V1(7) = CWDVOL(3,9,2,5)
      V1(8) = CWDVOL(3,10,2,5)
      V1(9) = CWDVOL(3,1,1,5)+CWDVOL(3,2,1,5)+CWDVOL(3,3,1,5)
      V1(10) = CWDVOL(3,4,1,5)
      V1(11) = CWDVOL(3,5,1,5)    
      V1(12) = CWDVOL(3,6,1,5)
      V1(13) = CWDVOL(3,7,1,5)                                         
      V1(14) = CWDVOL(3,8,1,5)
      V1(15) = CWDVOL(3,9,1,5)
      V1(16) = CWDVOL(3,10,1,5)  

      CALL DBSFMDWVOL(IYR,NPLT,V1,16,DBSKODE)
      IF(DBSKODE.EQ.0) GOTO 850

C     IF HEADER REQUESTED AND THIS IS THE FIRST OPPORTUNITY TO PRINT
C     IT, THEN DO SO.

      IDWPAS = IDWPAS + 1
      IF (IDWPAS .EQ. 1) THEN
         WRITE(JROUT,799) IDDWRP,IDDWRP
         WRITE(JROUT,800) IDDWRP
         WRITE(JROUT,801) IDDWRP
         WRITE(JROUT,802) IDDWRP
         WRITE (JROUT,44) IDDWRP,NPLT,MGMID
         WRITE(JROUT,800) IDDWRP
         WRITE(JROUT,803) IDDWRP
         WRITE(JROUT,804) IDDWRP
         WRITE(JROUT,805) IDDWRP
         WRITE(JROUT,820) IDDWRP
         WRITE(JROUT,800) IDDWRP
  799    FORMAT(2(/1X,I5))
  800    FORMAT(1X,I5,1X,134('-'))
  801    FORMAT(1X,I5,1X,42X,'******  FIRE MODEL VERSION 1.0 ******')
  802    FORMAT(1X,I5,1X,46X,'DOWN DEAD WOOD VOLUME REPORT '
     &                       '(BASED ON STOCKABLE AREA)')
C   44    FORMAT(1X,I5,' STAND ID: ',A26,4X,'MGMT ID: ',A4)
  803    FORMAT(1X,I5,1X,30X,'ESTIMATED DOWN WOOD VOLUME (CUFT/ACRE)',
     &                       ' BY SIZE CLASS (INCHES)')
  804    FORMAT(1X,I5,34X,'HARD ',61X,'SOFT')
  805    FORMAT(I5,8X,62('-'),4X,62('-'))
  820    FORMAT(1X,I5,1X,'YEAR    0-3    3-6   6-12   12-20   20-35',
     &    '   35-50     >=50      TOT       0-3    3-6   6-12   12-20',
     &    '   20-35   35-50     >=50      TOT ')
      ENDIF
C
      WRITE(JROUT,830) IDDWRP,IYR,(NINT(V1(I)),I=1,16)
  830 FORMAT(1X,I5,1X,I4,1X,3(I6,1X),3(I7,1X),2(I8,1X),
     &       3X, 3(I6,1X),3(I7,1X),2(I8,1X))

C
  850 CONTINUE
C     STOP HERE IF WE ARE NOT PRINTING THE DOWN WOOD COVER REPORT
      IF (.NOT. LPRINT3) RETURN
C
C     CALL THE DBS MODULE TO OUTPUT DOWN WOOD COVER REPORT TO A DATABASE
C
      DBSKODE = 1
      DO I = 1,14
        V2(I) = 0.0
      ENDDO
      V2(1) = CWDCOV(3,4,2,5)
      V2(2) = CWDCOV(3,5,2,5)
      V2(3) = CWDCOV(3,6,2,5)
      V2(4) = CWDCOV(3,7,2,5)
      V2(5) = CWDCOV(3,8,2,5)
      V2(6) = CWDCOV(3,9,2,5)
      V2(7) = CWDCOV(3,10,2,5)
      V2(8) = CWDCOV(3,4,1,5)
      V2(9) = CWDCOV(3,5,1,5)    
      V2(10) = CWDCOV(3,6,1,5)
      V2(11) = CWDCOV(3,7,1,5)                                         
      V2(12) = CWDCOV(3,8,1,5)
      V2(13) = CWDCOV(3,9,1,5)
      V2(14) = CWDCOV(3,10,1,5)         

      CALL DBSFMDWCOV(IYR,NPLT,V2,14,DBSKODE)
      IF(DBSKODE.EQ.0) RETURN

C     IF HEADER REQUESTED AND THIS IS THE FIRST OPPORTUNITY TO PRINT
C     IT, THEN DO SO.

      IDCPAS = IDCPAS + 1
      IF (IDCPAS .EQ. 1) THEN
         WRITE(JROUT,899) IDDWCV,IDDWCV
         WRITE(JROUT,900) IDDWCV
         WRITE(JROUT,901) IDDWCV
         WRITE(JROUT,902) IDDWCV
         WRITE (JROUT,44) IDDWCV,NPLT,MGMID
         WRITE(JROUT,900) IDDWCV
         WRITE(JROUT,903) IDDWCV
         WRITE(JROUT,904) IDDWCV
         WRITE(JROUT,905) IDDWCV
         WRITE(JROUT,920) IDDWCV
         WRITE(JROUT,900) IDDWCV
  899    FORMAT(2(/1X,I5))
  900    FORMAT(1X,I5,1X,122('-'))
  901    FORMAT(1X,I5,1X,42X,'******  FIRE MODEL VERSION 1.0 ******')
  902    FORMAT(1X,I5,1X,46X,'DOWN DEAD WOOD COVER REPORT '
     &                       '(BASED ON STOCKABLE AREA)')
C   44    FORMAT(1X,I5,' STAND ID: ',A26,4X,'MGMT ID: ',A4)
  903    FORMAT(1X,I5,1X,30X,'ESTIMATED DOWN WOOD PERCENT COVER (%)',
     &                       ' BY SIZE CLASS (INCHES)')
  904    FORMAT(1X,I5,31X,'HARD ',53X,'SOFT')
  905    FORMAT(I5,8X,56('-'),2X,58('-'))
  920    FORMAT(1X,I5,1X,'YEAR    3-6    6-12   12-20   20-35   35-50',
     &    '    >=50     TOT       3-6    6-12   12-20   20-35   35-50',
     &    '    >=50     TOT ')
      ENDIF
C
      WRITE(JROUT,930) IDDWCV,IYR,(V2(I),I=1,14)
  930 FORMAT(1X,I5,1X,I4,1X,7(F6.1,2X),2X,7(F6.1,2X))

      RETURN
      END


