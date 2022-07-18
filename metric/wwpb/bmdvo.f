      SUBROUTINE BMDVO (I,IYEAR)
C----------
C  **BMDVO-METRIC  DATE OF LAST REVISION:  JUNE 27, 1994
C----------
C
c     Westwide Pine Beetle model; 
C         Prints the output relating to driving variables (fast kills,
c     special trees and individual rvs). Output is printed as cycle
c     averages for each stand and for the landscape.
c     
C     A TABLE OF RESULTS IS OUTPUT AT THE CONCLUSION OF EACH CYCLE.
C
C     CALLED FROM -- BMDRV
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77' 
      INCLUDE 'PPCNTL.F77'
      
      INCLUDE 'BMPRM.F77'
      INCLUDE 'BMCOM.F77'
      INCLUDE 'BMPCOM.F77'

      INCLUDE 'METRIC.F77'
      
      INTEGER ryear,ryear2, RYEARP
      
      IF (.NOT. LBMDVO) RETURN
      
c     ryear triggers the reporting. It is the last year of a cycle.
C     The average values are reported for the first year of the 
C     cycle just finished. e.g: for and outbreak from 1993-1994
C     during the 1990-1994 period, the reporting would be for 1990
C     and would be the average of 1993 and 1994.

      ryear = miy(micyc) - 1
      ryear2= miy(micyc - 1)
      
C
C     New local variable RYEARP to identify output table values as
C     end of cycle values (RNH June98)
C
      RYEARP= MIY(MICYC)
C
c     Accumulate sums for each variable

      STFK(I) = STFK(I) + FASTK(I,1) / ACRtoHA
      SVFK(I) = SVFK(I) + FASTK(I,2) * FT3pACRtoM3pHA
      SBAFK(I) = SBAFK(I) + FASTK(I,3) * FT2pACRtoM2pHA
      IF (.NOT. LBMVOL .AND. .NOT. LBMDET) 
     &      SRVSTD(I) = SRVSTD(I) + GRFSTD(I)

      DO 10 ISIZ= 1,NSCL              
         SSPEC(I) = SSPEC(I) + SPCLT(I,ISIZ,1) * TREE(I,ISIZ,1)/ACRtoHA
   10 CONTINUE                                                 
       
      DO 15 IDV= 1,NUMRV              
         SDVRV(I,IDV) = SDVRV(I,IDV) + DVRV(I,IDV)         
   15 CONTINUE                                                 
      
C     Zero out the variable holding the fast kills (since it is added to
c     during calculation in BMMORT

      FASTK(I,1) = 0.0
      FASTK(I,2) = 0.0
      FASTK(I,3) = 0.0

c     Print state information averaged within the master cycle.
       
      IF (IYEAR .EQ. ryear) THEN
        IF (ibmmrt .LE. 0) GOTO 85
      
        X = 1.0 / FLOAT(ibmmrt)     

        STFK(I) = STFK(I) * X
        SVFK(I) = SVFK(I) * X
        SBAFK(I) = SBAFK(I) * X
        SSPEC(I) = SSPEC(I) * X
        IF (.NOT. LBMVOL .AND. .NOT. LBMDET) SRVSTD(I) = SRVSTD(I) * X
       
        DO 20 IDV= 1,NUMRV              
           SDVRV(I,IDV) = SDVRV(I,IDV) * X
   20   CONTINUE                                                 

c       Write the stand averages to the driving variables output file
C
C     Changed RYEAR2 to RYEARP to specify end rather than beginning
C     of cycle in output table (RNH June98),
C
C        WRITE(JBMDV,60) ryear2, bmstds(i), STFK(I),SVFK(I),SBAFK(I),
       WRITE(JBMDV,60) ryearp, bmstds(i), STFK(I),SVFK(I),SBAFK(I),
     &     SSPEC(I), SRVSTD(I),(SDVRV(I,IDV),IDV=1,NUMRV)
   60   FORMAT (1X,I4,1X,A8,1X,F8.2,F9.2,F9.2,F8.2,F5.2,1X,9(F5.2))

C...RVDNST added to above output table 7/1/99 (AJM)
C...Due to the complexity of getting the new RVDNST variable into the
C   SDVRV array, it is merely tacked on at the end of the write statement
C   and the original SDVRV array values snet to output are reduced by one
C   (the last value in the array is the RV due to stand density effects)
C
c       Prepare landscape averages.

C        IF (I .EQ. 1) SACRES = 0.0

        CALL SPLAAR (I, ACRES, IRC)
        IF (acres .LE. 0.0) ACRES = 1.0

C        Change ACRES to hold HA instead of Acres
         ACRES = ACRES * ACRtoHA

C        SACRES = SACRES + ACRES

        LTFK = LTFK + STFK(I) * ACRES
        LVFK = LVFK + SVFK(I) * ACRES
        LBAFK = LBAFK + SBAFK(I) * ACRES
        LSPEC = LSPEC + SSPEC(I) * ACRES
        IF (.NOT. LBMVOL .AND. .NOT. LBMDET)
     &       LRVSTD = LRVSTD + SRVSTD(I) * ACRES

        DO 30 IDV= 1,NUMRV
           LDVRV(IDV) = LDVRV(IDV) + SDVRV(I,IDV) * ACRES
   30   CONTINUE

C     KLUDGE TO GET AROUND COUNTING OF NONSTOCKABLE STANDS.

        IF (ICNT .GE. BMEND) ICNT = 0
        ICNT = ICNT + 1


        IF (ICNT .GE. BMEND) THEN

C        NOTE: SACRES is added up elsewhere, and already is in HA
      
          Y = 1.0 / SACRES

          LTFK = LTFK * Y
          LVFK = LVFK * Y
          LBAFK = LBAFK * Y
          LSPEC = LSPEC * Y
          IF (.NOT. LBMVOL .AND. .NOT. LBMDET) LRVSTD = LRVSTD * Y

          DO 40 IDV= 1,NUMRV
             LDVRV(IDV) = LDVRV(IDV) * Y
   40     CONTINUE
C
C     Changed RYEAR2 to RYEARP to specify end rather than beginning
C     of cycle in output table (RNH June98), next 2 write statements
C
C          WRITE (JBMDV,90) RYEAR2,'LANDSCAPE',LTFK,LVFK,LBAFK,
          WRITE (JBMDV,90) RYEARp,'LANDSCAPE',LTFK,LVFK,LBAFK,
     >    LSPEC, LRVSTD, (LDVRV(IDV), IDV=1,NUMRV)
   90     FORMAT (1X,I4,1X,A9,F8.2,F9.2,F9.2,F8.2,F5.2,1X,9(F5.2))

C
c     Zero out all summary variables after printing.

          LTFK = 0.0
          LVFK = 0.0
          LBAFK = 0.0
          LSPEC = 0.0
          LRVSTD = 0.0

          DO 70 IDV= 1,NUMRV
            LDVRV(IDV) = 0.0
   70     CONTINUE
        ENDIF

        STFK(I) = 0.0
        SVFK(I) = 0.0
        SBAFK(I) = 0.0
        SSPEC(I) = 0.0
        SRVSTD(I) = 0.0

        DO 71 IDV= 1,NUMRV
          SDVRV(I,IDV) = 0.0
   71   CONTINUE

      END IF
              
   85 CONTINUE           
              
      RETURN
      END
