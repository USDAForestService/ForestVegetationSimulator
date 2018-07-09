      SUBROUTINE TMBMAS 
      IMPLICIT NONE
C---------- 
C DFTM $Id$
C---------- 
C     
C     CALCULATES BIOMASS  OF THE NOMINAL BRANCHES     
C     
C     NOTE: TMBMAS IS CALLED BEFORE DGDRIV SIMPLY BECAUSE OPTION 2
C     REQUIRES THE PAST 10 YR GROWTH INCREMENT. IF OPTION 2 IS    
C     ELIMINATED (AS IT SHOULD BE), THIS ROUTINE MAY BE CALLED FROM     
C     TMCOUP.  ALSO NOTE THAT FBIOMS AND PCNEWF ARE BOTH RECORD TRIPLED 
C     HERE...IF THE ROUTINE CALL IS MOVED TO TMCOUP, THE RECORD   
C     TRIPLING CODE EMBEDED HERE WILL HAVE TO BE REMOVED.   
C     
C     N.L. CROOKSTON NOV 1980.
C     
C  REVISION HISTORY:
C    01-APR-2013 Lance R. David (FMSC)
C      A few variables defined locally were already defined
C      in a common block. Local declaration removed.
C----------
C
COMMONS     
C     
      INCLUDE 'PRGPRM.F77'

      INCLUDE 'ARRAYS.F77'

      INCLUDE 'CONTRL.F77'

      INCLUDE 'TMCOM1.F77'

      INCLUDE 'PLOT.F77'

      INCLUDE 'BIOMAS.F77'
C     
COMMONS     

      INTEGER IPT(MAXTRE)   
      INTEGER I,IS,IK,J,I1,I2
      REAL CR,DGI,FERROR,PERROR,TMBCHL,SCALDG

      EQUIVALENCE (IPT(1), WK6(1))  

C     
C     IF IBMTYP IS 1 3 OR 4 BRANCH TO STM 900; ELSE: USE CODE BELOW.    
C     
      IF (IBMTYP .NE. 2) GO TO 900  

C     
C     COMPUTE THE SCALING FACTOR OF DG... 
C     
      SCALDG = ICL6
      IF (ICYC .GT. 1) SCALDG = IY(ICYC) - IY(ICYC-1)  
      SCALDG = 10.0 / SCALDG    

C     
C     EQUATIONS FROM C. HATCH AND P. MIKA, UN. OF IDAHO, COL. OF  
C     FORESTRY.  SEE: 'FOLIAGE BIOMASS ESTIMATES FOR DOUGLAS-FIR, 
C     GRAND FIR AND WHITE FIR FROM SELECTED DFTM OUTBREAK AREAS IN
C     THE WESTERN U. S.'  FINAL REPORT SUBMITTED TO THE DFTM
C     PROGRAM FEB. 8, 1978.   
C     ( USED IF IBMTYP = 2 )  
C     
      IF (TMDTRE) WRITE (JODFTM,610) ITRN 
  610 FORMAT(//' DUMP FROM TMBMAS:  ITRN=',I4// 
     >'    I   IS  SLOPE    ASPECT        BA       TPROB  RELDEN',
     >'       DBH        HT        DG        CR       PCT    FBIOMS',   
     >'    PCNEWF ISP'/)

      IF (.NOT. LDF) GOTO 15  

C     
C     CALCULATE BRANCH BIOMASS FOR DOUGLAS FIR.
C
      I1 = ISCT(IDFCOD,1)
      I2 = ISCT(IDFCOD,2)

      DO 10 I = I1,I2   
        IS = IND1(I)    
        IK = IS   
        J = -2    
        CR = ICR(IK) / 100.0  
        DGI = DG(IK) * SCALDG  

    8   CONTINUE  
        FBIOMS(IS) = 195.20482 -    
     &               249.78151 * SLOPE -  
     &               99.36800  * SLOPE * COS(ASPECT) -
     &               129.92132 * SLOPE * SIN(ASPECT) +
     &               22.05280  * DBH(IK) -
     &               2.14018   * HT(IK) + 
     &               79.43490  * CR -     
     &               0.62932   * BA 

      IF (FBIOMS(IS) .GT. 400.0) FBIOMS(IS) = 400.0   
      IF (FBIOMS(IS) .LT. 91.0) FBIOMS(IS) = 91.0     

      PCNEWF(IS) = 0.49738 +  
     &             0.080570  * SLOPE +    
     &             0.27017   * SLOPE * COS(ASPECT) +  
     &             0.32162   * SLOPE * SIN(ASPECT) +  
     &             0.039360  * ALOG(PCT(IK) * (0.31830989 * 
     &             ATAN(((RELDEN / 100) - 1.5) / 1.7) + 0.5)) -   
     &             0.0043258 * HT(IK) +   
     &             0.077044  * DGI -
     &             0.078413  * TPROB / 100 +    
     &             0.0048268 * (TPROB**2 / 10000)     

      PCNEWF(IS) = PCNEWF(IS) * 100.0     

      IF (PCNEWF(IS) .GT. 42.0) PCNEWF(IS) = 42.0     
      IF (PCNEWF(IS) .LT. 11.0) PCNEWF(IS) = 11.0     

      IF (TMDTRE) WRITE(JODFTM,620) I, IS, SLOPE, ASPECT, BA, TPROB,    
     &   RELDEN, DBH(IK), HT(IK), DG(IK), CR, PCT(IK), FBIOMS(IS),
     &   PCNEWF(IS), ISP(IK)  
  620 FORMAT(2I5,F7.3,11F10.3,I3 )  

C     
C     RECORD TRIPLES:   
C     
      IF (.NOT. LTRIP) GOTO 10
      J = J + 1   
      IF (J .GT. 0) GOTO 10   
      IS = ITRN + 2 * IK + J  
      GOTO 8

   10 CONTINUE    
   15 CONTINUE    
      IF (.NOT. LGF) GOTO 25  

C     
C     CALCULATE BRANCH BIOMASS FOR GRAND FIR.
C
      I1 = ISCT(IGFCOD,1)
      I2 = ISCT(IGFCOD,2)

      DO 20 I = I1,I2   
        IS = IND1(I)    
        IK = IS   
        J = -2    
        CR = ICR(IK) / 100.0  
        DGI = DG(IK) * SCALDG  

   18   CONTINUE  
        FBIOMS(IS) = EXP(4.70244 +  
     &                   0.15833  * DBH(IK) -   
     &                   0.014290 * HT(IK) +    
     &                   0.17778  * DGI)  

        IF (FBIOMS(IS) .GT. 400.0) FBIOMS(IS) = 400.0 
        IF (FBIOMS(IS) .LT. 125.0) FBIOMS(IS) = 125.0 

        PCNEWF(IS) = 0.12690 -
     &               0.017636  * ALOG(PCT(IK) * (0.31830989 *     
     &               ATAN(((RELDEN / 100) - 1.5) / 1.7) + 0.5)) + 
     &               0.037998  * DBH(IK) -
     &               0.0062929 * HT(IK) + 
     &               0.49115   * CR -     
     &               0.59622   * SLOPE    

        PCNEWF(IS) = PCNEWF(IS) * 100.0   
        IF (PCNEWF(IS) .GT.  47.0) PCNEWF(IS) = 47.0  
        IF (PCNEWF(IS) .LT.  15.0) PCNEWF(IS) = 15.0  

        IF (TMDTRE) WRITE(JODFTM,620) I, IS, SLOPE, ASPECT, BA, TPROB,  
     &     RELDEN, DBH(IK), HT(IK), DG(IK), CR, PCT(IK), FBIOMS(IS),    
     &     PCNEWF(IS), ISP(IK)

C     
C       RECORD TRIPLES: 
C     
        IF (.NOT. LTRIP) GOTO 20    
        J = J + 1 
        IF (J .GT. 0) GOTO 20 
        IS = ITRN + 2 * IK + J
        GOTO 18   
   20 CONTINUE    

   25 CONTINUE    

      RETURN
C     
C     ****************  
C     
  900 CONTINUE    
C     
C     ****************  
C     
C     IBMTYP   METHOD   
C     
C       1      RAMDOMLY ASSIGN THE FOLIAGE BIOMASS AND %NEW 
C       3      USE HATCH'S EQUATIONS TO PREDICT BIOMASS AS A
C              FUNCTION OF BASAL AREA PERCENTILE.     
C       4      LIKE 3, EXCEPT THAT A RANDOM ERROR IS ADDED  
C     

      IF (TMDTRE) WRITE(JODFTM,630) IBMTYP, ITRN
  630 FORMAT(//' DUMP FROM TMBMAS(IBMTYP=',I4,';ITRN=',I4,'):'//  
     >'    I   IS  ISP       DBH       PCT    FBIOMS    FERROR',  
     >'    PCNEWF    PERROR'/ )     
C     
C     SINCE RANDOM PROCESSES ARE USED TO ASSIGN FOLIAGE BIOMASS,  
C     THE RESULTS OF THE CALCULATIONS ARE DEPENDENT UPON THE ORDER
C     OF THE TREE RECORDS.  TO KEEP RESULTS INDEPENDENT OF ORDER THE    
C     TREE RECORDS NEED TO BE SORTED ON A TREE ATTRIBUTE SUCH AS DBH.   
C     THIS IS A 2 STEP PROCESS:     
C     1 - LOAD IND1 INTO IPT  
C     2 - SORT IPT IN ORDER OF DBH BY SPECIES. THIS REQUIRES
C         TWO CALLS TO RDPSRT.
C     
      DO 26 I=1,ITRN    
        IPT(I) = IND1(I)
   26 CONTINUE    

      IF (IBMTYP .NE. 4) GOTO 27    
      I1 = ISCT(IDFCOD,1)
      IF (LDF) CALL RDPSRT (ISCT(IDFCOD,2)-I1+1, DBH, IPT(I1), .FALSE.)
      I1 = ISCT(IGFCOD,1)
      IF (LGF) CALL RDPSRT (ISCT(IGFCOD,2)-I1+1, DBH, IPT(I1), .FALSE.)

   27 CONTINUE    
      FERROR = 0.0
      PERROR = 0.0

      IF (.NOT. LDF) GOTO 35  

C     
C     CALCULATE BRANCH BIOMASS FOR DOUGLAS FIR.
C
      I1 = ISCT(IDFCOD,1)
      I2 = ISCT(IDFCOD,2)
      IF (IBMTYP .EQ. 1) GOTO 31

      DO 30 I = I1,I2   
        IS = IPT(I)     
        IK = IS   
        J = -2    

   28   CONTINUE  

        IF (IBMTYP .EQ. 4) FERROR = TMBCHL(0.0, 56.885)     
        IF (IBMTYP .EQ. 4) PERROR = TMBCHL(0.0, 10.970)     

        FBIOMS(IS) = 154.3661 + 1.3706 * PCT(IK) + FERROR   
        PCNEWF(IS) = 100.0 * (0.168298 + 0.002634 * PCT(IK)) + PERROR   

        IF (FBIOMS(IS) .GT. 400.0) FBIOMS(IS) = 400.0 
        IF (FBIOMS(IS) .LT. 91.0) FBIOMS(IS) = 91.0   
        IF (PCNEWF(IS) .GT. 42.0) PCNEWF(IS) = 42.0   
        IF (PCNEWF(IS) .LT. 11.0) PCNEWF(IS) = 11.0   

        IF (TMDTRE) WRITE (JODFTM,640) I, IS, ISP(IK), DBH(IK),   
     &     PCT(IK), FBIOMS(IS), FERROR, PCNEWF(IS), PERROR  
  640   FORMAT (3I5,6F10.3)   

C     
C       RECORD TRIPLES  
C     
        IF (.NOT. LTRIP) GOTO 30    
        J = J + 1 
        IF (J .GT. 0) GOTO 30 
        IS = ITRN + 2 * IK + J
        GOTO 28   

   30 CONTINUE    
      GOTO 35     

   31 CONTINUE    
      DO 33 I=I1,I2     
        IS = IPT(I)     
        IK = IS   
        J = -2    

   32   CONTINUE  
        PCNEWF(IS) = TMBCHL(DFPNEW(1), DFPNEW(2))     
        FBIOMS(IS) = TMBCHL(DFFBIO(1), DFFBIO(2))     
        IF (TMDTRE) WRITE (JODFTM,640) I, IS, ISP(IK), DBH(IK),   
     &     PCT(IK), FBIOMS(IS), FERROR, PCNEWF(IS), PERROR  

C     
C       RECORD TRIPLES  
C     
        IF (.NOT. LTRIP) GOTO 33    
        J = J + 1 
        IF (J .GT. 0) GOTO 33 
        IS = ITRN + 2 * IK + J
        GOTO 32   
   33 CONTINUE    

   35 CONTINUE    
      IF (.NOT. LGF) GOTO 45  

C     
C     CALCULATE BRANCH BIOMASS FOR GRAND FIR.
C
      I1 = ISCT(IGFCOD,1)
      I2 = ISCT(IGFCOD,2)
      IF (IBMTYP .EQ. 1) GOTO 41

      DO 40 I = I1,I2   
        IS = IPT(I)     
        IK = IS   
        J = -2    
   38   CONTINUE  

        IF (IBMTYP .EQ. 4) FERROR = TMBCHL(0.0, 57.590)     
        IF (IBMTYP .EQ. 4) PERROR = TMBCHL(0.0, 7.229)

        FBIOMS(IS) = 174.7162  + 1.3794 * PCT(IK) + FERROR  
        PCNEWF(IS) = 100.0 * (0.324830 + 0.000723 * PCT(IK)) + PERROR   

        IF (FBIOMS(IS) .GT. 400.0) FBIOMS(IS) = 400.0 
        IF (FBIOMS(IS) .LT. 125.0) FBIOMS(IS) = 125.0 
        IF (PCNEWF(IS) .GT.  47.0) PCNEWF(IS) = 47.0  
        IF (PCNEWF(IS) .LT.  15.0) PCNEWF(IS) = 15.0  

        IF (TMDTRE) WRITE (JODFTM,640) I, IS, ISP(IK), DBH(IK),   
     &     PCT(IK), FBIOMS(IS), FERROR, PCNEWF(IS), PERROR  

C     
C       RECORD TRIPLES  
C     
        IF (.NOT. LTRIP) GOTO 40    
        J = J + 1 
        IF (J .GT. 0) GOTO 40 
        IS = ITRN + 2 * IK + J
        GOTO 38   
   40 CONTINUE    
      GOTO 45     

   41 CONTINUE    
      DO 43 I=I1,I2     
        IS = IPT(I)     
        IK = IS   
        J = -2    

   42   CONTINUE  
        PCNEWF(IS) = TMBCHL(GFPNEW(1), GFPNEW(2))     
        FBIOMS(IS) = TMBCHL(GFFBIO(1), GFFBIO(2))     
        IF (TMDTRE) WRITE (JODFTM,640) I, IS, ISP(IK), DBH(IK),   
     &     PCT(IK), FBIOMS(IS), FERROR, PCNEWF(IS), PERROR  

C     
C       RECORD TRIPLES  
C     
        IF (.NOT. LTRIP) GOTO 43    
        J = J + 1 
        IF (J .GT. 0) GOTO 43 
        IS = ITRN + 2 * IK + J
        GOTO 42   
   43 CONTINUE    

   45 CONTINUE    

      RETURN
      END   
