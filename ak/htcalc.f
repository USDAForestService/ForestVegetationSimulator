      SUBROUTINE HTCALC(I,ISPC,XSITE,POTHTG)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C   THIS SUBROUTINE COMPUTES THE HEIGHT INCREMENT GIVEN TREE-SPECIFIC
C   INDEPENDENT VARIABLES SUCH AS DBH, DG AGE ...
C   CALLED FROM **HTGF**
C----------
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'VARCOM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
COMMONS
C------------
C  VARIABLE DECLARATIONS:
C----------
C
      LOGICAL DEBUG
C
      INTEGER I,ISPC,ISPEC,IWHO,I1,PI2,RDZ1
C
      REAL POTHTG,XSITE,RDZ,ZRD(MAXPLT),CRAT,ELEVATN
      REAL DLO,DHI,SDIC,SDIC2,A,B,AX,BX,CX,DX,EX,FX,PBAL
      REAL GX,HX,IX,AX1,BX1,CX1,DX1,EX1,FX1,GX1,HX1,IX1
C-----------
C  SEE IF WE NEED TO DO SOME DEBUG. 
C-----------
      CALL DBCHK (DEBUG,'HTCALC',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,3)ICYC
    3 FORMAT(' ENTERING SUBROUTINE HTCALC  CYCLE =',I5)
C
C  COMPUTE RELATIVE DENSITY (ZEIDI) FOR INDIVIDUAL POINTS.
C  ALL SPECIES AND ALL SIZES INCLUDED FOR THIS CALCULATION.
C 
      DLO = 0.0
      DHI = 500.0
      ISPEC = 0
      IWHO = 1
      PI2=INT(PI)
      DO I1 = I, PI2
         CALL SDICLS (ISPEC,DLO,DHI,IWHO,SDIC,SDIC2,A,B,I1)
         ZRD(I1) = SDIC2                                                          
      END DO 
C
      POTHTG = 0.
      SELECT CASE(ISPC)
C----------
C  CALCULATE HIEGHT GROWTH FOR BOREAL SPECIES 
C----------
      CASE(4:7,13,16:23)
        IF(.NOT.LPERM) THEN
        IF((HT(I) - 4.5) .LE. 0.0)GO TO 900
        PBAL=PTBALT(I)
        RDZ1=ITRE(I)
        RDZ=(ZRD(RDZ1))/SDIMAX
        CRAT=ICR(I)
        ELEVATN=ELEV*100
        AX=NOPERMH1(I)
        BX=NOPERMH2(I)
        CX=NOPERMH3(I)
        DX=NOPERMH4(I)
        EX=NOPERMH5(I)
        FX=NOPERMH6(I)
        GX=NOPERMH7(I)
        HX=NOPERMH8(I)
        IX=NOPERMH9(I)
        POTHTG=EXP(AX + BX*(HT(I))**2 + CX*LOG(HT(I)) + DX*PBAL + 
     &         EX*LOG(RDZ) + FX*LOG(CRAT) + GX*ELEVATN +
     &         HX*RDZ + IX*LOG(XSITE))*FINT
        ENDIF
        IF(LPERM) THEN
        IF((HT(I) - 4.5) .LE. 0.0)GO TO 900
        PBAL=PTBALT(I)
        RDZ1=ITRE(I)
        RDZ=(ZRD(RDZ1))/SDIMAX
        CRAT=ICR(I)
        ELEVATN=ELEV*100
        AX1=PERMH1(I)
        BX1=PERMH2(I)
        CX1=PERMH3(I)
        DX1=PERMH4(I)
        EX1=PERMH5(I)
        FX1=PERMH6(I)
        GX1=PERMH7(I)
        HX1=PERMH8(I)
        IX1=PERMH9(I)
        POTHTG=EXP(AX1 + BX1 +CX1*(HT(I))**2 + DX1*LOG(HT(I)) +  
     &  EX1*PBAL + FX1*LOG(RDZ) + GX1*LOG(CRAT) + 
     &  HX1*ELEVATN + IX1*RDZ)*FINT
        ENDIF
C----------
C  CALCULATE HIEGHT GROWTH FOR COASTAL SPECIES 
C----------  
       CASE(1:3,8:12,14,15)
         IF((HT(I) - 4.5) .LE. 0.0)GO TO 900
          PBAL=PTBALT(I)
          RDZ1=ITRE(I)
          RDZ=(ZRD(RDZ1))/SDIMAX
          CRAT=ICR(I)
          ELEVATN=ELEV*100
          AX=NOPERMH1(I)
          BX=NOPERMH2(I)
          CX=NOPERMH3(I)
          DX=NOPERMH4(I)
          EX=NOPERMH5(I)
          FX=NOPERMH6(I)
          GX=NOPERMH7(I)
          HX=NOPERMH8(I)
          IX=NOPERMH9(I)
          POTHTG=EXP(AX + BX*(HT(I))**2 + CX*LOG(HT(I)) + DX*PBAL + 
     &           EX*LOG(RDZ) + FX*LOG(CRAT) + GX*ELEVATN +
     &           HX*RDZ + LOG(XSITE))*FINT
C----------
C  SPACE FOR OTHER SPECIES
C---------
      CASE DEFAULT
        POTHTG = 0.
C
      END SELECT
C
  900 CONTINUE
C
      RETURN
      END     