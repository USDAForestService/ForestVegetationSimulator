      SUBROUTINE BENTYUPDT(FVS_CYC,CV_THIN,FVS_TRN,FVS_DBH,FVS_HT,
     $                    FVS_CR,FVS_PROB,FVS_ISP,FVS_JSP,FVS_IDTREE,
     $                    CV_CLOW,CV_CMED,CV_CTALL,MAXCY1)
C----------
C  **BENTYUPDT  DATE OF LAST REVISION JULY 2003, AJM.  INSERTING NEW METHOD
C FOR CACLULATING BD.  ADDING NEW VARIALBE "A_"; THIS IS THE PARAMETER "a" IN
C THE EQUATION FOR A PARABOLA : X^2 = 4ay.  EACH TREE HAS ITS OWN "a".

CCC              BGC--DATE OF LAST REVISION: 3/13/00
C----------
C
C     ADDS NEW TREE RECORDS FROM FVS TO THOSE PASSED FROM STAND-BGC.
C     EXISTING TREE RECORDS ONLY HAVE PROB UPDATED. ONLY
C     CALLED IF USER HAS SPECIFIED BGC INCREMENTS AND IF AT LEAST ONE
C     FVS CYCLE HAS BEEN COMPLETED.
C
C     CALLED FROM: BGCGROW
C
COMMONS
      INCLUDE 'ENTITY.F77'
      INCLUDE 'SITE.F77'
COMMONS
      LOGICAL CV_THIN
      INTEGER FVS_ISP(*),FVS_CR(*),FVS_IDTREE(*),FVS_CYC
      INTEGER FVS_TRN,MAXCY1
      CHARACTER *4 FVS_JSP(*), SPEC
      DIMENSION FVS_PROB(*),FVS_DBH(*),FVS_HT(*),CV_THIN(*),
     $          CV_CLOW(MAXCY1,2),CV_CMED(MAXCY1,2),CV_CTALL(MAXCY1,2)


C
C-----------------------------------------------------------------------
C LOOK FOR NEW TREE RECORDS AND DEFINE ATTRIBUTES FOR BGC WHEN FOUND 
C-----------------------------------------------------------------------
      NX=NLF                 ! NLF = NO. LIVE PLANTS FROM BGC
   10 DO 50 I=1,FVS_TRN
         DO 60 J=1,NLF         
            IF(TREENO(J).EQ.FVS_IDTREE(I)) THEN
               TPH(J)=FVS_PROB(I)*2.47
               EXPAND(J)=TPH(J)
c************troubleshoot ajm 8/29/00
C	    WRITE(76,15)FVS_CYC,FVS_TRN,NLF,TREENO(J),FVS_IDTREE(I),
C     +                  FVS_PROB(I)
C   15       FORMAT(5(2X,I4),F6.2)
c************************************************
	       GOTO 50
            END IF
   60    CONTINUE
C NO MATCH WAS FOUND. DEFINE AND ADD A NEW BGC TREE
            NX=NX+1
            ID(NX)='T'
            TREENO(NX)=FVS_IDTREE(I)
            D(NX)=2.54*FVS_DBH(I)
            H(NX)=FVS_HT(I)/3.28
            TPH(NX)=2.47*FVS_PROB(I)
            EXPAND(NX)=TPH(NX)
            IK=FVS_ISP(I)
            SPP(NX)=FVS_JSP(IK)
            CR(NX)=FVS_CR(I)/100.          ! BGC wants decimal value
            SPEC=SPP(NX)
            DOB=FVS_DBH(I)
C FOR SMALL TREES WITH HT<1.3M, SET DBH=0
            IF(H(NX).LE.1.3) D(NX)=0.0
C CALCULATE BD FOR TREES > 2" FVS_DBH BY ADDING 2*BARK THICKNESS
            IF(FVS_DBH(I).GE.2.0) THEN
               CALL BARK(SPEC,DOB,BRK)
               BD(NX)=(FVS_DBH(I)+2*BRK)*2.54
            ELSE
c***************************************************************************
C COMMENTING OUT THIS METHOD.
c NEW METHOD FOR CALCULATION OF BASAL DIAMETER (BD)
C 1) ASSUME PARABOLIC SHAPE
C 2) KNOWING HT AND DBH, GET PARAMETER "a" IN EQN FOR PARABOLA X^2 = 4aY
C 3) ONCE "a" IS KNOWN, WE CAN SOLVE FOR ANY X AT ANY Y (A DIAMETER AT ANY HT)
C
C***************************************************************************
C    *****  OLD WAY   *****

C CALCULATE FROM FVS_HT FOR TREES WITH FVS_DBH < 2.0". FIRST GET HEIGHT
C AT REFERENCE DBH FROM H:D CURVE. ASSUME BD FOR REFERENCE DBH IS
C DBH + 2*BRK. STRAIGHT LINE INTERPOLATION.
C               REFD=2.0
C               CALL BARK(SPEC,REFD,BRK)
C               CALL HTDIAM(SPEC,REFD,REFHT)
C               BD(NX)=(FVS_HT(I)*(REFD+2*BRK)/REFHT)*2.54
C
C     *****  NEW WAY  *****  (ASSUMES TREE HAS A DBH!)
C
              A_(NX)=(FVS_DBH(I)/2.)**2/(4.*(FVS_HT(I)-4.5))
              BD(NX)=(4.*SQRT(A_(I)*FVS_HT(I)))*2.54 !METRIC
C     *****  *****  *****  *****
            END IF
C CALCULATE TREE BASAL AREA. USE BD FOR TREES WITH DBH < 5.08cm.
            IF(D(NX).LT.5.08) THEN
C               DIB=BD(NX)-2*BRK*2.54 ! THIS NOT NEEDED EITHER W/ OR W/O MY NEW CHANGE AJM 7/03
               BA(NX)=((BD(NX)/1.074)**2.)*0.00007854
            ELSE
               BA(NX)=D(NX)**2. * 0.00007854
            END IF
C****************************************************************************
C accumulate basal area per hectare
            TOTBA=TOTBA+BA(NX)*TPH(NX)
C INCREASE ENTITY COUNT
            NB=NB+1
c************troubleshoot ajm 8/29/00
C	    WRITE(76,45)FVS_CYC,FVS_TRN,NLF,TREENO(J),FVS_IDTREE(I),
C     +                  FVS_PROB(I)
C   45       FORMAT(5(2X,I4),F6.2)
c************************************************
   50 CONTINUE
      RETURN
      END
