      SUBROUTINE HT2TOPD(REGN,FORST,VOLEQ,DBHOB,HTTOT,HT1PRD,HT2PRD,
     + UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,BTR,FCLASS,
     + STEMDIB,STEMHT,ERRFLAG)

      USE CLKCOEF_MOD
      USE VOLINPUT_MOD
      IMPLICIT NONE
*  Purpose:
*     This subroutine calculates the height to a particular top diameter (insidebark)
*     for volume equation with profile model.
      INTEGER REGN,ERRFLAG,HTREF,FCLASS
      REAL DBHOB,HTTOT,STEMDIB,STEMHT
      CHARACTER*10 VOLEQ,VOLEQ2
      CHARACTER*2 FORST
      CHARACTER*3 MDL
      REAL HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2
      REAL DBTBH,BTR,VOL(15)
      CHARACTER*1 HTTYPE, VEQ, CTYPE
      
      REAL MAXLEN,MINLEN,MERCHL,MINLENT,MTOPP,MTOPS,STUMP,TRIM,MINBFD
      INTEGER EVOD,OPT
      CHARACTER*1 COR
      CHARACTER*2 PROD
      
C     Variables to hold flewellings coefficients
      INTEGER SETOPT(6),JSP,NEXTRA
      REAL RHFW(4),RFLW(6),TAPCOE(12),F,FMOD(3),PINV_Z(2)
      REAL HEX(2),dex(2),ZEX(2),UHT,MHT,LMERCH
      REAL TOP6,HT2,TOPD,HTTOTC

c     VARIABLES FOR R4 MAT
      REAL STUMPD,BUTTCF,CF0,B
      
C     Variables for Clark profile  
      INTEGER SPP, GEOG,IPROD
      REAL TOPDIB,SAWDIB,PULPDIB,SHRTHT,TOPHT,PLPDIB,DBHIB,DIB17,brokHt
      TYPE(CLKCOEF):: COEFFS,COEFFST
      TYPE(CLKCOEF):: COEFFSO
      LOGICAL SHORT    
      
      MDL = VOLEQ(4:6)
c      TOPD = STEMDIB
      STEMHT = 0.0
      PROD = '01'
      STUMP = 0.0
      HTTYPE = 'F'
      MTOPP = 0.0
      MTOPS = 0.0
*     Profile model
c     MRULES IS EXTERNAL AND CONTAINS THE MERCHANDIZING RULES SETTINGS
        CALL MRULES(REGN,FORST,VOLEQ,DBHOB,COR,EVOD,OPT,MAXLEN,MINLEN,
     >       MERCHL,MINLENT,MTOPP,MTOPS,STUMP,TRIM,BTR,DBTBH,MINBFD,
     >       PROD)
     
c     Added this for no total height(12/20/2017)     
      TOPD = MTOPP
      IF (HTTOT.LE.0)THEN
        IF (HT1PRD.GT.0)THEN
          MHT = HT1PRD + STUMP
        ELSEIF(HT2PRD.GT.0)THEN
          MHT = HT2PRD + STUMP
          TOPD = MTOPS
        ENDIF
      ENDIF
      IF(MDL.EQ.'FW2' .OR. MDL.EQ.'fw2' .OR. MDL.EQ.'FW3' .OR.
     +   MDL.EQ.'fw3' .OR. MDL.EQ.'CZ2' .OR. MDL.EQ.'cz2' .OR.
     +   MDL.EQ.'CZ3' .OR. MDL.EQ.'cz3' .OR. MDL.EQ.'WO2' .OR.     
     +   MDL.EQ.'wo2' .OR. MDL.EQ.'F32' .OR. MDL.EQ.'f32' .OR.
     +   MDL.EQ.'F33' .OR. MDL.EQ.'f33' .OR. MDL.EQ.'JB2' .OR.
     +   MDL.EQ.'jb2' .OR. MDL.EQ.'BEH' .OR. MDL.EQ.'beh') THEN

        IF (VOLEQ(4:4).EQ.'F' .OR. VOLEQ(4:4).EQ.'f') THEN
C--   Initialize Flewelling model for this tree
          
          CALL FWINIT(VOLEQ,DBHOB,HTTOT,MHT,TOPD,UPSHT1,UPSHT2,UPSD1,
     >     UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,JSP,RHFW,RFLW,
     >     TAPCOE,F,SETOPT,NEXTRA,HEX,DEX,ZEX,PINV_Z,FMOD,btr,FCLASS,
     >     ERRFLAG)
       
           AVGZ1 = ZEX(1)
           AVGZ2 = ZEX(2)
          IF(ERRFLAG .GT. 0)  GO TO 1000
	  
        ELSEIF (VOLEQ(4:6).EQ.'CZ3' .OR. VOLEQ(4:6).EQ.'cz3') THEN
C        initialize Czaplewski three point model
          IF(HTTOT.LE.4.5)THEN
	          ERRFLAG = 4
	          GOTO 1000
           ENDIF
         UHT = HTTOT * 0.95
         IF(UPSHT1.LE.0 .or. UPSD1.LE.0) THEN 
            ERRFLAG = 9
            GO TO 1000
         ENDIF
         IF(UPSHT1.LE.4.5 .or. UPSHT1.GT.UHT) THEN
            ERRFLAG = 10
            GO TO 1000
         ENDIF      
         HEX(1) = UPSHT1
         DEX(1) = UPSD1
         CALL TOP6LEN(VOLEQ,HTTOT,DBHOB,DEX,HEX,STUMP,6.0,
     >                TOP6,DBTBH,errflag)
        ELSEIF(MDL.EQ.'BEH' .OR. MDL.EQ.'beh')THEN
          IF (FCLASS.LE.0) THEN
           CALL GETFCLASS(VOLEQ,FORST,DBHOB,FCLASS)
         ENDIF
         FORMCLASS = FCLASS
        ENDIF      
        CALL MERLEN(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,
     >         DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,
     >         STUMP,STEMDIB,LMERCH,errflag)
        STEMHT = LMERCH + STUMP
C     !OTHER PROFILE MODEL
      ELSEIF (MDL.EQ.'MAT' .OR. MDL.EQ.'mat') THEN
        CALL R4MATTAPER(VOLEQ,DBHOB,HTTOT,STUMPD,BUTTCF,CF0,B,
     +  STEMHT,STEMDIB,ERRFLAG )
      ELSEIF (MDL.EQ.'CLK' .OR. MDL.EQ.'clk') THEN
        IF(REGN.EQ.9.OR.
     +     (VOLEQ(1:1).EQ.'8'.AND.VOLEQ(3:3).EQ.'1'))THEN
          IF(VOLEQ(1:1).EQ.'8')THEN
          !R8 use UPSHT1 for ht479. Need to set ht4 to HT2PRD
            IF(HTTOT.LE.0.0.AND.UPSHT1.GT.0.0)THEN
              IF(UPSD1.EQ.4.0.OR.
     &         (Prod.NE.'01'.AND.(UPSD1.NE.7.OR.UPSD1.NE.9)))THEN
                HT2PRD = UPSHT1
                UPSHT1 = 0.0
              ENDIF
            ENDIF
          call r8Prep(volEq,dbhOb,topDib,topHt,ht1Prd,ht2Prd,htTot,
     &            spp,geog,COEFFS,forst,maxLen,
     &            minLen,merchL,mTopP,mTopS,stump,trim,minBfD,
     &            prod,iProd,sawDib,plpDib,short,shrtHt,errFlag,
     &            upsHt1,COEFFSO)
           call r9totHt(COEFFS%totHt,htTot,dbhOb,COEFFSO%dib17,topHt,
     &             topDib,COEFFSO%a, COEFFSO%b,errFlag)
           COEFFSO%totHt = COEFFS%totHt

          ELSE          
          call r9Prep(volEq,dbhOb,topDib,topHt,ht1Prd,ht2Prd,htTot,
     &            spp,geog,COEFFS,forst,maxLen,
     &            minLen,merchL,mTopP,mTopS,stump,trim,minBfD,
     &            prod,iProd,sawDib,plpDib,short,shrtHt,errFlag,
     &            upsHt1)
C-----    Get DIBs at heights of 4.5' and 17.3'
          call r9dia417(COEFFS,topDib,dbhOb,topHt,ht1Prd,ht2Prd,
     &              htTot,sawDib,plpDib,errFlag,upsHt1,volEq)
          if(errFlag.ne.0) return
C-----Get total height
          call r9totHt(COEFFS%totHt,htTot,COEFFS%dbhIb,COEFFS%dib17,
     &             topHt,topDib,COEFFS%a, COEFFS%b,errFlag)
     
          ENDIF
          if(errFlag.ne.0) return

          if(COEFFS%totHt.le.17.3) errFlag=8
          if(errFlag.ne.0) return
          IF(VOLEQ(1:1).EQ.'8')THEN
          ! COEFFSO is coefficients for outside bark. Should use COEFFS for inside bark (2021/03/02)
          !  CALL r9ht(stemHt,COEFFSO,stemDib,errFlag)
            CALL r9ht(stemHt,COEFFS,stemDib,errFlag)
          ELSE
            CALL r9ht(stemHt,COEFFS,stemDib,errFlag)
          ENDIF
        ELSE ! REGION 8
c         Currently only works with total height equation
c          IF(VOLEQ(3:3).NE.'0')THEN
c            VOLEQ2 = VOLEQ(1:2)//'0'//VOLEQ(4:10)
c            ERRFLAG=1
c            RETURN
c          ELSE
c            VOLEQ2 = VOLEQ
c          ENDIF     
          VEQ = VOLEQ(3:3)   
          CALL R8PREPCOEF(VOLEQ, COEFFS, ERRFLAG)
          DBHIB = COEFFS%A4+COEFFS%B4*DBHOB
          IF(VEQ.EQ.'0'.OR.VEQ.EQ.'8')THEN
            DIB17=DBHOB*(COEFFS%A17+COEFFS%B17*(17.3/HTTOT)**2)
          ELSEIF(VEQ.EQ.'4'.OR.VEQ.EQ.'7'.OR.VEQ.EQ.'9')THEN
            IF(UPSHT1.EQ.0.)THEN
c            ! CALC upsht1 from HTTOT (2021/03/03)     
              VOLEQ2 = VOLEQ(1:2)//'0'//VOLEQ(4:10)
              CALL R8PREPCOEF(VOLEQ2, COEFFST, ERRFLAG)
              COEFFST%TOTHT = HTTOT
              COEFFST%DBHIB = COEFFST%A4+COEFFST%B4*DBHOB
              COEFFST%DIB17=DBHOB*(COEFFST%A17+COEFFST%B17*
     &                      (17.3/HTTOT)**2)
              CALL r9ht(UPSHT1,COEFFST,COEFFS%FIXDI,errFlag)
              
              IF(UPSHT1.EQ.0)THEN
                IF(VEQ.EQ.'4')THEN
                  IF(HT2PRD.GT.0) UPSHT1 = HT2PRD
                ELSE
                  IF(HT1PRD.GT.0) UPSHT1 = HT1PRD
                ENDIF  
              ENDIF
            ENDIF
            IF(UPSHT1.EQ.0)THEN
              ERRFLAG = 9
              RETURN
            ENDIF
            DIB17=DBHOB*(COEFFS%A17+COEFFS%B17*(17.3/UPSHT1)**2)
          ENDIF
          COEFFS%DBHIB = DBHIB
          COEFFS%DIB17 = DIB17
          COEFFS%TOTHT = HTTOT
          IF(VEQ.EQ.'0'.OR.VEQ.EQ.'8')THEN
            CALL r9ht(STEMHT,COEFFS,STEMDIB,errFlag)
          ELSEIF(VEQ.EQ.'4'.OR.VEQ.EQ.'7'.OR.VEQ.EQ.'9')THEN
C-----First Get total height
            topHt = UPSHT1
            topDib = COEFFS%FIXDI
            IF(HTTOT.LE.0.0)THEN
              VOLEQ2 = VOLEQ(1:2)//'0'//VOLEQ(4:10)
              CALL R8PREPCOEF(VOLEQ2, COEFFST, ERRFLAG)
              COEFFST%TOTHT = HTTOT
              COEFFST%DBHIB = DBHIB
              COEFFST%dib17 = DIB17
              call r9totHt(COEFFST%totHt,htTot,COEFFST%dbhIb,
     &         COEFFST%dib17,topHt,topDib,COEFFST%a, COEFFST%b,errFlag)
              if(COEFFST%totHt.le.17.3) errFlag=8
              if(errFlag.ne.0) return
              HTTOTC = COEFFST%totHt
            ENDIF
            IF(STEMDIB.LE.COEFFS%FIXDI)THEN
              VOLEQ2 = VOLEQ(1:2)//'0'//VOLEQ(4:10)
              CALL R8PREPCOEF(VOLEQ2, COEFFST, ERRFLAG)
              IF(HTTOT.EQ.0.0)THEN
                COEFFST%TOTHT = HTTOTC
                COEFFST%DBHIB = DBHIB
                COEFFST%dib17 = DIB17
              ELSE
                COEFFST%TOTHT = HTTOT
                COEFFST%DBHIB = COEFFST%A4+COEFFST%B4*DBHOB
                COEFFST%dib17 = DBHOB*(COEFFST%A17+COEFFST%B17*
     &                      (17.3/HTTOT)**2)
              ENDIF
              CALL r9ht(STEMHT,COEFFST,STEMDIB,errFlag)
            ELSE
              CALL R8CLKHT(VOLEQ,DBHOB,HTTOT,topHt,STEMDIB,STEMHT)
            ENDIF
          ENDIF
        ENDIF
      ELSEIF(MDL.EQ.'DEM' .OR. MDL.EQ.'CUR' .OR.
     +       MDL.EQ.'dem' .OR. MDL.EQ.'cur') THEN
        IF(VOLEQ(8:10).EQ.'000')THEN
          VOLEQ2 = VOLEQ(1:7)//'098'
        ELSE
          VOLEQ2 = VOLEQ
        ENDIF
        IF(VOLEQ2(8:10).EQ.'042'.OR.VOLEQ2(8:10).EQ.'242'.OR.
     +     VOLEQ2(8:10).EQ.'098'.OR.VOLEQ2(8:10).EQ.'351')THEN
C          region 10 call to determine total height or merch height
          CALL R10HTS(VOLEQ2,HTTOT,HT1PRD,DBHOB,HTTYPE,STUMP,STEMDIB,
     >          LMERCH)
           STEMHT = LMERCH + STUMP
        ENDIF
      ENDIF
1000  CONTINUE
      RETURN
      END
C ************************************************************************
      subroutine ht2topd_r(VOLEQ,REGN,FORST,DBHOB_d,HTTOT_d,
     + STMDIB_d,STMHT_d, ERRFLAG, UPSHT1_d,UPSD1_d)
C This subroutine is for R user to calculate stem height to a given top DIB      !
C YW 04/10/2017

      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::ht2topd_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'ht2topd_r_'::ht2topd_r

      IMPLICIT NONE
      DOUBLE PRECISION DBHOB_d,HTTOT_d,STMDIB_d,STMHT_d
      DOUBLE PRECISION UPSHT1_d,UPSD1_d
      CHARACTER*2  FORST 
      CHARACTER*10 VOLEQ
      INTEGER      REGN,ERRFLAG 

      INTEGER FCLASS,HTREF
      REAL DBHOB,HTTOT,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2
      REAL AVGZ1,AVGZ2,DBTBH,BTR,STMDIB,STMHT
      DBHOB = REAL(DBHOB_d)
      HTTOT = REAL(HTTOT_d)
      STMDIB = REAL(STMDIB_d)
      UPSHT1 = REAL(UPSHT1_d)
      UPSD1 = REAL(UPSD1_d)
C     Set the default value for other variable
c      UPSHT1 = 0.0
c      UPSD1 = 0.0
      DBTBH = 0.0
      BTR = 0.0
      AVGZ1=0.0
      HTREF=0
      UPSHT2=0.0
      UPSD2=0.0
      AVGZ2=0.0
      STMHT = 0.0
      UPSHT2 = 0.0
      UPSD2 = 0.0
      HTREF = 0
      FCLASS = 0
      
      CALL HT2TOPD(REGN,FORST,VOLEQ,DBHOB,HTTOT,HT1PRD,HT2PRD,
     + UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,BTR,FCLASS,
     + STMDIB,STMHT,ERRFLAG)
     
      STMHT_d = DBLE(STMHT)
      RETURN
      end subroutine ht2topd_r
c ********************************************************************      
      SUBROUTINE HT2TOPDCS(REGN,FORSTI,VOLEQI,DBHOB,HTTOT,HT1PRD,
     + HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,BTR,
     + FCLASS,STEMDIB,STEMHT,ERRFLAG)
       
      !DEC$ ATTRIBUTES DLLEXPORT::HT2TOPDCS
      IMPLICIT NONE 
      CHARACTER*(*) FORSTI,VOLEQI
      INTEGER REGN,ERRFLAG,HTREF,FCLASS
      REAL DBHOB,HTTOT,STEMDIB,STEMHT
      REAL HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2
      REAL DBTBH,BTR
      CHARACTER*10 VOLEQ
      CHARACTER*2 FORST
    
      FORST   = FORSTI(1:2)
      VOLEQ   = VOLEQI(1:10)
      CALL HT2TOPD(REGN,FORST,VOLEQ,DBHOB,HTTOT,HT1PRD,HT2PRD,
     + UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,BTR,FCLASS,
     + STEMDIB,STEMHT,ERRFLAG)
      FORSTI = FORST // char(0)
      VOLEQI = VOLEQ // char(0)
      RETURN
      END    
C ************************************************************************
      SUBROUTINE CALCHT2TOPD(REGN,FORSTI,VOLEQI,DBHOB,HTTOT,HT1PRD,
     + HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,BTR,
     + FCLASS,STEMDIB,STEMHT,ERRFLAG)
      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::CALCHT2TOPD
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: CALCHT2TOPD
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'CALCHT2TOPD'::CALCHT2TOPD
      CHARACTER*(*) FORSTI,VOLEQI
      INTEGER REGN,ERRFLAG,HTREF,FCLASS
      REAL DBHOB,HTTOT,STEMDIB,STEMHT
      REAL HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2
      REAL DBTBH,BTR
      CHARACTER*10 VOLEQ
      CHARACTER*2 FORST
    
      FORST   = FORSTI(1:2)
      VOLEQ   = VOLEQI(1:10)
      CALL HT2TOPD(REGN,FORST,VOLEQ,DBHOB,HTTOT,HT1PRD,HT2PRD,
     + UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,BTR,FCLASS,
     + STEMDIB,STEMHT,ERRFLAG)
      RETURN
      END    
      
             