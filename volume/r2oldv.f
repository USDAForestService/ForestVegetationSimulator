!== last modified  3-29-2004
      SUBROUTINE R2OLDV(VOLEQ,HTTOT,DBHOB,DRC,FCLASS,VOL,ERRFLAG,PROD,
     & MTOPP)

C--  THIS SUBROUTINE DETERMINES THE VOLUME OF A TREE
C--  USING REGION 2 D*D*H VOLUME DETERMINATION EQUATIONS.
! YW 2018/09/05 Added International boardfoot volume and sawlog portion cubic volume calculation
! for 200DVEW122, 200DVEW746, 200DVEW108, 200DVEW093 and 212DVEW122. This calculation
! is from FIA volume codes. 
! YW 2018/09/06 Added 201DEVW746 AND MYERS(1964) RM-6 TOP=8 (Scribner and international)
! YW 2018/09/12 Changed the FCLASS to be: 1 = sngle, others = multistem
!               This is to make it consistent with R3 DVE equation.
C**************************************************************

      CHARACTER*10 VOLEQ
      INTEGER ERRFLAG,MSTEM,FCLASS,I
      CHARACTER*2 PROD
      REAL MTOPP,TOPWOOD,CV6,SPF,DIB,X1,X2,X3
      REAL DBHOB,DRC,D2H,HTTOT,VOL(15),grsbdt,GCUFT,TCUFT,trevol
    
      TREVOL = 0.0
      grsbdt=0.0
      GCUFT=0.0
      ERRFLAG = 0
!     Changed the FCLASS to be: 1 = single, others = multistem
!     2018/09/12
      IF(FCLASS .EQ. 1) THEN
         MSTEM = 1
      ELSE
         MSTEM = 0
      ENDIF

C--   IF DBHOB OR HTTOT EQUALS ZERO THEN DON'T CALCULATE THE VOLUME

      IF (DBHOB .LE. 0.0 .AND. DRC.LE.0.0) THEN
         ERRFLAG = 3
         GO TO 1000
      ENDIF
      IF( HTTOT .LE. 0.0)THEN
         ERRFLAG = 4
         GO TO 1000
      ENDIF

      DO 100, I=1,15
         VOL(I) = 0
  100 CONTINUE
C--   CALCULATE THE DIAMETER SQUARED TIMES HEIGHT

      D2H = (DBHOB**2)*(HTTOT)

C--***********  ASPEN-RM232-TOTAL PG3  TOTAL CUBIC

      IF (VOLEQ(8:10).EQ.'746' .AND. VOLEQ(1:3).EQ.'200') THEN

         IF ( D2H .LE. 12470. ) THEN
            TCUFT = 0.002219 * D2H
         ELSE
            TCUFT = 0.001896 * D2H + 4.0267
         ENDIF

C--**************  ASPEN-RM232-TOP=6   BOARD FOOT
         IF(DBHOB.GT.7)THEN
             IF ( D2H .LE. 2500. ) THEN
                grsbdt = 8
             ELSEIF ( D2H .LE. 8850. ) THEN
                grsbdt = 0.011389 * D2H - 20.5112
             ELSE
                grsbdt = 0.010344 * D2H - 11.2615
             ENDIF
! INTERNATIONAL BOARDFOOT VOLUME (YW 2018/0905)
            IF(D2H.LE.11460) THEN
              VOL(10)= .013472*D2H-25.5968
            ELSE
              VOL(10) = .011989*D2H-8.6015
            ENDIF
         ENDIF
C--***********  ASPEN-RM232-TOP=4

         IF ( D2H .LE. 11800. ) THEN
            GCUFT = 0.002195 * D2H - 0.9076
         ELSE
            GCUFT = 0.001837 * D2H + 3.3075
         ENDIF
! SAWLOG PORTION CUBIC VOLUME (YW 2018/09/05)
         IF(PROD.EQ.'01'.AND.MTOPP.GE.6.0)THEN
           IF(DBHOB.LT.11.0) THEN
             SPF = 0.0
           ELSEIF (DBHOB.GT.42.0) THEN
             SPF = .96
           ELSE
             SPF =.92-(.22*((42.0-DBHOB)/33.0)**1.4)
           ENDIF
           CV6 = SPF*GCUFT
           IF(CV6.GT.0.0)THEN
             TOPWOOD = GCUFT-CV6
             VOL(7) = TOPWOOD
             GCUFT = CV6
           ENDIF
         ENDIF
           
! --Aspen
!Peterson, Geraldine. 1961. Volume Tables for Aspen in Colorado. Forest Service, U.S. Department of Agriculture, Rocky Mountain
!Forest Range Experiment Station. Research Notes, No. 63. 4p.
!volume to 4" top DIB
      ELSEIF (VOLEQ(8:10).EQ.'746' .AND. VOLEQ(1:3).EQ.'201') THEN
         DIB = (0.8954*DBHOB)+0.3168
         X1 = LOG10(DIB-4.0)
         X2 = LOG10(HTTOT-4.5)
         X3 = X1*X2
         GCUFT = (10**((0.0827*X1)+(0.4045*X2)+(.6593*X3)-0.4721))+0.3
C--***********  ASPEN-RM232-TOTAL PG3

      ELSEIF (VOLEQ(8:10).EQ.'746' .AND. VOLEQ(1:3).EQ.'210') THEN

         IF ( D2H .LE. 12470. ) THEN
            GCUFT = 0.002219 * D2H
         ELSE
            GCUFT = 0.001896 * D2H + 4.0267
         ENDIF
C******************************************************************
C--*************  LODGEPOLE PINE-RM6-TOTAL PG5  TOTAL CUBIC

      ELSEIF (VOLEQ(8:10).EQ.'108' .AND. VOLEQ(1:3).EQ.'200' ) THEN

         IF ( D2H .LE. 7000.) THEN
            TCUFT = 0.002777 * D2H + 0.027967
         ELSE
            TCUFT = 0.002332 * D2H + 3.446454
         ENDIF

C--*************  LODGEPOLE PINE-RM157-TOP=6   BOARD FOOT
         IF(DBHOB.GT.7)THEN

            IF ( D2H .LE. 22800. ) THEN
               grsbdt = 0.01202 * D2H - 6.00933
            ELSE
               grsbdt = 0.01263 * D2H - 19.76641
            ENDIF
! ADD INTERNATIONAL BOARDFOOT VOLUME FROM MYERS(1964) RM-6 TOP=8           
            IF(MTOPP.GE.8.0)THEN
              IF(D2H.LE.15000.0)THEN
                grsbdt = .012535*D2H-20.19057
                VOL(10) = 0.015097*D2H - 26.28782
              ELSE
                grsbdt = .012893*D2H -29.24566
                VOL(10) = 0.013766*D2H - 6.30999
              ENDIF
            ENDIF
         ENDIF
C--*************  LODGEPOLE PINE-RM6-TOP=4

         IF ( D2H .LE. 7000.) THEN
            GCUFT = 0.002798 * D2H - 1.04578
         ELSE
            GCUFT = 0.002256 * D2H + 2.836222
         ENDIF
! SAWLOG PORTION CUBIC VOLUME (YW 2018/09/05)
         IF(PROD.EQ.'01'.AND.MTOPP.GE.6.0)THEN
           IF(DBHOB.LT.9.0) THEN
             SPF = 0.0
           ELSEIF (DBHOB.GT.42.0) THEN
             SPF = .95
           ELSE
             SPF =.98-(.10*((42.0-DBHOB)/33.0)**4.0)
           ENDIF
           CV6 = SPF*GCUFT
           IF(CV6.GT.0.0)THEN
             TOPWOOD = GCUFT-CV6
             VOL(7) = TOPWOOD
             GCUFT = CV6
           ENDIF
         ENDIF

C--*************  LODGEPOLE PINE-RM6-TOTAL PG5

      ELSEIF (VOLEQ(8:10).EQ.'108' .AND. VOLEQ(1:3).EQ.'210') THEN

         IF ( D2H .LE. 7000.) THEN
            GCUFT = 0.002777 * D2H + 0.027967
         ELSE
            GCUFT = 0.002332 * D2H + 3.446454
         ENDIF

C******************************************************************
C--**************  PONDEROSA PINE PROSSER BLACK HILLS TOTAL  TOTAL CUBIC

      ELSEIF (VOLEQ(8:10).EQ.'122' .AND. VOLEQ(1:3).EQ.'203') THEN

         IF ( D2H .LT. 6000. ) THEN
            TCUFT = 0.0024506 * D2H + .3470564
         ELSE
            TCUFT = 0.0022325 * D2H + 3.2829984
         ENDIF

C--*************  PONDEROSA PINE PROSSER BLACK HILLS-TOP=6   BOARD FOOT
         IF(DBHOB.GT.7)THEN

            IF ( D2H .LT. 16000. ) THEN
               grsbdt = .0132429 * D2H - 26.0553842
            ELSE
               grsbdt = 0.0127305 * D2H - 15.5653183
            ENDIF
         ENDIF
C--**************  PONDEROSA PINE BLACK HILLS MYERS TOP=4

         IF ( D2H .LE. 6700. ) THEN
            GCUFT = 0.002297 * D2H - 1.032297
         ELSE
            GCUFT = 0.002407 * D2H - 2.257724
         ENDIF
C--**************  PONDEROSA PINE BLACK HILLS MYERS-RM8-TOT PG5

      ELSEIF (VOLEQ(8:10).EQ.'122' .AND. VOLEQ(1:3).EQ.'213') THEN

         IF ( D2H .LE. 6000. ) THEN
            GCUFT = 0.002213 * D2H + 0.030288
         ELSE
            GCUFT = 0.002474 * D2H - 1.557103
         ENDIF

C******************************************************************
C--**************  PONDEROSA PINE FRONT RANGE-RM218-TOTAL PG4  TOTAL CUBIC

      ELSEIF (VOLEQ(8:10).EQ.'122' .AND. VOLEQ(1:3).EQ.'200') THEN

         TCUFT = 0.00226 * D2H

C--**************  PONDEROSA PINE FRONT RANGE-RM218-TOP=6   BOARD FOOT
         IF(DBHOB.GT.7)THEN

            IF ( D2H .LE. 2830. ) THEN
               grsbdt = 8
            ELSE
               grsbdt = 0.01149 * D2H - 24.5404
            ENDIF
!           ADDED CALCULATION OF INTERNATIONAL BD YW 2018/09/04            
            IF ( D2H .LE. 2535. ) THEN
              VOL(10) = 9.0
            ELSE
              VOL(10) = .01286*D2H-23.5932
            ENDIF
         ENDIF
C--**************  PONDEROSA PINE FRONT RANGE-RM218-TOP-4

         GCUFT = 0.00216 * D2H - 0.44670
         
! SAWLOG PORTION CUBIC VOLUME (YW 2018/09/05)
         IF(PROD.EQ.'01'.AND.MTOPP.GE.6.0)THEN
           IF(DBHOB.LT.9.0) THEN
             SPF = 0.0
           ELSEIF (DBHOB.GT.42.0) THEN
             SPF = .93
           ELSE
             SPF =.95-(.26*((42.0-DBHOB)/33.0)**7.0)
           ENDIF
           CV6 = SPF*GCUFT
           IF(CV6.GT.0.0)THEN
             TOPWOOD = GCUFT-CV6
             VOL(7) = TOPWOOD
             GCUFT = CV6
           ENDIF
         ENDIF

C--**************  PONDEROSA PINE FRONT RANGE-RM218-TOTAL PG4

      ELSEIF (VOLEQ(8:10).EQ.'122' .AND. VOLEQ(1:3).EQ.'210') THEN

         GCUFT = 0.00226 * D2H


C******************************************************************
C--**************  ENGELMAN SPRUCE-RM95-TOTAL PG5  TOTAL CUBIC

      ELSEIF (VOLEQ(8:10).EQ.'093' .AND. VOLEQ(1:3).EQ.'200') THEN

         IF ( D2H .LE. 22500. ) THEN
            TCUFT = 0.00239 * D2H + 0.06439
         ELSE
            TCUFT = 0.00193 * D2H + 10.41663
         ENDIF

C--**************  ENGELMANN SPRUCE-RM95-TOP=6   BOARD FOOT

         IF(DBHOB.GT.7)THEN

            IF ( D2H .LE. 12200. ) THEN
               grsbdt = 0.01097 * D2H - 15.14466
            ELSE
               grsbdt = 0.01202 * D2H - 27.91343
            ENDIF
            IF(D2H.LE.27300.0) THEN
              VOL(10)= .01391*D2H-25.61022
            ELSE
              VOL(10)= .01235*D2H+17.02079
            ENDIF
         ENDIF

C--**************  ENGELMAN SPRUCE-RM95-TOP=4

         IF ( D2H .LE. 27900. ) THEN
            GCUFT = 0.00232 * D2H - 0.83010
         ELSE
            GCUFT = 0.00182 * D2H + 13.11320
         ENDIF
! SAWLOG PORTION CUBIC VOLUME (YW 2018/09/05)
         IF(PROD.EQ.'01'.AND.MTOPP.GE.6.0)THEN
           IF(DBHOB.LT.9.0) THEN
             SPF = 0.0
           ELSEIF (DBHOB.GT.42.0) THEN
             SPF = .98
           ELSE
             SPF =.96-(.15*((42.0-DBHOB)/33.0)**3.0)
           ENDIF
           CV6 = SPF*GCUFT
           IF(CV6.GT.0.0)THEN
             TOPWOOD = GCUFT-CV6
             VOL(7) = TOPWOOD
             GCUFT = CV6
           ENDIF
         ENDIF

C--**************  ENGELMAN SPRUCE-RM95-TOTAL PG5

      ELSEIF (VOLEQ(8:10).EQ.'093' .AND. VOLEQ(1:3).EQ.'210') THEN

         IF ( D2H .LE. 22500. ) THEN
            GCUFT = 0.00239 * D2H + 0.06439
         ELSE
            GCUFT = 0.00193 * D2H + 10.41663
         ENDIF

C******************************************************************
C--**************  PONDEROSA PINE BLACK HILLS MYERS-RM8-TOT PG5  TOTAL CUBIC

      ELSEIF (VOLEQ(8:10).EQ.'122' .AND. VOLEQ(1:3).EQ.'212') THEN

         IF ( D2H .LE. 6000. ) THEN
            TCUFT = 0.002213 * D2H + 0.030288
         ELSE
            TCUFT = 0.002474 * D2H - 1.557103
         ENDIF

C--**************  PONDEROSA PINE BLACK HILLS MYERS-RM8-TOP=8   BOARD FOOT
         IF(DBHOB.GT.7)THEN

            IF ( D2H .LE. 16000. ) THEN
               grsbdt = 0.012331 * D2H - 34.167170
            ELSE
               grsbdt = 0.016318 * D2H - 99.212720
            ENDIF
            IF(D2H.LE.13000.0) THEN
              VOL(10)= .015011*D2H-44.36046
            ELSE
              VOL(10)= .016991*D2H-68.7502
            ENDIF        
         ENDIF
C--**************  PONDEROSA PINE BLACK HILLS MYERS-RM8-TOP=4

         IF ( D2H .LE. 6700. ) THEN
             GCUFT = 0.002297 * D2H - 1.032297
         ELSE
             GCUFT = 0.002407 * D2H - 2.257724
         ENDIF
! SAWLOG PORTION CUBIC VOLUME (YW 2018/09/05)
         IF(PROD.EQ.'01'.AND.MTOPP.GE.6.0)THEN
           IF(DBHOB.LT.9.0) THEN
             SPF = 0.0
           ELSEIF (DBHOB.GT.42.0) THEN
             SPF = .93
           ELSE
             SPF =.95-(.26*((42.0-DBHOB)/33.0)**7.0)
           ENDIF
           CV6 = SPF*GCUFT
           IF(CV6.GT.0.0)THEN
             TOPWOOD = GCUFT-CV6
             VOL(7) = TOPWOOD
             GCUFT = CV6
           ENDIF
         ENDIF

C******************************************************************
C--**************  Oneseed Juniper Chojnacky INT-339
      ELSEIF (VOLEQ(8:10).EQ.'069')THEN
c     look for drc, if not use dbhob
        IF(DRC .GT. 0) D2H = DRC*DRC*HTTOT

        TCUFT = (-0.19321+0.136101*D2H**(1./3.)+0.038187*MSTEM)**3.
        GCUFT = TCUFT

C******************************************************************
C--**************  Rocky Mountain Juniper Chojnacky INT-339
      ELSEIF (VOLEQ(8:10).EQ.'066')THEN
c     look for drc, if not use dbhob
        IF(DRC .GT. 0) D2H = DRC*DRC*HTTOT

        TCUFT = (0.02434+0.119106*D2H**(1./3.))**3.
        GCUFT = TCUFT

C******************************************************************
C--**************  Utah Juniper Chojnacky INT-339
      ELSEIF (VOLEQ(8:10).EQ.'065')THEN
c     look for drc, if not use dbhob
        IF(DRC .GT. 0) D2H = DRC*DRC*HTTOT

        TCUFT = (-0.08728+0.135420*D2H**(1./3.)-0.019587*MSTEM)**3.
        GCUFT = TCUFT

C******************************************************************
C--**************  Gambel Oak Chojnacky INT-339
      ELSEIF (VOLEQ(8:10).EQ.'814')THEN
c     look for drc, if not use dbhob
        IF(DRC .GT. 0) D2H = DRC*DRC*HTTOT

        TCUFT = (-0.13600 + 0.145743*D2H**(1./3.))**3.
        GCUFT = TCUFT

C******************************************************************
C--**************  Bur Oak Chojnacky INT-339
      ELSEIF (VOLEQ(8:10).EQ.'823')THEN
c     look for drc, if not use dbhob
        IF(DRC .GT. 0) D2H = DRC*DRC*HTTOT

        TCUFT = (0.12853 + 0.105885*D2H**(1./3.))**3.
        GCUFT = TCUFT

C******************************************************************
C--**************  Pinyon Pine
      ELSEIF (VOLEQ(8:10).EQ.'106')THEN
c     look for drc, if not use dbhob
        IF(DRC .GT. 0) D2H = DRC*DRC*HTTOT

        TCUFT = (-0.20296 + 0.150283*D2H**(1./3.)+0.054178*MSTEM)**3.
        GCUFT = TCUFT

C******************************************************************
C--**************  Mountain Mahogany Chojnacky INT-339
      ELSEIF (VOLEQ(8:10).EQ.'475')THEN
c     look for drc, if not use dbhob
        IF(DRC .GT. 0) D2H = DRC*DRC*HTTOT

        TCUFT = (-0.13363 + 0.128222*D2H**(1./3.)+0.080208*MSTEM)**3.
        GCUFT = TCUFT

C******************************************************************
C--**************  Other Hardwoods Chojnacky INT-339
      ELSEIF (VOLEQ(8:10).EQ.'998')THEN
c     look for drc, if not use dbhob
        IF(DRC .GT. 0) D2H = DRC*DRC*HTTOT

        TCUFT = (-0.13822 + 0.121850*D2H**(1./3.))**3.
        GCUFT = TCUFT

C--       (ENDIF FOR VOLEQU EQUAL TO A VALID EQUATION)
      ELSE
         ERRFLAG = 1
      ENDIF

      IF (TCUFT .LT. 0.0) THEN
           TCUFT = 0.0
      ENDIF
      VOL(1) = TCUFT

      IF (grsbdt .LT. 0.0) THEN
            grsbdt = 0.0
      ENDIF
      VOL(2) = grsbdt

      IF (GCUFT .LT. 0.0) THEN
          GCUFT = 0.0
      ENDIF
      VOL(4) = GCUFT

 1000 CONTINUE

      RETURN
      END
C**************************************************************

C********BOARD FOOT VOLUME EQUATION NUMBERS********

C--    201 =  ASPEN RM-232 - 6" TOP - PAGE 6
C--    202 = LODGEPOLE PINE RESEARCH NOTE RM-157 - 6" TOP
C--    203 = PONDEROSA PINE BLACK HILLS PROSSER - 6" TOP
C--    204 = PONDEROSA PINE FRONT RANGE RM-218 - 6" TOP - PAGE 7
C--    207 = ENGELMANN SPRUCE RM-95 - 6" TOP - PAGE 8
C--    212 = PONDEROSA PINE BLACK HILLS MYERS RM-8-8" TOP - PG 8
C--    FOR LIMBER PINE USE EQUATION 204
C--    FOR SOUTHWEST PONDEROSA PINE USE EQUATION 204
C--    FOR DOUGLAS FIR USE EQUATION 207
C--    FOR TRUE FIRS USE EQUATION 207

C********CUBIC FOOT VOLUME EQUATION NUMBERS********

C--    201 =  ASPEN RM-232 - 4" TOP - PAGE 4
C--    202 = LODGEPOLE PINE RM-6 - 4" TOP - PAGE 6
C--    203 = PONDEROSA PINE MYERS RM-8-4" TOP
C--    204 = PONDEROSA PINE FRONT RANGE RM-218 - 4" TOP - PAGE 5
C--    207 = ENGELMANN SPRUCE RM-95 - 4" TOP - PAGE 6
C--    212 = PONDEROSA PINE BLACK HILLS MYERS RM-8-4" TOP - PG 6
C--    FOR LIMBER PINE USE EQUATION 204
C--    FOR SOUTHWEST PONDEROSA PINE USE EQUATION 204
C--    FOR DOUGLAS FIR USE EQUATION 207
C--    FOR TRUE FIRS USE EQUATION 207

C*****CUBIC FOOT BIOMASS GROUND TO TIP VOLUME EQUATION NUMBERS
C--    221 =  ASPEN RM-232 - PAGE 3
C--    222 = LODGEPOLE PINE RESEARCH NOTE RM-6 - PAGE 5
C--    223 = PONDEROSA PINE BLACK HILLS MYERS RM-8 - PAGE 5
C--    224 = PONDEROSA PINE FRONT RANGE RM-218 - PAGE 4
C--    227 = ENGELMANN SPRUCE RM-95 - PAGE 5
C--    FOR LIMBER PINE USE EQUATION 204
C--    FOR SOUTHWEST PONDEROSA PINE USE EQUATION 204
C--    FOR DOUGLAS FIR USE EQUATION 207
C--    FOR TRUE FIRS USE EQUATION 207

C--  HTTOT - REAL -  **TREE HEIGHT IN FT. FROM A 1 FOOT STUMP**
