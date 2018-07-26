      SUBROUTINE FMCROWE (SPILS,SPIYV,D,H,IC,SG,XV)
      IMPLICIT NONE
C----------
C FIRE-VBASE $Id$
C----------

C     CALLED FROM: FMCROW

C  PURPOSE:
C     THIS SUBROUTINE CALCULATES CROWNW(TREE,SIZE), THE WEIGHT OF
C     VARIOUS SIZES OF CROWN MATERIAL THAT IS ASSOCIATED WITH EACH TREE
C     RECORD IN THE CURRENT STAND.  THESE WEIGHTS COME FROM:
C     1-NATIONAL-SCALE ESTIMATORS FOR UNITED STATES TREE SPECIES, BY
C     JENKINS ET. AL. (FOR. SCI. 2003 49(1))
C     2-ESTIMATING ASPEN CROWN FUELS IN NORTHEASTERN MINNESOTA, BY R.M.
C     LOOMIS AND P.J. ROUSSOPOULOS (RES. PAP. NC-156)
C     3-ESTIMATING NORTHERN RED OAK CROWN COMPONENT WEIGHTS IN THE
C     NORTHEASTERN UNITED STATES, BY R.M. LOOMIS AND R.W. BLANK
C     (RES. PAP. NC-194)
C     4-ESTIMATING FOLIAGE AND BRANCHWOOD QUANTITIES IN SHORTLEAF PINE
C     BY R.M. LOOMIS, R.E. PHARES, AND J.S. CROSBY (FOREST SCIENCE,
C     VOL 12, ISSUE 1, 1966)
C     5-PREDICTING CROWN WEIGHT AND BOLE VOLUME OF FIVE WESTERN
C     HARDWOODS BY J.A. KENDALL SNELL AND SUSAN N. LITTLE
C     (RES. PAP. PNW-151, MARCH 1983)

C  LOCAL VARIABLE DEFINITIONS:

C     SPILS = SPECIES NUMBER AS DEFINED BELOW (SAME AS FOR LS VARIANT)
C     SPIYV = SPECIES NUMBER IN VARIANT CALLING ROUTINE
C     D = DBH
C     H = HEIGHT
C     C = CROWN RATIO EXPRESSED AS A PERCENT

C     LILPCE = BIOMASS OF THE SMALL PIECE MISSING SINCE VOLUME
C            EQUATIONS GO TO 4" DIB AND CROWN EQUATIONS START AT 4" DOB
C     HTLP = HT CALC NEEDED TO ESTIMATE LILPCE.  HT AT 4" DOB.
C     DIB  = DIB VALUE THAT CORRESPONDS TO 4" DOB.
C     TTOPW  = TOTAL TOP WEIGHT
C     FOL    = FOLIAGE BIOMASS
C     P1 - P3 = PROPORTIONS OF CROWN MATERIAL IN DIFFERENT SIZE CLASSES
C     F1 - F4 = PROPORTIONS USED IN CALCULATING MAPLE BIOMASS (SLIGHTLY
C              DIFFERENT THEN P1-P3, MATCH SNELL AND LITTLE NOTATION)
C     HTF     = HEIGHT TO A FOUR INCH TOP DIAMETER (IB)
C     UMBTW   = UNMERCHANTABLE BOLE TIP WEIGHT BY SIZE CLASS.
C              (1 = 0-.25, 2 = 0-1, 3 = 0-3, 4 = 0 - 4)
C     ANGLE, TEMPHT, DBRK  = USED IN CALCULATING UMBTW
C     TOTABV = TOTAL ABOVE GROUND BIOMASS (AS PER JENKINS ET. AL.)

C   SPECIES LIST (SPILS)
C     1   jack pine                                
C     2   scotch pine                              
C     3   red pine natural                         
C     4   red pine plantation                      
C     5   white pine                               
C     6   white spruce                             
C     7   Norway spruce                            
C     8   balsam fir                               
C     9   black spruce                             
C     10  tamarack                                 
C     11  n. white cedar                           
C     12  eastern hemlock                          
C     13  other softwoods    
C     14  eastern redcedar                         
C     15  black ash                                
C     16  green ash                                
C     17  cottonwood                               
C     18  silver maple                             
C     19  red maple                                
C     20  black cherry                             
C     21  American elm                             
C     22  slippery elm                             
C     23  rock elm                               
C     24  yellow birch                             
C     25  basswood                                 
C     26  sugar maple                              
C     27  black maple                            
C     28  American beech                           
C     29  white ash                              
C     30  white oak                                
C     31  swamp white oak                          
C     32  bur oak                                  
C     33  chinkapin oak                            
C     34  northern red oak                         
C     35  black oak                                
C     36  northern pin oak                         
C     37  bitternut hickory                        
C     38  pignut hickory                           
C     39  shagbark hickory                         
C     40  bigtooth aspen                           
C     41  quaking aspen                            
C     42  balsam poplar                            
C     43  paper birch                              
C     44  commercial hardwoods    
C     45  butternut                                
C     46  black walnut                             
C     47  eastern hophornbeam                      
C     48  black locust                             
C     49  non-commercial hardwoods  
C     50  boxelder                                 
C     51  striped maple                            
C     52  mountain maple                         
C     53  American hornbeam                        
C     54  American chestnut                        
C     55  hackberry / sugarberry              
C     56  flowering dogwood                        
C     57  hawthorn                                 
C     58  apple sp.                                
C     59  black gum / tupelos                               
C     60  sycamore                                 
C     61  pin cherry                               
C     62  choke cherry                             
C     63  wild plum                                
C     64  willow                                   
C     65  black willow                             
C     66  diamond willow                         
C     67  sassafras                                
C     68  American mountain ash   

COMMONS

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

      INCLUDE 'CONTRL.F77'
      INCLUDE 'COEFFS.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'VARCOM.F77'

C  VARIABLE DECLARATIONS

      REAL    D,H,SG,XV(0:5)
      INTEGER SPILS,SPIYV,IC

      INTEGER J, K
      REAL C, ANGLE, XNEG1
      REAL TTOPW, FOL, DBRK(0:3), TOTABV, BARK, WOOD, BRANCH
      REAL P1, P2, P3, TEMP, F1, F2, F3, F4, HTF, TEMPHT, MYPI, UMBTW(4)
      REAL LILPCE, DIB, DOBF, HTLP, TEMPD, BRATIO, VT, VT1, VT2
      REAL DX,HX
      LOGICAL DEBUG, LMERCH

      DATA MYPI/3.14159/

      CALL DBCHK (DEBUG,'FMCROWE',7,ICYC)
      IF (DEBUG) WRITE(JOSTND,'('' ENTERING FMCROWE'')')

C     GET SOME VARIABLES YOU'LL NEED.

      DX = D            ! STORE ORIGINAL D, H
      HX = H
      C  = REAL(IC)

C  INITIALIZE ALL THE CANOPY COMPONENTS TO ZERO, AND SKIP THE REST
C  OF THIS LOOP IF THE TREE HAS NO DIAMETER, OR HEIGHT.

      DO J = 0,5
        XV(J) = 0.0
        IF (J.GT.0 .AND. J.LT.5) UMBTW(J) = 0
      ENDDO
      FOL    = 0.0
      TTOPW  = 0.0
      LILPCE = 0.0

      IF ((D .EQ. 0.0) .OR. (H .EQ. 0.0)) GOTO 999

C  LETS GET ABOVE GROUND BIOMASS ESTIMATES FROM JENKINS ET. AL. (FOR. SCI. 49(1))
C  FOR TREES LESS THAN 1 INCH, GET THE 1 INCH ESTIMATE AND SCALE BACK

      IF (DEBUG) WRITE(JOSTND,*) 'ABOUT TO CALC TOTABV'
      IF (D .LT. 1.0) THEN
        D = 1.0
      ENDIF
      SELECT CASE (SPILS)

C       aspen/alder/cottonwood/willow
        CASE (17,40:42,64:66)
          TOTABV = EXP(-2.2094 + 2.3867*LOG(D*2.54))

C       soft maple/birch
        CASE (18,19,24,43,49:52)
          TOTABV = EXP(-1.9123 + 2.3651*LOG(D*2.54))

C       mixed hardwood
        CASE (15,16,20:23,25,29,44:48,53:63,67,68)
          TOTABV = EXP(-2.4800 + 2.4835*LOG(D*2.54))

C       hard maple/oak/hickory/beech
        CASE (26:28,30:39)
          TOTABV = EXP(-2.0127 + 2.4342*LOG(D*2.54))

C       cedar/larch (taxodiaceae is put here)
        CASE (10,11,13,14)
          TOTABV = EXP(-2.0336 + 2.2592*LOG(D*2.54))

C       true fir/hemlock
        CASE (8,12)
          TOTABV = EXP(-2.5384 + 2.4814*LOG(D*2.54))

C       pine
        CASE (1:5)
          TOTABV = EXP(-2.5356 + 2.4349*LOG(D*2.54))

C       spruce
        CASE (6,7,9)
          TOTABV = EXP(-2.0773 + 2.3323*LOG(D*2.54))

      END SELECT
      
      TOTABV = TOTABV * 2.2046
      
      SELECT CASE (SPILS)

C       hardwoods
        CASE (15:68)
          FOL = EXP(-4.0813 + 5.8816/(D*2.54))
          BARK = EXP(-2.0129 - 1.6805/(D*2.54))
          WOOD = EXP(-0.3065 - 5.4240/(D*2.54))
          
C       softwoods
        CASE (1:14)
          FOL = EXP(-2.9584 + 4.4766/(D*2.54))
          BARK = EXP(-2.0980 - 1.1432/(D*2.54))
          WOOD = EXP(-0.3737 - 1.8055/(D*2.54))
                    
      END SELECT
      
      FOL = FOL * TOTABV
      BARK = BARK * TOTABV
      WOOD = WOOD * TOTABV
      BRANCH = TOTABV - (FOL + BARK + WOOD)
      
C     RESET D

      IF (DX .LT. 1.0) THEN
        D = DX
        FOL = D * FOL
        BARK = D * BARK
        WOOD = D * WOOD
        BRANCH = D * BRANCH
        TOTABV = D * TOTABV       
      ENDIF
      
      IF (BRANCH .LT. 0) BRANCH = 0
      TTOPW = BRANCH

      IF (DEBUG) WRITE(JOSTND,*) 'D = ',D,'H = ',H,'TTOPW = ',TTOPW

C     FOR SMALL TREES IN THE EAST WE WANT TO ADD IN AN ESTIMATE OF THE BOLE VOLUME TOO. 
C     SINCE SMALL TREES ARE UNMERCHANTABLE, THE "CROWN" IS REALY THE WHOLE TREE.

C     FOR TREES LESS THAN THE MERCH DBH, ESTIMATE THE VOLUME OF THE BREAKPOINT TREE
C     AND USE THIS WITH THE STANDARD VOLUME ESTIMATE OF THE BREAKPOINT TREE TO
C     CREATE AN ADJUSTMENT FACTOR.

      TEMP = 0.0       
      IF  ((DX .LT. DBHMIN(SPIYV)) .AND. 
     >    ((VARACD .EQ. 'SN') .OR. (VARACD .EQ. 'LS') .OR. 
     >     (VARACD .EQ. 'NE') .OR. (VARACD .EQ. 'CS'))) THEN 
      
        D = DBHMIN(SPIYV)

        CALL HTDBH(IFOR,SPIYV,D,H,0)

        XNEG1  = -1.0
        LMERCH = .FALSE.
        CALL FMSVL2(SPIYV,D,H,XNEG1,VT,LMERCH,DEBUG,JOSTND)
        VT1 = 0.0015*DX*DX*HX     !!! MVD eqn. - volume of a cone minus some % for loss of tip volume  
        VT2 = 0.0015*D*D*H
        VT = (VT/VT2)*VT1
        TEMP = SG * VT / P2T  
        TTOPW = TTOPW + TEMP 
        
C     RESET D and H

        D = DX
        H = HX        

      ENDIF

      IF (DEBUG) WRITE(JOSTND,*) 'D = ',D,'H = ',H,'FOL = ',FOL,
     & 'DBHMIN= ',DBHMIN(SPIYV), 'TTOPW = ',TTOPW,' TEMP = ',TEMP

C     NOW WE NEED TO ALLOCATE THE CROWN BIOMASS TO DIFFERENT SIZE CLASSES.
C     EACH SPECIES IS MAPPED TO EITHER SHORTLEAF PINE, ASPEN, RED OAK, OR
C     MAPLE, SINCE THESE ARE THE SPECIES WE HAVE SOME SORT OF SIZE
C     BREAKDOWN FOR.  REFERENCES ARE 1)LOOMIS AND BLANK, 2) LOOMIS, PHARES,
C     AND CROSBY, 3) LOOMIS AND ROUSSOPOULOS AND 4) SNELL AND LITTLE

C     P1 = PROPORTION IN 0-.25 INCH CLASS
C     P2 = PROPORTION IN 0-1 INCH CLASS
C     P3 = PROPORTION IN 0-3 INCH CLASS

C     SNELL AND LITTLE DO IT A LITTLE DIFFERENTLY.  FOR MAPLE,
C     F1 = PROPORTION OF FOLIAGE
C     F2 = PROPORTION OF FOLIAGE + 0-.25
C     F3 = PROPORTION OF FOLIAGE + 0-1 INCH
C     F4 = PROPORTION OF FOLIAGE + 0-3 INCH

      IF (DEBUG) WRITE(JOSTND,*) 'ABOUT TO CALC PROPORTIONS'
      IF (DEBUG) WRITE(JOSTND,*) 'SPILS = ',SPILS, 'SPIYV= ', SPIYV,
     >  'D = ',D, 'C = ',C

      P1 = 0.0
      P2 = 0.0
      P3 = 0.0
      F1 = 0.0
      F2 = 0.0
      F3 = 0.0
      F4 = 0.0

      SELECT CASE (SPILS)

C       red oak
        CASE (30:39) ! OAKS AND HICKORIES
          P1 =  6.4735*(D**(-1.1313))*(C**(-0.5777))
          P2 = 36.8351*(D**(-0.9345))*(C**(-0.7014))
          P3 = 28.2916*(D**(-0.8658))*(C**(-0.4084))

C       shortleaf pine
        CASE (1:14) ! ALL THE CONIFERS
          P1 = 3.525*(D**(-0.778))*(C**(-0.412))
          P2 = 5.989*(D**(-0.565))*(C**(-0.346))
          P3 = 8.585*(D**(-0.517))*(C**(-0.223))
          IF (D .LE. 1.5) P1 = 0.5
          IF (D .LE. 1.5) P2 = 1.0
          IF ((D .LE. 10.5) .OR. (C .LE. 35)) P3 = 1

C       maple
        CASE (18,19,26,27,49:52) ! ALL THE MAPLES
          F1 = 1.0/(4.6762 + 0.1091*D**2.0390)
          F2 = 1.0/(3.3212 + 0.0777*D**2.0496)
          F3 = 1.0/(0.9341 + 0.0158*D**2.1627)
          F4 = 1.0/(0.8625 + 0.0093*D**1.7070)
          IF (D .LT. 1.9) F3 = 1.0
          IF (D .LT. 4.8) F4 = 1.0

C       aspen
        CASE DEFAULT ! ALL THE REST
          P1 = 1.856*(D*2.54)**(-0.773)
          P2 = 5.317*(D*2.54)**(-0.718)
          P3 = 1.793*(D*2.54)**(-0.185)

      END SELECT

C     BECAUSE THE ABOVE EQUATIONS THAT PREDICT PROPORTIONS IN EACH SIZE
C     CLASS ARE BASED ON DIFFERENT TOP ASSUMPTIONS, WE NEED TO GET THE
C     BIOMASS IN THE UNMERCH. TIP (BOLEWOOD ABOVE 4 INCH TOP DIAM) TO
C     REALIGN THE PROPORTIONS.  FIRST, WE GET THE HEIGHT AT A 4 IN TOP
C     DIAMETER.  ASSUMING A CONE, WE CAN GET VOLUME, CONVERT TO BIOMASS AND
C     ALSO BREAK THE CONE UP INTO PIECES TO FIGURE OUT WHAT GOES IN EACH
C     SIZE CLASS.

      IF (DEBUG) WRITE(JOSTND,*) 'ABOUT TO CALC BOLE TIP'

C     NOW GET THE HEIGHT (HTF) AT A 4 INCH TOP DIAM (IB)
      DOBF = 4.0 / BRATIO(SPIYV,D,H)
      IF ((D .GT. DOBF) .AND. (D .GT. DBHMIN(SPIYV))) THEN
   
        HTF = 4.5 + (H - 4.5)/D*(D - DOBF)  !assumes constant taper

C       CALCULATE TOTAL VOLUME OF UNMERCH TIP

        IF ((H - HTF) .GT. 0) THEN
          TEMP = (H - HTF)*4*4*MYPI/12/12/12
        ELSE
          TEMP = 0
        ENDIF
        UMBTW(4)=(SG * TEMP / P2T)

C       CALCULATE AMOUNT IN DIFFERENT SIZE CLASSES
C       UMBTW = UNMERCHANTABLE BOLE TIP WEIGHT BY SIZE CLASS.
C       (1 = 0-.25, 2 = 0-1, 3 = 0-3, 4 = 0 - 4)

        DBRK(0) = 0.0
        DBRK(1) = 0.25
        DBRK(2) = 1.0
        DBRK(3) = 3.0
        ANGLE = ATAN((H - HTF)/2.0)

        DO J = 1,3
          TEMPHT = DBRK(J)/2*TAN(ANGLE)
          TEMP = (TEMPHT)*DBRK(J)*DBRK(J)*MYPI/12/12/12
          UMBTW(J) = (SG * TEMP / P2T)
        ENDDO

C       CALCULATE THE AMOUNT IN THE LITTLE MISSING PIECE, I.E. THE AMOUNT
C       MISSING DUE TO THE FACT THAT THE VOLUME EQUATIONS GO TO 4 INCHES DIB
C       AND THE CROWN EQUATIONS START AT 4 INCHES DOB.
C       THIS ENTAILS USING BRATIO TO GET THE DIB THAT CORRESPONDS TO 4 IN DOB
C       AND GETTING THE HEIGHT AT THIS DIAMETER.
C       THIS LILPCE NEEDS TO BE SUBRACTED FROM UMBTW, SINCE NOT INCLUDED IN THE 
C       TTOPW CALCULATION. LATER IT WILL
C       BE ADDED BACK INTO CROWNW, SO THE PIECE IS NOT EXCLUDED.

        IF ((VARACD .EQ. 'SN') .OR. (VARACD .EQ. 'LS') .OR. 
     >      (VARACD .EQ. 'NE') .OR. (VARACD .EQ. 'CS')) THEN 
          DIB = 4 * BRATIO(SPIYV,D,H)
          HTLP = 4.5 + (H - 4.5)/D*(D - 4.0)  !assumes constant taper      
          IF ((HTLP - HTF) .GT. 0) THEN
            LILPCE = MYPI*(HTLP - HTF)/12/12/12*(4*4 + 4*DIB + DIB*DIB)
          ELSE
            LILPCE = 0.0
          ENDIF
          LILPCE = LILPCE * SG/P2T
          IF (LILPCE .LT. 0) LILPCE = 0
          UMBTW(4) = UMBTW(4) - LILPCE
        ENDIF
      ELSE

C       IF TREE IS LESS THAN 4" DBH OR UNMERCH, DO THE SAME THING
C       FIRST CALCULATE TOTAL BIOMASS OF STEMWOOD ABOVE BREAST HEIGHT

        IF (H .GT. 4.5) THEN
          TEMP = (H - 4.5)*D*D*MYPI/12/12/12
          UMBTW(4)=(SG * TEMP / P2T)

C         CALCULATE AMOUNT IN DIFFERENT SIZE CLASSES
C         (1 = 0-.25, 2 = 0-1, 3 = 0-3, 4 = 0 - 4)

          DBRK(0) = 0.0
          DBRK(1) = 0.25
          DBRK(2) = 1.0
          DBRK(3) = 3.0
          ANGLE = ATAN((H - 4.5)/(D/2.0))

          DO J = 1,3
            IF ((J .EQ. 1) .OR.
     &           (J .GT. 1 .AND. D .GT. DBRK(J-1))) THEN
              TEMPD = MIN(DBRK(J), D)
              TEMPHT = TEMPD/2*TAN(ANGLE)
              TEMP = (TEMPHT)*TEMPD*TEMPD*MYPI/12/12/12
              UMBTW(J) = (SG * TEMP / P2T)
            ENDIF
          ENDDO
        ENDIF

C       NOW ADD STEM MATERIAL LESS THAN 4.5 FT IN HEIGHT.  ASSUME A CYLINDER.

        TEMP = MYPI*D*D/4/12/12*MIN(4.5,H)
        IF (D .LE. 0.25) THEN
          K = 1
        ELSEIF (D .LE. 1.0) THEN
          K = 2
        ELSEIF (D .LE. 3.0) THEN
          K = 3
        ELSE
          K = 4
        ENDIF
        DO J = K,4
          UMBTW(J) = UMBTW(J) + TEMP
        ENDDO
      ENDIF

      IF (TTOPW .LT. 0) TTOPW = 0
      IF (LILPCE .LT. 0) LILPCE = 0
      IF (FOL .LT. 0)    FOL = 0
      IF (P1 .LT. 0.0)   P1 = 0.0
      IF (P2 .LT. 0.0)   P2 = 0.0
      IF (P3 .LT. 0.0)   P3 = 0.0
      IF (P1 .GT. 1.0)   P1 = 1.0
      IF (P2 .GT. 1.0)   P2 = 1.0
      IF (P3 .GT. 1.0)   P3 = 1.0
      IF (P2 .LT. P1)    P2 = P1
      IF (P3 .LT. P2)    P3 = P2
      IF (F1 .LT. 0.0)   F1 = 0.0
      IF (F2 .LT. 0.0)   F2 = 0.0
      IF (F3 .LT. 0.0)   F3 = 0.0
      IF (F4 .LT. 0.0)   F4 = 0.0
      IF (F1 .GT. 1.0)   F1 = 1.0
      IF (F2 .GT. 1.0)   F2 = 1.0
      IF (F3 .GT. 1.0)   F3 = 1.0
      IF (F4 .GT. 1.0)   F4 = 1.0
      IF (F2 .LT. F1)    F2 = F1
      IF (F3 .LT. F2)    F3 = F2
      IF (F4 .LT. F3)    F4 = F3
      IF (UMBTW(1) .LT. 0) UMBTW(1) = 0
      IF (UMBTW(2) .LT. 0) UMBTW(2) = 0
      IF (UMBTW(3) .LT. 0) UMBTW(3) = 0
      IF (UMBTW(4) .LT. 0) UMBTW(4) = 0
      IF (UMBTW(2) .LT. UMBTW(1)) UMBTW(2) = UMBTW(1)
      IF (UMBTW(3) .LT. UMBTW(2)) UMBTW(3) = UMBTW(2)
      IF (UMBTW(4) .LT. UMBTW(3)) UMBTW(4) = UMBTW(3)

C  WHEN CALCULATING CROWN BIOMASS FOR EACH SIZE CLASS, ADJUST THE VALUES
C  BASED ON THE UNMERCHANTABLE STEMWOOD.  THIS HELPS CORRECT THE PROBLEM
C  THAT THE UNMERCH. STEMWOOD WASN'T INCLUDED WHEN PREDICTING
C  PROPORTIONS IN EACH SIZE CLASS (IN LITERATURE).

      SELECT CASE (SPILS)

C       PROPORTION ESTIMATES FROM MAPLE AND SHORTLEAF PINE DID NOT SEEM
C       TO INCLUDE ANY BOLEWOOD--JUST BRANCHWOOD

        CASE (18,19,26,27,49:52) ! maple
          IF (TTOPW .LT. UMBTW(4)) TTOPW = UMBTW(4)
          TTOPW = TTOPW + FOL
          XV(0)=FOL
          XV(1)=(TTOPW-UMBTW(4))*(F2-F1)+UMBTW(1)
          XV(2)=(TTOPW-UMBTW(4))*(F3-F2)+(UMBTW(2)-UMBTW(1))
          XV(3)=(TTOPW-UMBTW(4))*(F4-F3)+(UMBTW(3)-UMBTW(2))
          XV(4)=(TTOPW-UMBTW(4))*(1-F4)+(UMBTW(4)-UMBTW(3))
          XV(5)=0

        CASE (1:14) ! shortleaf pine
          IF (TTOPW .LT. UMBTW(4)) TTOPW = UMBTW(4)
          XV(0)=FOL
          XV(1)=(TTOPW-UMBTW(4))*P1+UMBTW(1)
          XV(2)=(TTOPW-UMBTW(4))*(P2-P1)+(UMBTW(2)-UMBTW(1))
          XV(3)=(TTOPW-UMBTW(4))*(P3-P2)+(UMBTW(3)-UMBTW(2))
          XV(4)=(TTOPW-UMBTW(4))*(1-P3)+(UMBTW(4)-UMBTW(3))
          XV(5)=0

C       RED OAK CROWN PROPORTION PAPER INCLUDES BOLEWOOD LESS THAN 1 INCH

        CASE (30:39) ! red oak
          IF (TTOPW .LT. (UMBTW(4) - UMBTW(2)))
     &                           TTOPW = UMBTW(4) - UMBTW(2)
          XV(0)=FOL
          XV(1)=(TTOPW-UMBTW(4)+UMBTW(2))*P1
          XV(2)=(TTOPW-UMBTW(4)+UMBTW(2))*(P2-P1)
          XV(3)=(TTOPW-UMBTW(4)+UMBTW(2))*(P3-P2)+
     &                (UMBTW(3)-UMBTW(2))
          XV(4)=(TTOPW-UMBTW(4)+UMBTW(2))*(1-P3)+
     &                (UMBTW(4)-UMBTW(3))
          XV(5)=0

C       ASPEN CROWN PROPORTION PAPER INCLUDES BOLEWOOD LESS THAN .25 INCHES

        CASE DEFAULT ! aspen
          IF (TTOPW .LT. (UMBTW(4) - UMBTW(1)))
     >                        TTOPW = UMBTW(4) - UMBTW(1)
          XV(0)=FOL
          XV(1)=(TTOPW-UMBTW(4)+UMBTW(1))*P1
          XV(2)=(TTOPW-UMBTW(4)+UMBTW(1))*(P2-P1)+
     >             (UMBTW(2)-UMBTW(1))
          XV(3)=(TTOPW-UMBTW(4)+UMBTW(1))*(P3-P2)+
     >             (UMBTW(3)-UMBTW(2))
          XV(4)=(TTOPW-UMBTW(4)+UMBTW(1))*(1-P3)+
     >             (UMBTW(4)-UMBTW(3))
          XV(5)=0

      END SELECT

C     FOR LARGE TREES ADD THE LILPCE BACK IN TO THE RIGHT SIZE CLASS.

      IF ((D .GT. DOBF) .AND. (D .GT. DBHMIN(SPIYV))) THEN
        XV(4) = XV(4) + LILPCE
      ENDIF

  999 CONTINUE

      RETURN
      END