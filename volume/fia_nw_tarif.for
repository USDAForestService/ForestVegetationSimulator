! This is from FIA package code NIMS_VOL_FIA_EAST_PKB
! Created by YW 2018/08/07
!
!--Browne, J. E. 1962
!--Standard Cubic Foot Volume for Commercial Tree Species of British Columbia
!--British Columbia Forest Service
!--Coastal trees less than 140 years old
! FUNCTION BROWNE_COAST_YOUNG_CVTS(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER)
! FUNCTION DNR24_BC_COAST_YOUNG(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER, VOL_TYPE VARCHAR2 := 'CVTS')
!--Valid for 011, 098, 202, 242 and 263
! FIA VOLEQ NUMBER:
!--VOL_TYPE Options:
!--  CVTS   Cubic foot volume of total stem from ground to tip of tree                    - CU000070
!--  CVT    Cubic foot volume from stump to tip of tree                                   - CU000071
!--  CV4    Cubic foot volume from stump to 4 inch top                                    - CU000072
!--  CV6    Cubic foot volume from stump to 6 inch top                                    - CU000073
!--  CV8    Cubic foot volume from stump to 8 inch top                                    - CU000074
!--  SV616  Scribner board foot volume from stump to 6 inch top using 16 foot scale rules - BD000050
!--  SV632  Scribner board foot volume from stump to 6 inch top using 32 foot scale rules - BD000051
!--  SV816  Scribner board foot volume from stump to 8 inch top using 16 foot scale rules - BD000052
!--  IV6    International board foot volume from stump to 6 inch top foot scale rules     - BD000053
!--  IV8    International board foot volume from stump to 6 inch top foot scale rules     - BD000054
! NVEL EQUATION NUMBER
! P16BRC2011,P16BRC2098,P16BRC2202,P16BRC2242,P16BRC2263,
! P32BRC2011,P32BRC2098,P32BRC2202,P32BRC2242,P32BRC2263,
!
! FUNCTION BROWNE_COAST_OLD_CVTS(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER)
! FUNCTION DNR24_BC_COAST_OLD(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER, VOL_TYPE VARCHAR2 := 'CVTS')
! --Valid for 011, 098, 202, 242 and 263
! --VOL_TYPE Options:
! --  CVTS   Cubic foot volume of total stem from ground to tip of tree                    - CU000075
! --  CVT    Cubic foot volume from stump to tip of tree                                   - CU000076
! --  CV4    Cubic foot volume from stump to 4 inch top                                    - CU000077
! --  CV6    Cubic foot volume from stump to 6 inch top                                    - CU000078
! --  CV8    Cubic foot volume from stump to 8 inch top                                    - CU000079
! --  SV616  Scribner board foot volume from stump to 6 inch top using 16 foot scale rules - BD000055
! --  SV632  Scribner board foot volume from stump to 6 inch top using 32 foot scale rules - BD000056
! --  SV816  Scribner board foot volume from stump to 8 inch top using 16 foot scale rules - BD000057
! --  IV6    International board foot volume from stump to 6 inch top foot scale rules     - BD000058
! --  IV8    International board foot volume from stump to 6 inch top foot scale rules     - BD000059
! NVEL EQUATION NUMBER
! P16BRC1011,P16BRC1098,P16BRC1202,P16BRC1242,P16BRC1263,
! P32BRC1011,P32BRC1098,P32BRC1202,P32BRC1242,P32BRC1263,
!
! FUNCTION BROWNE_INTERIOR_CVTS(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER)
! FUNCTION DNR24_BC_INTERIOR(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER, VOL_TYPE VARCHAR2 := 'CVTS')
! Valid species code 019, 094, 202, 242 and 263
! --VOL_TYPE Options:
! --  CVTS   Cubic foot volume of total stem from ground to tip of tree                    - CU000080
! --  CVT    Cubic foot volume from stump to tip of tree                                   - CU000081
! --  CV4    Cubic foot volume from stump to 4 inch top                                    - CU000082
! --  CV6    Cubic foot volume from stump to 6 inch top                                    - CU000083
! --  CV8    Cubic foot volume from stump to 8 inch top                                    - CU000084
! --  SV616  Scribner board foot volume from stump to 6 inch top using 16 foot scale rules - BD000060
! --  SV632  Scribner board foot volume from stump to 6 inch top using 32 foot scale rules - BD000061
! --  SV816  Scribner board foot volume from stump to 8 inch top using 16 foot scale rules - BD000062
! --  IV6    International board foot volume from stump to 6 inch top foot scale rules     - BD000063
! --  IV8    International board foot volume from stump to 8 inch top foot scale rules     - BD000064
! NVEL EQUATION NUMBER
! P16BRI0019,P16BRI0094,P16BRI0202,P16BRI0242,P16BRI0263,
! P32BRI0019,P32BRI0094,P32BRI0202,P32BRI0242,P32BRI0263,
!
! FUNCTION BROWNE_CVTS(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER)
! FUNCTION DNR24_BC(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER, VOL_TYPE VARCHAR2 := 'CVTS')
! Valid species code 42,73,108,119,122,312,351,370,746,747
! --VOL_TYPE Options:
! --  CVTS   Cubic foot volume of total stem from ground to tip of tree                    - CU000085
! --  CVT    Cubic foot volume from stump to tip of tree                                   - CU000086
! --  CV4    Cubic foot volume from stump to 4 inch top                                    - CU000087
! --  CV6    Cubic foot volume from stump to 6 inch top                                    - CU000088
! --  CV8    Cubic foot volume from stump to 8 inch top                                    - CU000089
! --  SV616  Scribner board foot volume from stump to 6 inch top using 16 foot scale rules - BD000065
! --  SV632  Scribner board foot volume from stump to 6 inch top using 32 foot scale rules - BD000066
! --  SV816  Scribner board foot volume from stump to 8 inch top using 16 foot scale rules - BD000067
! --  IV6    International board foot volume from stump to 6 inch top foot scale rules     - BD000068
! --  IV8    International board foot volume from stump to 8 inch top foot scale rules     - BD000069
! NVEL EQUATION NUMBER
! P16BRO0042,P16BRO0073,P16BRO0108,P16BRO0119,P16BRO0122,
! P16BRO0312,P16BRO0351,P16BRO0370,P16BRO0746,P16BRO0747,
! P32BRO0042,P32BRO0073,P32BRO0108,P32BRO0119,P32BRO0122,
! P32BRO0312,P32BRO0351,P32BRO0370,P32BRO0746,P32BRO0747,
!
! NOTES ON NVEL EQUATION NUMBER:
! P16BR@#***
! P32BR@#***
! WHERE 16 = 16 FOOT LOG, 32 = 32 FOOT LOG
!       @ = O (NO LOCATION SPECIFICATION), @ = C (COSTAL BC), @ = I (INTERIOR BC)
!       # = 0 (NO AGE SPECIFICATION), # = 1 (OLD GROWTH > 140 YEARS), # = 2 (YOUNG, < 140 YEAR)
!       *** = VALIDATE SPECIES CODE (012, 098, 202, 242 and 263)
! P16BRC1011,P16BRC1098,P16BRC1202,P16BRC1242,P16BRC1263,
! P32BRC1011,P32BRC1098,P32BRC1202,P32BRC1242,P32BRC1263,
! P16BRC2011,P16BRC2098,P16BRC2202,P16BRC2242,P16BRC2263,
! P32BRC2011,P32BRC2098,P32BRC2202,P32BRC2242,P32BRC2263,
! P16BRI0019,P16BRI0094,P16BRI0202,P16BRI0242,P16BRI0263,
! P32BRI0019,P32BRI0094,P32BRI0202,P32BRI0242,P32BRI0263,
! P16BRO0042,P16BRO0073,P16BRO0108,P16BRO0119,P16BRO0122,
! P16BRO0312,P16BRO0351,P16BRO0370,P16BRO0746,P16BRO0747,
! P32BRO0042,P32BRO0073,P32BRO0108,P32BRO0119,P32BRO0122,
! P32BRO0312,P32BRO0351,P32BRO0370,P32BRO0746,P32BRO0747,

      SUBROUTINE BROWNE_DNR24(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,ERRFLG)
      CHARACTER*10 VOLEQ
      REAL DBHOB,HTTOT,VOL(15),BFMIND,DBH,HT,CVTS,CV4,MTOPP
      CHARACTER*4 CVTYPE
      INTEGER SPN,ERRFLG,LOGLEN,VOLSP1(5),VOLSP2(5),VOLSP0(10)
      INTEGER I,J,IDX
      REAL COSTAL1(5,4),COSTAL2(5,4),INTERIOR(5,4),COEF0(10,4)
      REAL A1,A2,A3
      DATA VOLSP1/11, 98, 202, 242, 263/
      DATA VOLSP2/19, 94, 202, 242, 263/
! --Coastal Greater than 140 years old
      DATA ((COSTAL1(J,I), I=1,4), J=1,5) /       
     &  11,-2.575642,1.806775,1.094665,
     &  98,-2.700574,1.754171,1.164531,
     & 202,-2.712153,1.659012,1.195715,
     & 242,-2.379642,1.6823,1.039712,
     & 263,-2.663834,1.79023,1.124873/
! --Coastal less than 140 years old
      DATA ((COSTAL2(J,I), I=1,4), J=1,5) /       
     & 11,-2.575642,1.806775,1.094665,
     & 98,-2.550299,1.835678,1.042599,
     & 202,-2.658025,1.739925,1.133187,
     & 242,-2.441193,1.720761,1.049976,
     & 263,-2.702922,1.84268,1.123661/
! --Interior
      DATA ((INTERIOR(J,I), I=1,4), J=1,5) / 
     & 19,-2.502332,1.864963,1.004903,
     & 94,-2.539944,1.841226,1.034051,
     & 202,-2.734532,1.739418,1.166033,
     & 242,-2.464614,1.701993,1.067038,
     & 263,-2.571619,1.96971,0.977003/
      DATA VOLSP0/42,73,108,119,122,312,351,370,746,747/
! --Species that were not stratafied by age or location
      DATA ((COEF0(J,I), I=1,4), J=1,10) / 
     & 42,-2.454348,1.741044,1.058437,
     & 73,-2.624325,1.847123,1.044007,
     & 108,-2.615591,1.847504,1.085772,
     & 119,-2.480145,1.867286,0.994351,
     & 122,-2.729937,1.909478,1.085681,
     & 312,-2.770324,1.885813,1.119043,
     & 351,-2.672775,1.920617,1.074024,
     & 370,-2.757813,1.911681,1.105403,
     & 746,-2.63536,1.946034,1.024793,
     & 747,-2.945047,1.803973,1.238853/      
      V(DBH,HT,A1,A2,A3) = 10**A1*DBH**A2*HT**A3     
      READ(VOLEQ(8:10),'(I3)')SPN
      IDX = 0
      VOL = 0.0
      IF(VOLEQ(6:7).EQ.'C1')THEN
!     COSTAL OLD GROWTH
        DO I=1,5
          IF(COSTAL1(I,1).EQ.SPN)THEN
            IDX = I
            A1 = COSTAL1(I,2)
            A2 = COSTAL1(I,3)
            A3 = COSTAL1(I,4)
          ENDIF
        END DO
      ELSEIF(VOLEQ(6:7).EQ.'C2')THEN
!     COSTAL YOUNG GROWTH
        DO I=1,5
          IF(COSTAL2(I,1).EQ.SPN)THEN
            IDX = I
            A1 = COSTAL2(I,2)
            A2 = COSTAL2(I,3)
            A3 = COSTAL2(I,4)
          ENDIF
        END DO
      ELSEIF(VOLEQ(6:6).EQ.'I')THEN
!     INTERIOR
        DO I=1,5
          IF(INTERIOR(I,1).EQ.SPN)THEN
            IDX = I
            A1 = INTERIOR(I,2)
            A2 = INTERIOR(I,3)
            A3 = INTERIOR(I,4)
          ENDIF
        END DO
      ELSEIF(VOLEQ(6:7).EQ.'O0'.OR.VOLEQ(6:7).EQ.'00'
     & .OR.VOLEQ(6:7).EQ.'OO')THEN
!     Species that were not stratafied by age or location
        DO I=1,10
          IF(NINT(COEF0(I,1)).EQ.SPN)THEN
            IDX = I
            A1 = COEF0(I,2)
            A2 = COEF0(I,3)
            A3 = COEF0(I,4)
          ENDIF
        END DO
      ENDIF
      IF(IDX.EQ.0)THEN
        ERRFLG = 6
        RETURN
      ENDIF
      DBH = DBHOB
      HT = HTTOT
      CVTS = V(DBH,HT,A1,A2,A3)  
      CVTYPE = 'CVTS'
      CV4=0.0
      CALL DNR24_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,CVTS,CVTYPE,VOL,
     & CV4,ERRFLG)   
      RETURN   
      END SUBROUTINE BROWNE_DNR24
! ----------------------------------------------------------------------
!--Bell, John F.; D. D. Marshall and G. P. Johnson
!--Tarif Tables for Mountian Hemlock, July 1981
!--OSU Research Bulletin 35
!--Deschutes N.F. data
! FUNCTION TSME_BELL_CVTS(DBH IN NUMBER, HT IN NUMBER)
! FUNCTION TSME_DNR24_BELL(DBH IN NUMBER, HT IN NUMBER, VOL_TYPE VARCHAR2 := 'CVTS')
! --  CVTS   Cubic foot volume of total stem from ground to tip of tree                     -CU264001
! --  CVT    Cubic foot volume from stump to tip of tree                                    -CU264002
! --  CV4    Cubic foot volume from stump to 4 inch top                                     -CU264003
! --  CV6    Cubic foot volume from stump to 6 inch top                                     -CU264004
! --  CV8    Cubic foot volume from stump to 8 inch top                                     -CU264005
! --  SV616  Scribner board foot volume from stump to 6 inch top using 16 foot scale rules  -BD264001
! --  SV632  Scribner board foot volume from stump to 6 inch top using 32 foot scale rules  -BD264002
! --  SV816  Scribner board foot volume from stump to 8 inch top using 16 foot scale rules  -BD264003
! --  IV6    International board foot volume from stump to 6 inch top foot scale rules      -BD264004
! --  IV8    International board foot volume from stump to 8 inch top foot scale rules      -BD264005
! NVEL EQUATION NUMBER:
! P16BEL0264, P32BEL0264
      SUBROUTINE BELL_DNR24(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,ERRFLG)
      CHARACTER*10 VOLEQ
      REAL DBHOB,HTTOT,VOL(15),BFMIND,DBH,HT,CVTS,CV4,MTOPP
      CHARACTER*4 CVTYPE
      INTEGER SPN,ERRFLG,I,J,IDX
      DBH = DBHOB
      HT = HTTOT
      CVTS = .001106485*DBH**1.8140497*HT**1.2744923
      CVTYPE = 'CVTS'
      CV4=0.0
      CALL DNR24_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,CVTS,CVTYPE,VOL,
     & CV4,ERRFLG)   
      RETURN 
      END SUBROUTINE BELL_DNR24
! ----------------------------------------------------------------------  
!--King, James T. and Turnbull K. J.
!--Weyerhaeuser Douglas fir
! FUNCTION PSME_KING_CVTS(DBH IN NUMBER, HT IN NUMBER)
! FUNCTION PSME_DNR24_KING(DBH IN NUMBER, HT IN NUMBER, VOL_TYPE VARCHAR2 := 'CVTS')    
!--VOL_TYPE Options:
!--  CVTS   Cubic foot volume of total stem from ground to tip of tree                     - CU202018
!--  CVT    Cubic foot volume from stump to tip of tree                                    - CU202019
!--  CV4    Cubic foot volume from stump to 4 inch top                                     - CU202020
!--  CV6    Cubic foot volume from stump to 6 inch top                                     - CU202021
!--  CV8    Cubic foot volume from stump to 8 inch top                                     - CU202022
!--  SV616  Scribner board foot volume from stump to 6 inch top using 16 foot scale rules  - BD202013
!--  SV632  Scribner board foot volume from stump to 6 inch top using 32 foot scale rules  - BD202014
!--  SV816  Scribner board foot volume from stump to 8 inch top using 16 foot scale rules  - BD202015
!--  IV6    International board foot volume from stump to 6 inch top foot scale rules      - BD202016
!--  IV8    International board foot volume from stump to 8 inch top foot scale rules      - BD202017
! NVEL EQUATION NUMBER:
! P16KIN0202, P32KIN0202
      SUBROUTINE KING_DNR24(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,ERRFLG)
      CHARACTER*10 VOLEQ
      REAL DBHOB,HTTOT,VOL(15),BFMIND,DBH,HT,CVTS,CV4,MTOPP
      CHARACTER*4 CVTYPE
      INTEGER SPN,ERRFLG
      READ(VOLEQ(8:10),'(I3)')SPN
      DBH = DBHOB
      HT = HTTOT
      IF(SPN.NE.202)THEN
        ERRFLG = 6 
        RETURN
      ENDIF       
      CVTS = 10**(-3.21809+0.04948*LOG10(DBH)*LOG10(HT)
     &   -0.15664*LOG10(DBH)**2
     &   + 2.02132*LOG10(DBH)+ 1.63408*LOG10(HT)- 0.16185*LOG10(HT)**2)
      CVTYPE = 'CVTS'
      CV4 = 0.0
      CALL DNR24_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,CVTS,CVTYPE,VOL,
     & CV4,ERRFLG)   
      RETURN 
      END SUBROUTINE KING_DNR24
! ----------------------------------------------------------------------      
!--Summerfield, Edward R.
!--Letter Dated November 7, 1980
!--For 122 and 202
! FUNCTION SUMMERFIELD_CVTS(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER)
! FUNCTION DNR24_SUMMERFIELD(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER, VOL_TYPE VARCHAR2 := 'CVTS')
!--VOL_TYPE Options:
!--  CVTS   Cubic foot volume of total stem from ground to tip of tree                    - CU000090
!--  CVT    Cubic foot volume from stump to tip of tree                                   - CU000091
!--  CV4    Cubic foot volume from stump to 4 inch top                                    - CU000092
!--  CV6    Cubic foot volume from stump to 6 inch top                                    - CU000093
!--  CV8    Cubic foot volume from stump to 8 inch top                                    - CU000094
!--  SV616  Scribner board foot volume from stump to 6 inch top using 16 foot scale rules - BD000070
!--  SV632  Scribner board foot volume from stump to 6 inch top using 32 foot scale rules - BD000071
!--  SV816  Scribner board foot volume from stump to 8 inch top using 16 foot scale rules - BD000072
!--  IV6    International board foot volume from stump to 6 inch top foot scale rules     - BD000073
!--  IV8    International board foot volume from stump to 8 inch top foot scale rules     - BD000074
! NVEL EQUATION NUMBER:
! P16SMF0122, P32SMF0122,P16SMF0202, P32SMF0202
      SUBROUTINE SUMMERFIELD_DNR24(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,
     & ERRFLG)
      CHARACTER*10 VOLEQ
      REAL DBHOB,HTTOT,VOL(15),BFMIND,DBH,HT,CVTS,CV4,MTOPP
      CHARACTER*4 CVTYPE
      INTEGER SPN,ERRFLG
      READ(VOLEQ(8:10),'(I3)')SPN
      DBH = DBHOB
      HT = HTTOT
      IF(SPN.EQ.122)THEN
        CVTS = EXP(- 8.521558+ 1.977243*LOG(DBH)- 0.105288*LOG(HT)**2
     &        + 136.0489/HT**2+1.99546 *LOG(HT))
      ELSEIF(SPN.EQ.202)THEN
        CVTS = EXP(-6.110493+1.81306*LOG(DBH)+ 1.083884*LOG(HT))
      ELSE
        ERRFLG = 6 
        RETURN
      ENDIF       
      CVTYPE = 'CVTS'
      CV4 = 0.0
      CALL DNR24_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,CVTS,CVTYPE,VOL,
     & CV4,ERRFLG)   
      RETURN 
      END SUBROUTINE SUMMERFIELD_DNR24
! ----------------------------------------------------------------------      
!--Chambers, Charles J. and Flotz, Bruce W.
!--Washington State DNR Note 27
!--Cubic foot volume of stem from ground to tip
!--Western Hemlock
! FUNCTION TSHE_CHAMBERS_CVTS(DBH IN NUMBER, HT IN NUMBER)
! FUNCTION TSHE_DNR24_CHAMBERS(DBH IN NUMBER, HT IN NUMBER, VOL_TYPE VARCHAR2 := 'CVTS')
!--VOL_TYPE Options:
!--  CVTS   Cubic foot volume of total stem from ground to tip of tree                    -CU263002
!--  CVT    Cubic foot volume from stump to tip of tree                                   -CU263003
!--  CV4    Cubic foot volume from stump to 4 inch top                                    -CU263004
!--  CV6    Cubic foot volume from stump to 6 inch top                                    -CU263005
!--  CV8    Cubic foot volume from stump to 8 inch top                                    -CU263006
!--  SV616  Scribner board foot volume from stump to 6 inch top using 16 foot scale rules -BD263002
!--  SV632  Scribner board foot volume from stump to 6 inch top using 32 foot scale rules -BD263003
!--  SV816  Scribner board foot volume from stump to 8 inch top using 16 foot scale rules -BD263004
!--  IV6    International board foot volume from stump to 6 inch top foot scale rules     -BD263005
!--  IV8    International board foot volume from stump to 8 inch top foot scale rules     -BD263006
! NVEL EQUATION NUMBER:
! P16CHA0263, P32CHA0263
      SUBROUTINE CHAMBERS_DNR24(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,
     & ERRFLG)
      CHARACTER*10 VOLEQ
      REAL DBHOB,HTTOT,VOL(15),BFMIND,DBH,HT,CVTS,CV4,MTOPP
      CHARACTER*4 CVTYPE
      INTEGER SPN,ERRFLG
      READ(VOLEQ(8:10),'(I3)')SPN
      DBH = DBHOB
      HT = HTTOT
      IF(SPN.EQ.263)THEN
        CVTS = 10**(-2.72170+ 2.00857*LOG10(DBH)+1.08620*LOG10(HT)
     &       -.00568*DBH)
      ELSE
        ERRFLG = 6 
        RETURN
      ENDIF       
      CVTYPE = 'CVTS'
      CV4 = 0.0
      CALL DNR24_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,CVTS,CVTYPE,VOL,
     & CV4,ERRFLG)   
      RETURN 
      END SUBROUTINE CHAMBERS_DNR24
! ----------------------------------------------------------------------      
!--Washington State DNR Report 24 and PNW-420, Chittester and MacLean, 1984
!--Tarif volume conversion from Chittester and MacLean CVTS to other types
! FUNCTION JUOC_DNR24_CHT(DBH IN NUMBER, HT IN NUMBER, VOL_TYPE VARCHAR2 := 'CVTS')
!--VOL_TYPE Options:
!--  CVTS   Cubic foot volume of total stem from ground to tip of tree                    - CU064009
!--  CVT    Cubic foot volume from stump to tip of tree                                   - CU064011
!--  CV4    Cubic foot volume from stump to 4 inch top                                    - CU064012
!--  CV6    Cubic foot volume from stump to 6 inch top                                    - CU064013
!--  CV8    Cubic foot volume from stump to 8 inch top                                    - CU064014
!--  SV616  Scribner board foot volume from stump to 6 inch top using 16 foot scale rules - BD064001
!--  SV632  Scribner board foot volume from stump to 6 inch top using 32 foot scale rules - BD064002
!--  SV816  Scribner board foot volume from stump to 8 inch top using 16 foot scale rules - BD064003
!--  IV6    International board foot volume from stump to 6 inch top foot scale rules     - BD064004
!--  IV8    International board foot volume from stump to 8 inch top foot scale rules     - BD064005
! NVEL EQUATION NUMBER:
! P16CHT0064, P32CHT0064
      SUBROUTINE CHT_DNR24(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,ERRFLG)
      CHARACTER*10 VOLEQ
      REAL DBHOB,HTTOT,VOL(15),BFMIND,DBH,HT,CVTS,CV4,TOPD,MTOPP
      CHARACTER*4 CVTYPE
      INTEGER SPN,ERRFLG,BFPFLG,CUPFLG
      READ(VOLEQ(8:10),'(I3)')SPN
      DBH = DBHOB
      HT = HTTOT
      IF(SPN.EQ.64)THEN
        TOPD = 4.0
        BFPFLG = 1
        CUPFLG = 1
        CALL R5HARV(VOLEQ,DBHOB,HTTOT,TOPD,VOL,BFPFLG,CUPFLG,ERRFLG)
        CVTS = VOL(1)
        CV4 = VOL(4)
      ELSE
        ERRFLG = 6 
        RETURN
      ENDIF       
      CVTYPE = 'CVTS'
      CALL DNR24_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,CVTS,CVTYPE,VOL,
     & CV4,ERRFLG)   
      RETURN 
      END SUBROUTINE CHT_DNR24
! ----------------------------------------------------------------------   
!--Pillsbury, Norman H. and Micheal L. Kirkely
!--Equations for Total, Wood and Saw-Log Volume for 13 California Hardwoods
!--PNW-414, June 1984
!--Cubic foot wood volume from 1 foot stump to 9 inch top DOB
! Valid species code 312,361,431,631,801,805,807,811,815,818,821,839,981
! FUNCTION PILLSBURY_CV9(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER, QUAL IN NUMBER := NULL)    
! FUNCTION DNR24_PILLSBURY(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER, VOL_TYPE VARCHAR2 := 'CVTS', QUAL IN NUMBER := NULL)
!--VOL_TYPE Options:
!--  CVTS   Cubic foot volume of total stem from ground to tip of tree                    - CU000095
!--  CVT    Cubic foot volume from stump to tip of tree                                   - CU000096
!--  CV4    Cubic foot volume from stump to 4 inch top                                    - CU000097
!--  CV6    Cubic foot volume from stump to 6 inch top                                    - CU000098
!--  CV8    Cubic foot volume from stump to 8 inch top                                    - CU000099
!--  SV616  Scribner board foot volume from stump to 6 inch top using 16 foot scale rules - BD000075
!--  SV632  Scribner board foot volume from stump to 6 inch top using 32 foot scale rules - BD000076
!--  SV816  Scribner board foot volume from stump to 8 inch top using 16 foot scale rules - BD000077
!--  IV6    International board foot volume from stump to 6 inch top foot scale rules     - BD000078
!--  IV8    International board foot volume from stump to 6 inch top foot scale rules     - BD000079
! NVEL EQUATION NUMBER:
! P16PIL0312,P16PIL0361,P16PIL0431,P16PIL0631,P16PIL0801,P16PIL0805,P16PIL0807,
! P16PIL0811,P16PIL0815,P16PIL0818,P16PIL0821,P16PIL0839,P16PIL0981,
! P32PIL0312,P32PIL0361,P32PIL0431,P32PIL0631,P32PIL0801,P32PIL0805,P32PIL0807,
! P32PIL0811,P32PIL0815,P32PIL0818,P32PIL0821,P32PIL0839,P32PIL0981,
      SUBROUTINE PILLSBURY_DNR24(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,
     & ERRFLG)
      CHARACTER*10 VOLEQ
      REAL DBHOB,HTTOT,VOL(15),BFMIND,DBH,HT,CV,CV4,TOPD,MTOPP
      CHARACTER*4 CVTYPE
      INTEGER SPN,ERRFLG,BFPFLG,CUPFLG,VOLSP(13),CNT,IDX
      DATA VOLSP/312,361,431,631,801,805,807,811,815,818,821,839,981/
      READ(VOLEQ(8:10),'(I3)')SPN
      DBH = DBHOB
      HT = HTTOT
      CNT = 13
      IDX = 0
      CALL SEARCH_SP(CNT,VOLSP,SPN,IDX,ERRFLG)
      IF(IDX.LE.0) THEN
        ERRFLG = 6
        RETURN
      ENDIF
      IF(IDX.GT.0)THEN
        TOPD = 9.0
        BFPFLG = 1
        CUPFLG = 1
        CALL R5HARV(VOLEQ,DBHOB,HTTOT,TOPD,VOL,BFPFLG,CUPFLG,ERRFLG)
        CV = VOL(4)
      ENDIF       
      CVTYPE = 'CV8'
      CV4 = 0.0
      CALL DNR24_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,CV,CVTYPE,VOL,
     & CV4,ERRFLG)   
      RETURN 
      END SUBROUTINE PILLSBURY_DNR24
! ----------------------------------------------------------------------      
!--MacLean, Colin D. --Letter Dated January 27, 1983
!--Developed from mixed hardwoods - used for eucalyptus
!--Cubic foot volume of stem from ground to tip
!--Tarif volume conversion from MacLean CVTS to other types
! FUNCTION HARDWOOD_MAC_CVTS(DBH IN NUMBER, HT IN NUMBER)
! FUNCTION HARDWOOD_DNR24_MAC(DBH IN NUMBER, HT IN NUMBER, VOL_TYPE VARCHAR2 := 'CVTS')
!--VOL_TYPE Options:
!--  CVTS   Cubic foot volume of total stem from ground to tip of tree                    -CU998001
!--  CVT    Cubic foot volume from stump to tip of tree                                   -CU998002
!--  CV4    Cubic foot volume from stump to 4 inch top                                    -CU998003
!--  CV6    Cubic foot volume from stump to 6 inch top                                    -CU998004
!--  CV8    Cubic foot volume from stump to 8 inch top                                    -CU998005
!--  SV616  Scribner board foot volume from stump to 6 inch top using 16 foot scale rules -BD998001
!--  SV632  Scribner board foot volume from stump to 6 inch top using 32 foot scale rules -BD998002
!--  SV816  Scribner board foot volume from stump to 8 inch top using 16 foot scale rules -BD998003
!--  IV6    International board foot volume from stump to 6 inch top foot scale rules     -BD998004
!--  IV8    International board foot volume from stump to 8 inch top foot scale rules     -BD998005
! NVEL EQUATION NUMBER:
! P16MAC0998, P32MAC0998
      SUBROUTINE HARDWOOD_MAC_DNR24(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,
     & ERRFLG)
      CHARACTER*10 VOLEQ
      REAL DBHOB,HTTOT,VOL(15),BFMIND,DBH,HT,CVTS,CV4,MTOPP
      CHARACTER*4 CVTYPE
      INTEGER SPN,ERRFLG
      READ(VOLEQ(8:10),'(I3)')SPN
      DBH = DBHOB
      HT = HTTOT
      CVTS = .0016144*DBH**2*HT
      CVTYPE = 'CVTS'
      CV4 = 0.0
      CALL DNR24_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,CVTS,CVTYPE,VOL,
     & CV4,ERRFLG)   
      RETURN 
      END SUBROUTINE HARDWOOD_MAC_DNR24
! ----------------------------------------------------------------------      
!--MacLean, C.D. and J.M. Berger
!--Softwood Tree Volume Equations for Major California Species
!--PNW-266
!--Washington State DNR Report 24 and PNW-266, MacLean and Berger, 1976
!--Tarif volume conversion from MacLean CVT4 to other types
!--E. Oregon and N. California sample
! FUNCTION PSME_DNR24_MAC(DBH IN NUMBER, HT IN NUMBER, VOL_TYPE VARCHAR2 := 'CVTS')
! FUNCTION PIPO_DNR24_MAC(DBH IN NUMBER, HT IN NUMBER, VOL_TYPE VARCHAR2 := 'CVTS')
! FUNCTION PILA_DNR24_MAC(DBH IN NUMBER, HT IN NUMBER, VOL_TYPE VARCHAR2 := 'CVTS')
! FUNCTION PICO_DNR24_MAC(DBH IN NUMBER, HT IN NUMBER, VOL_TYPE VARCHAR2 := 'CVTS')
! FUNCTION ABCO_DNR24_MAC(DBH IN NUMBER, HT IN NUMBER, VOL_TYPE VARCHAR2 := 'CVTS')
! FUNCTION ABMA_DNR24_MAC(DBH IN NUMBER, HT IN NUMBER, VOL_TYPE VARCHAR2 := 'CVTS')
! FUNCTION CADE27_DNR24_MAC(DBH IN NUMBER, HT IN NUMBER, VOL_TYPE VARCHAR2 := 'CVTS')
!--VOL_TYPE Options:
!--  CVTS   Cubic foot volume of total stem from ground to tip of tree                    - CU202013 CU122024 CU119003 CU108007 CU015008 CU020003 CU081003
!--  CVT    Cubic foot volume from stump to tip of tree                                   - CU202014 CU122025 CU119004 CU108008 CU015009 CU020004 CU081004
!--  CV4    Cubic foot volume from stump to 4 inch top                                    - CU202015 CU122026 CU119005 CU108009 CU015010 CU020005 CU081005
!--  CV6    Cubic foot volume from stump to 6 inch top                                    - CU202016 CU122027 CU119006 CU108010 CU015011 CU020006 CU081006
!--  CV8    Cubic foot volume from stump to 8 inch top                                    - CU202017 CU122028 CU119007 CU108011 CU015012 CU020007 CU081007
!--  SV616  Scribner board foot volume from stump to 6 inch top using 16 foot scale rules - BD202008 BD122018 BD119003 BD108008 BD015005 BD020003 BD081003
!--  SV632  Scribner board foot volume from stump to 6 inch top using 32 foot scale rules - BD202009 BD122019 BD119004 BD108009 BD015006 BD020004 BD081004
!--  SV816  Scribner board foot volume from stump to 8 inch top using 16 foot scale rules - BD202010 BD122020 BD119005 BD108010 BD015007 BD020005 BD081005
!--  IV6    International board foot volume from stump to 6 inch top foot scale rules     - BD202011 BD122021 BD119006 BD108011 BD015008 BD020006 BD081006
!--  IV8    International board foot volume from stump to 8 inch top foot scale rules     - BD202012 BD122022 BD119007 BD108012 BD015009 BD020007 BD081007
! valid species code 015, 020, 081, 108, 116, 117,119,122,202,   
! NVEL EQUATION NUMBER:
! P16MAC0015, P16MAC0020, P16MAC0081, P16MAC0108, P16MAC0116, P16MAC0117, P16MAC0119, P16MAC0122, P16MAC0202
! P32MAC0015, P32MAC0020, P32MAC0081, P32MAC0108, P32MAC0116, P32MAC0117, P32MAC0119, P32MAC0122, P32MAC0202
! P16MAC0998, P32MAC0998
      SUBROUTINE MAC_DNR24(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,
     & ERRFLG)
      CHARACTER*10 VOLEQ,VOLEQTMP
      REAL DBHOB,HTTOT,VOL(15),BFMIND,MTOPP
      INTEGER ERRFLG,BFPFLG,CUPFLG,SPN
      ERRFLG = 0
      READ(VOLEQ(8:10),'(I3)')SPN
      IF(SPN.EQ.998)THEN
        CALL HARDWOOD_MAC_DNR24(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,
     & ERRFLG)
      ELSE
        BFPFLG = 1
        CUPFLG = 1
        VOLEQTMP = '5'//VOLEQ(2:10)
        CALL PNWTARIF(VOLEQTMP,DBHOB,HTTOT,MTOPP,VOL,BFPFLG,CUPFLG,
     &   ERRFLG)
      ENDIF
      RETURN 
      END SUBROUTINE MAC_DNR24
! ----------------------------------------------------------------------      
!--Curtis, Robert O. David Bruce and Caryanne VanCoevering
!--Taper Tables for Red Alder, PNW-56, 1968
!--Washington State DNR Report 24 and Curtis PNW-56
!--Tarif volume conversion from Curtis CVT to other types
! FUNCTION ALRU_CURTIS_CVT(DBH IN NUMBER, HT IN NUMBER)
! FUNCTION ALRU_DNR24_CURTIS(DBH IN NUMBER, HT IN NUMBER, VOL_TYPE VARCHAR2 := 'CVTS')
!--VOL_TYPE Options:
!--  CVTS   Cubic foot volume of total stem from ground to tip of tree                    - CU351002
!--  CVT    Cubic foot volume from stump to tip of tree                                   - CU351003
!--  CV4    Cubic foot volume from stump to 4 inch top                                    - CU351004
!--  CV6    Cubic foot volume from stump to 6 inch top                                    - CU351005
!--  CV8    Cubic foot volume from stump to 8 inch top                                    - CU351006
!--  SV616  Scribner board foot volume from stump to 6 inch top using 16 foot scale rules - BD351003
!--  SV632  Scribner board foot volume from stump to 6 inch top using 32 foot scale rules - BD351004
!--  SV816  Scribner board foot volume from stump to 8 inch top using 16 foot scale rules - BD351005
!--  IV6    International board foot volume from stump to 6 inch top foot scale rules     - BD351006
!--  IV8    International board foot volume from stump to 6 inch top foot scale rules     - BD351007
! NVEL EQUATION NUMBER:
! P16CUR0351, P32CUR0351
      SUBROUTINE CURTIS_DNR24(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,
     & ERRFLG)
      CHARACTER*10 VOLEQ,VOLEQTMP
      REAL DBHOB,HTTOT,VOL(15),BFMIND,MTOPP
      INTEGER ERRFLG,BFPFLG,CUPFLG
      ERRFLG = 0
      BFPFLG = 1
      CUPFLG = 1
      VOLEQTMP = '6'//VOLEQ(2:10)
      CALL PNWTARIF(VOLEQTMP,DBHOB,HTTOT,MTOPP,VOL,BFPFLG,CUPFLG,ERRFLG)
      RETURN 
      END SUBROUTINE CURTIS_DNR24
! ---------------------------------------------------------------------
!--Krumland B. E. and L. C. Wensel
!--Preliminary Young Growth Volume Tables for Coastal California Conifers
!--Reasearch Note No. 1, December 1975
!--Co-op Redwood Yield Paper
!--Washington State DNR Report 24
!--Tarif volume conversion from Krumland CVT to other types
!--Valid for 211, 202, 263, 017 and 098
!--Note: According to publication Krumand volume is CVT (stump not included) but
!--      this program treats it as CVTS.
! FUNCTION KRUMLAND_CVT(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER)
! FUNCTION DNR24_KRUMLAND(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER, VOL_TYPE VARCHAR2 := 'CVTS')
!--VOL_TYPE Options:
!--  CVTS   Cubic foot volume of total stem from ground to tip of tree                    - CU000100
!--  CVT    Cubic foot volume from stump to tip of tree                                   - CU000101
!--  CV4    Cubic foot volume from stump to 4 inch top                                    - CU000102
!--  CV6    Cubic foot volume from stump to 6 inch top                                    - CU000103
!--  CV8    Cubic foot volume from stump to 8 inch top                                    - CU000104
!--  SV616  Scribner board foot volume from stump to 6 inch top using 16 foot scale rules - BD000081
!--  SV632  Scribner board foot volume from stump to 6 inch top using 32 foot scale rules - BD000082
!--  SV816  Scribner board foot volume from stump to 8 inch top using 16 foot scale rules - BD000083
!--  IV6    International board foot volume from stump to 6 inch top foot scale rules     - BD000084
!--  IV8    International board foot volume from stump to 8 inch top foot scale rules     - BD000085
! NVEL EQUATION NUMBER:
! P16KRU0017, P16KRU0098, P16KRU0202, P16KRU0211, P16KRU0263, 
! P32KRU0017, P32KRU0098, P32KRU0202, P32KRU0211, P32KRU0263, 
      SUBROUTINE KRUMLAND_DNR24(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,
     & ERRFLG)
      CHARACTER*10 VOLEQ
      REAL DBHOB,HTTOT,VOL(15),BFMIND,DBH,HT,CVTS,CV4,MTOPP
      CHARACTER*4 CVTYPE
      INTEGER SPN,ERRFLG
!      READ(VOLEQ(8:10),'(I3)')SPN
!      DBH = DBHOB
!      HT = HTTOT
!      IF(SPN.EQ.211)THEN
!        CVTS = EXP(-6.2597+1.9967*LOG(DBH)+.9642*LOG(HT))
!      ELSEIF(SPN.EQ.202)THEN
!        CVTS = EXP(-6.5193+1.7151*LOG(DBH)+1.2274*LOG(HT))
!      ELSEIF(SPN.EQ.17.OR.SPN.EQ.98.OR.SPN.EQ.263)THEN
!        CVTS = EXP(-6.7013+1.7022*LOG(DBH)+1.2979*LOG(HT))
!      ELSE
!        ERRFLG = 6 
!        RETURN
!      ENDIF       
      CALL KRUMLAND_VOL(VOLEQ,DBHOB,HTTOT,BFMIND,VOL,ERRFLG)
      CVTS = VOL(1)
      CVTYPE = 'CVTS'
      CV4 = 0.0
      CALL DNR24_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,CVTS,CVTYPE,VOL,
     & CV4,ERRFLG)   
      RETURN 
      END SUBROUTINE KRUMLAND_DNR24
! ---------------------------------------------------------------------
!--Krumland B. E. and L. C. Wensel
!--Preliminary Young Growth Volume Tables for Coastal California Conifers
!--Reasearch Note No. 1, December 1975
!--Co-op Redwood Yield Paper
!--Valid for 211, 202, 263, 017 and 098
! FUNCTION KRUMLAND_CVT(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER)
! FUNCTION KRUMLAND_SV6(SPN IN NUMBER, DBH IN NUMBER, HT IN NUMBER)
! CU000099 CVT
! BD000080 SV6
! NVEL EQUATION NUMBER:
! P02KRU1017, P02KRU1098, P02KRU1202, P02KRU1211, P02KRU1263, 
      SUBROUTINE KRUMLAND_VOL(VOLEQ,DBHOB,HTTOT,BFMIND,VOL,ERRFLG)
       CHARACTER*10 VOLEQ
      REAL DBHOB,HTTOT,VOL(15),BFMIND,DBH,HT,CVT,CV4,SV6
      CHARACTER*4 CVTYPE
      INTEGER SPN,ERRFLG
      READ(VOLEQ(8:10),'(I3)')SPN
      DBH = DBHOB
      HT = HTTOT
      VOL = 0.0
      IF(SPN.EQ.211)THEN
        CVT = EXP(-6.2597+1.9967*LOG(DBH)+.9642*LOG(HT))
        SV6 = EXP(-8.1850+2.2985*LOG(DBH)+1.5114*LOG(HT))
      ELSEIF(SPN.EQ.202)THEN
        CVT = EXP(-6.5193+1.7151*LOG(DBH)+1.2274*LOG(HT))
        SV6 = EXP(-9.4524+1.8416*LOG(DBH)+2.0940*LOG(HT))
      ELSEIF(SPN.EQ.17.OR.SPN.EQ.98.OR.SPN.EQ.263)THEN
        CVT = EXP(-6.7013+1.7022*LOG(DBH)+1.2979*LOG(HT))
        SV6 = EXP(-8.4979+2.5173*LOG(DBH)+1.4980*LOG(HT))
      ELSE
        ERRFLG = 6 
        RETURN
      ENDIF       
      CV4 = CVT - 1.5
      VOL(1) = CVT
      VOL(4) = CV4
      IF(DBH.GE.BFMIND) VOL(2) = SV6
      RETURN
      END SUBROUTINE KRUMLAND_VOL
! ---------------------------------------------------------------------
! Krumland, B. E., Wensel, L. C. and Dye, J.B. 1977  
! Young Growth Volume Tables for Coastal California Conifers. 
! CO-OP Redwood Yield Research Project, Reasearch Note No. 3, March 1977. 
! Dept. of Forestry and Conversion, College of Natural Resources, Univ. of California, Berkeley, CA
! --Valid for 211, 202, 263, 017 and 098
! NIMS_VOL_R5610
! CU211001 FUNCTION SESE3_KRUM_CU(DBH IN NUMBER, THT IN NUMBER)
! BD211001 FUNCTION SESE3_KRUM_BD(DBH IN NUMBER, THT IN NUMBER) 
! CU202006 FUNCTION PSME_KRUM_CU(DBH IN NUMBER, THT IN NUMBER)
! BD202005 FUNCTION PSME_KRUM_BD(DBH IN NUMBER, THT IN NUMBER) 
! CU263001 FUNCTION TSHE_KRUM_CU(DBH IN NUMBER, THT IN NUMBER)
! BD263001 FUNCTION TSHE_KRUM_BD(DBH IN NUMBER, THT IN NUMBER) 
! NVEL EQUATION NUMBER:
! P02KRU2017, P02KRU2098, P02KRU2202, P02KRU2211, P02KRU2263, 
      SUBROUTINE KRUMLAND_VOL2(VOLEQ,DBHOB,HTTOT,BFMIND,VOL,ERRFLG)
      CHARACTER*10 VOLEQ
      REAL DBHOB,HTTOT,VOL(15),BFMIND,DBH,HT,CVT,CV4,SV6
      CHARACTER*4 CVTYPE
      INTEGER SPN,ERRFLG
!      REAL B0,B1,B2
!      V(DBH,HT,B0,B1,B2) = B0 * (DBH ** B1) * (HT ** B2)
      READ(VOLEQ(8:10),'(I3)')SPN
      DBH = DBHOB
      HT = HTTOT
      VOL = 0.0
      IF(SPN.EQ.211)THEN
        CVT = .001841*DBH**1.9678*HT**.9887
        SV6 = .001190*DBH**2.1931*HT**1.29
      ELSEIF(SPN.EQ.202)THEN
        CVT = .001697 * (DBH ** 1.6726) * (HT ** 1.2261)
        SV6 = .0019 * (DBH ** 1.8918) * (HT ** 1.422)
      ELSEIF(SPN.EQ.17.OR.SPN.EQ.98.OR.SPN.EQ.263)THEN
        CVT = .001045 * (DBH ** 1.6759) * (HT ** 1.3494)
        SV6 = .000423 * (DBH ** 1.8928) * (HT ** 1.76)
      ELSE
        ERRFLG = 6 
        RETURN
      ENDIF       
      VOL(1) = CVT
      IF(DBH.GE.BFMIND) VOL(2) = SV6
      RETURN
      END SUBROUTINE KRUMLAND_VOL2
      
! ---------------------------------------------------------------------
! This is combined the code from thefollowing functions
! GET_TARIF_FROM_CVTS
! GET_TARIF_FROM_CVT
! GET_TARIF_FROM_CV4
! GET_TARIF_FROM_CV8
! GET_CVTS_FROM_CV4
! DNR24_FROM_CV4
! DNR24_FROM_CVT
! DNR24_FROM_CV8
! DNR24
! DBR24_PNW266
      SUBROUTINE DNR24_VOL(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,CV,CVTYPE,VOL,
     &                     CV4,ERRFLG)
      CHARACTER*10 VOLEQ
      REAL DBHOB,HTTOT,VOL(15),BFMIND,DBH,HT,CV,MTOPP
      CHARACTER*4 CVTYPE
      INTEGER SPN,ERRFLG,LOGLEN
      REAL CV4,CV6,CV8,CVT,CVTS,TARIF,SV616,SV32,SV816,IV6,IV8
!     FORMULA FOR TARIF FROM DIFFERENT VOLUME TYPE
!     GET_TARIF_FROM_CVTS
      TRF0(CVTS,DBH) =(CVTS*.912733)/
     & ((1.033*(1.0+1.382937*EXP(-4.015292*(DBH/10.0))))
     &  *(.005454*DBH**2+.087266)-.174533)
!     GET_TARIF_FROM_CVT
      TRF1(CVT,DBH) = CVT*0.912733/((0.9679-0.1051*0.5523**(DBH-1.5))
     &        *((1.033*(1.0+1.382937*Exp(-4.015292*DBH/10.0)))
     &        *(.005454154*DBH**2+0.087266)-0.174533))
!     GET_TARIF_FROM_CV4
      TRF4(CV4,DBH) = CV4*.912733/(.005454*DBH**2+.087266)
!     GET_TARIF_FROM_CV8
      TRF8(CV8,DBH) = (CV8*0.912733)/((0.983-0.983*0.65**(DBH-8.6))
     & *(.005454154*DBH**2-0.087266))   
     
      READ(VOLEQ(8:10),'(I3)')SPN
      IF(MTOPP.LT.6.0) MTOPP = 6.0
      IF(BFMIND.LT.0.1)THEN
        IF(SPN.LT.300)THEN
          BFMIND = 9.0
        ELSE
          BFMIND = 11.0
        ENDIF
      ENDIF
      CVTS = 0.0
      CVT = 0.0
      CV6 = 0.0
      CV8 = 0.0
      TARIF = 0.0
      SV616 = 0.0
      SV632 = 0.0
      SV816 = 0.0
      IV6 = 0.0
      IV8 = 0.0
      DBH = DBHOB
      HT = HTTOT
      VOL = 0.0
      IF(CVTYPE.EQ.'CVTS')THEN
        CVTS = CV
        TARIF = TRF0(CVTS,DBH)
        IF(CV4.EQ.0.0) CV4 = TARIF*(.005454154*DBH**2-.087266)/.912733
        CVT = CVTS*(.9679-.1051*.5523**(DBH-1.5))
      ELSEIF(CVTYPE.EQ.'CVT')THEN
        CVT = CV
        TARIF = TRF1(CVT,DBH)
        IF(CV4.EQ.0.0) CV4 = TARIF*(.005454154*DBH**2-.087266)/.912733
        CVTS = TARIF*((1.033*(1.0+1.382937*Exp(-4.015292*(DBH /10.0))))*
     &       (.005454154*DBH**2 + 0.087266) - 0.174533) / 0.912733
      ELSEIF(CVTYPE.EQ.'CV4')THEN
        CV4 = CV
        TARIF = TRF4(CV4,DBH)
        CVTS = CV4*((1.033*(1.0+1.382937*Exp(-4.015292*(DBH/10.0))))*
     &    (.005454*DBH**2+0.087266)-0.174533)/(.005454*DBH**2-0.087266)
        CVT = TARIF*(0.9679-0.1051*0.5523**(DBH-1.5))*
     &   ((1.033*(1.0+1.382937*EXP(-4.015292*(DBH/10.0))))*
     &   (.005454*DBH**2+0.087266)-0.174533)/0.912733
      ELSEIF(CVTYPE.EQ.'CV8')THEN
        CV8 = CV
        TARIF = TRF8(CV8,DBH)
        CVT = CV8/(1.03361-1.59234/DBH-4667.04/DBH**4+(0.104498*HT)/
     &    DBH**2+5322.16/(DBH**3*HT))
        CV4 = CVT * (0.99875-43.336/DBH**3-124.717/DBH**4+(0.193437*HT)
     &         /DBH**3+479.83/(DBH**3*HT))
        CVTS = CV4*((1.033*(1.0+1.382937*Exp(-4.015292*(DBH/10.0))))*
     &    (.005454*DBH**2+0.087266)-0.174533)/(.005454*DBH**2-0.087266)
      ENDIF
      VOL(1) = CVT
      VOL(4) = CV4
      VOL(14) = CVTS - CVT
      IF(DBH.GE.BFMIND)THEN
        CV6 = CV4*(.993-.993*.62**(DBH-6.0))
        IV6 = CV6*(-2.904154+3.466328*LOG10(DBH*TARIF)
     &         -.02765985*DBH-.00008205*TARIF**2+11.29598/DBH**2)
        SV616=(10**(.174439+.117594*LOG10(DBH)*LOG10((TARIF/.912733))
     &          -8.210585/DBH**2+.236693*LOG10((TARIF/.912733))
     &          -.00001345*(TARIF/.912733)**2-.00001937*DBH**2))*CV6
        VOL(4) = CV6
        VOL(7) = CV4 - CV6
        VOL(2) = SV616
        VOL(10) = IV6
        IF(VOLEQ(2:3).EQ.'32')THEN
          SV632 = SV616*(1.001491-6.924097/TARIF+.00001351*DBH**2)
          VOL(2) = SV632
        ENDIF
!       SAWLOG MERCH TOP 8.0        
        IF(MTOPP.GE.8.0)THEN
          IF(CV8.LE.0.0) CV8 = CV4*(.983-.983*.65**(DBH-8.6))
          SV816 = SV616*(.99-.58*.484**(DBH-9.5))
          IV8 = IV6*(0.99-0.55*0.485 ** (DBH-9.5))
          VOL(4) = CV8
          VOL(7) = CV4 - CV8
          VOL(2) = SV816
          VOL(10) = IV8
        ENDIF
      ENDIF
      VOL(15) = VOL(1)-VOL(4)-VOL(7)
      RETURN
      END SUBROUTINE DNR24_VOL  