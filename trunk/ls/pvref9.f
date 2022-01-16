      SUBROUTINE PVREF9 (KARD2,ARRAY2,LPVCOD,LPVREF)
C----------
C LS $Id$
C----------
C
C     MAPS PV/REFERENCE CODES INTO A FVS HABITAT/ECOCLASS CODE
C     CALLED FROM **HABTYP** WHEN REFTMP IS GREATER THAN ZERO
C
C     INPUT VARIABLES
C     KARD2          - FIELD 2 OF STDINFO KEYWORD
C     ARRAY2         - FIELD 2 OF STDINFO (REAL CONTERPART TO KARD2)
C     CPVREF         - FIELD 7 OF STDINFO KEYWORD
C                    - CARRIED IN PLOT.F77
C
C     RETURN VARIABLES
C     KARD2          - MAPPED FVS HABITAT/ECOCLASS CODE
C
C     INTERNAL VARIABLES
C     PVCODE,PVREF   - ARRAYS OF PV CODE/REFERENCE CODE COMBINATIONS
C                      FROM FSVEG DATA BASE
C     HABPVR         - FVS HABITAT/ECOCLASS CODE CORRESPONDING TO
C                      PV CODE/REFERENCE CODE COMBINATION
C
      IMPLICIT NONE
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
C
      INCLUDE 'PLOT.F77'
C
C  DECLARATIONS
C
      REAL         ARRAY2
      INTEGER      I,NCODES
      PARAMETER    (NCODES=127)
      CHARACTER*10 PVREF(NCODES),PVCODE(NCODES),KARD2,KARD2T
      CHARACTER*10 HABPVR(NCODES)
      LOGICAL      LPVCOD,LPVREF
C
C  DATA STATEMENTS
C
      DATA (HABPVR(I),I=   1,  60)/
     &'APN80     ','APN81     ','APN90     ','APN91     ',
     &'CTN11     ','CTN12     ','CTN24     ','CTN32     ',
     &'CTN42     ','CTU22     ','FDC12     ','FDC23     ',
     &'FDC24     ','FDC25     ','FDC34     ','FDN12     ',
     &'FDN22     ','FDN32     ','FDN33     ','FDN43     ',
     &'FFN57     ','FFN67     ','FPN62     ','FPN63     ',
     &'FPN71     ','FPN72     ','FPN73     ','FPN81     ',
     &'FPN82     ','FPS63     ','FPW63     ','LKI32     ',
     &'LKI43     ','LKI54     ','LKU32     ','LKU43     ',
     &'MHC26     ','MHC36     ','MHC37     ','MHC47     ',
     &'MHN35     ','MHN44     ','MHN45     ','MHN46     ',
     &'MHN47     ','MRN83     ','MRN93     ','MRU94     ',
     &'OPN81     ','OPN91     ','OPN92     ','OPN93     ',
     &'RON12     ','RON23     ','RVX32     ','RVX43     ',
     &'RVX54     ','WFN53     ','WFN55     ','WFN64     '/
      DATA (HABPVR(I),I=  61, 120)/
     &'WFS57     ','WFW54     ','WMN82     ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'          ','          ','          ','          ',
     &'FDC23     ','FDC12     ','          ','          '/
      DATA (HABPVR(I),I= 121, NCODES)/
     &'          ','          ','          ','          ',
     &'          ','          ','          '/
C
      DATA (PVCODE(I),I=   1,  60)/
     &'APN80     ','APN81     ','APN90     ','APN91     ',
     &'CTN11     ','CTN12     ','CTN24     ','CTN32     ',
     &'CTN42     ','CTU22     ','FDC12     ','FDC23     ',
     &'FDC24     ','FDC25     ','FDC34     ','FDN12     ',
     &'FDN22     ','FDN32     ','FDN33     ','FDN43     ',
     &'FFN57     ','FFN67     ','FPN62     ','FPN63     ',
     &'FPN71     ','FPN72     ','FPN73     ','FPN81     ',
     &'FPN82     ','FPS63     ','FPW63     ','LKI32     ',
     &'LKI43     ','LKI54     ','LKU32     ','LKU43     ',
     &'MHC26     ','MHC36     ','MHC37     ','MHC47     ',
     &'MHN35     ','MHN44     ','MHN45     ','MHN46     ',
     &'MHN47     ','MRN83     ','MRN93     ','MRU94     ',
     &'OPN81     ','OPN91     ','OPN92     ','OPN93     ',
     &'RON12     ','RON23     ','RVX32     ','RVX43     ',
     &'RVX54     ','WFN53     ','WFN55     ','WFN64     '/
      DATA (PVCODE(I),I=  61, 120)/
     &'WFS57     ','WFW54     ','WMN82     ','10        ',
     &'11        ','110       ','120       ','21        ',
     &'211       ','212       ','22        ','221       ',
     &'222       ','23        ','230       ','24        ',
     &'31        ','311       ','312       ','32        ',
     &'321       ','322       ','33        ','330       ',
     &'34        ','411       ','412       ','62        ',
     &'63        ','64        ','72        ','73        ',
     &'74        ','80        ','81        ','82        ',
     &'AOC       ','AQVac     ','AQVib     ','ATD       ',
     &'AVO       ','DSH       ','FE        ','FI        ',
     &'FMC       ','HPM       ','HPM42     ','HPM43     ',
     &'HRM       ','HRM47     ','MSH       ','MSH37     ',
     &'OWP       ','OWP1      ','PCS       ','PO        ',
     &'PVC       ','PVD       ','QAE       ','TAM       '/
      DATA (PVCODE(I),I= 121, NCODES)/
     &'TM        ','TMC       ','TMV       ','TTL       ',
     &'TTM       ','TTP       ','TTS       '/
C
      DATA (PVREF(I),I=   1,  60)/
     &'904       ','904       ','904       ','904       ',
     &'904       ','904       ','904       ','904       ',
     &'904       ','904       ','904       ','904       ',
     &'904       ','904       ','904       ','904       ',
     &'904       ','904       ','904       ','904       ',
     &'904       ','904       ','904       ','904       ',
     &'904       ','904       ','904       ','904       ',
     &'904       ','904       ','904       ','904       ',
     &'904       ','904       ','904       ','904       ',
     &'904       ','904       ','904       ','904       ',
     &'904       ','904       ','904       ','904       ',
     &'904       ','904       ','904       ','904       ',
     &'904       ','904       ','904       ','904       ',
     &'904       ','904       ','904       ','904       ',
     &'904       ','904       ','904       ','904       '/
      DATA (PVREF(I),I=  61, 120)/
     &'904       ','904       ','904       ','903       ',
     &'903       ','903       ','903       ','903       ',
     &'903       ','903       ','903       ','903       ',
     &'903       ','903       ','903       ','903       ',
     &'903       ','903       ','903       ','903       ',
     &'903       ','903       ','903       ','903       ',
     &'903       ','903       ','903       ','902       ',
     &'902       ','902       ','902       ','902       ',
     &'902       ','902       ','902       ','902       ',
     &'901       ','901       ','901       ','901       ',
     &'901       ','902       ','901       ','901       ',
     &'901       ','902       ','902       ','902       ',
     &'902       ','902       ','902       ','902       ',
     &'902       ','902       ','901       ','901       ',
     &'901       ','901       ','901       ','901       '/
      DATA (PVREF(I),I= 121, NCODES)/
     &'901       ','901       ','901       ','901       ',
     &'901       ','901       ','901       '/
C----------
C  MAP PV/REFERENCE CODES INTO A FVS HABITAT/ECOCLASS CODE
C----------
      KODTYP=0
      KARD2T=KARD2
      KARD2='          '
      ARRAY2=0.
      DO I=1,NCODES
      IF((ADJUSTL(PVCODE(I)).EQ.ADJUSTL(KARD2T)).AND.(ADJUSTL(PVREF(I))
     &  .EQ.ADJUSTL(CPVREF)))THEN
        KARD2=HABPVR(I)
        LPVCOD=.TRUE.
        LPVREF=.TRUE.
        EXIT
      ENDIF
      IF(ADJUSTL(PVCODE(I)).EQ.ADJUSTL(KARD2T))LPVCOD=.TRUE.
      IF(ADJUSTL(PVREF(I)).EQ.ADJUSTL(CPVREF))LPVREF=.TRUE.
      ENDDO
C
      RETURN
      END