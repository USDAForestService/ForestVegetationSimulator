      SUBROUTINE HABTYP (KARD2,ARRAY2)
      IMPLICIT NONE
C----------
C UT $Id$
C----------
C
C     TRANSLATES HABITAT TYPE  CODE INTO A SUBSCRIPT, ITYPE, AND IF
C     KODTYP IS ZERO, THE ROUTINE RETURNS 0.
C
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'VARCOM.F77'
C
C
COMMONS
C----------
      INTEGER NR4,I,I2,I1,IHB
C
C  IF NR4 CHANGES FROM 363 TO SOMETHING ELSE, THEN THE MAPPING FUNCTION
C  AND THE CORRESPONDING MAPPING ARRAY DIMENSION USED IN TETONS  
C  SUBROUTINE **DGF** MUST ALSO BE CHANGED.
C
      PARAMETER (NR4=363)
      CHARACTER*8 R4HABT(NR4)
C
      REAL ARRAY2
      CHARACTER*10 KARD2
      LOGICAL DEBUG
      LOGICAL LPVCOD,LPVREF,LPVXXX
C----------
C  LOAD REGION 4 HABITAT TYPE ARRAY
C----------
      DATA (R4HABT(I),I=1,50)/
C 1-25
     &'050     ', '060     ', '070     ', '080     ', '090     ',
     &'100     ', '120     ', '130     ', '140     ', '150     ',
     &'160     ', '161     ', '162     ', '170     ', '190     ',
     &'195     ', '41045   ', '41115   ', '41141   ', '41143   ',
     &'41144   ', '41376   ', '41406   ', '41416   ', '41708   ',
C 26-50
     &'41709   ', '41715   ', '41716   ', '41717   ', '41725   ',
     &'41726   ', '41813   ', '41814   ', '41815   ', '41862   ',
     &'41864   ', '41915   ', '41956   ', '41957   ', '41970   ',
     &'41107   ', '41108   ', '41109   ', '41111   ', '41112   ',
     &'41113   ', '41114   ', '41365   ', '41366   ', '41367   '/
      DATA (R4HABT(I),I=51,100)/
C 51-75
     &'41382   ', '41408   ', '41409   ', '41746   ', '41747   ',
     &'41816   ', '41866   ', '41867   ', '41868   ', '41869   ',
     &'41871   ', '41872   ', '41873   ', '42001   ', '42002   ',
     &'42003   ', '42004   ', '42005   ', '42006   ', '42007   ',
     &'42008   ', '42009   ', '42010   ', '42011   ', '42012   ',
C 76-10
     &'42040   ', '42041   ', '42042   ', '42043   ', '42044   ',
     &'42045   ', '42046   ', '42047   ', '42048   ', '42049   ',
     &'42050   ', '42051   ', '42052   ', '42053   ', '42054   ',
     &'42055   ', '42056   ', '42080   ', '42081   ', '42082   ',
     &'42083   ', '42084   ', '42085   ', '42086   ', '42087   '/
      DATA (R4HABT(I),I=101,150)/
C 101-15
     &'42088   ', '42089   ', '42101   ', '42102   ', '42103   ',
     &'42104   ', '42105   ', '42106   ', '42107   ', '42108   ',
     &'42109   ', '42110   ', '42111   ', '42201   ', '42202   ',
     &'42203   ', '42204   ', '42301   ', '42302   ', '42303   ',
     &'42304   ', '42401   ', '42402   ', '42403   ', '42500   ',
C 126-10
     &'42600   ', '42700   ', '43002   ', '43004   ', '43006   ',
     &'43012   ', '43105   ', '43202   ', '43208   ', '43225   ',
     &'43227   ', '43241   ', '43271   ', '43283   ', '43303   ',
     &'43305   ', '43307   ', '43321   ', '43354   ', '43400   ',
     &'43552   ', '43602   ', '43813   ', '43881   ', '43941   '/
      DATA (R4HABT(I),I=151,200)/
C 151-15
     &'43003   ', '43005   ', '43007   ', '43008   ', '43009   ',
     &'43011   ', '43013   ', '43041   ', '43042   ', '43044   ',
     &'43045   ', '43081   ', '43082   ', '43102   ', '43201   ',
     &'43209   ', '43210   ', '43221   ', '43226   ', '43244   ',
     &'43245   ', '43281   ', '43286   ', '43322   ', '43323   ',
C 176-200
     &'43324   ', '43326   ', '43601   ', '43603   ', '43802   ',
     &'43804   ', '43806   ', '43810   ', '43851   ', '43861   ',
     &'43901   ', '43925   ', '46001   ', '46002   ', '46003   ',
     &'46011   ', '46021   ', '46031   ', '46041   ', '46042   ',
     &'46043   ', '46051   ', '46061   ', '46101   ', '46111   '/
      DATA (R4HABT(I),I=201,250)/
C 201-225
     &'46112   ', '46113   ', '46114   ', '46131   ', '46132   ',
     &'46133   ', '46151   ', '46152   ', '46153   ', '46154   ',
     &'46155   ', '46156   ', '46157   ', '46158   ', '46159   ',
     &'46171   ', '46172   ', '46173   ', '46174   ', '46191   ',
     &'46192   ', '46201   ', '46301   ', '47001   ', '47011   ',
C 226-250
     &'47012   ', '47021   ', '47025   ', '47002   ', '47003   ',
     &'47004   ', '43001   ', '43010   ', '43043   ', '43046   ',
     &'43101   ', '43103   ', '43104   ', '43151   ', '43152   ',
     &'43155   ', '43203   ', '43204   ', '43205   ', '43206   ',
     &'43207   ', '43222   ', '43223   ', '43224   ', '43228   '/
      DATA (R4HABT(I),I=251,300)/                               
C 251-275                                                       
     &'43242   ', '43243   ', '43301   ', '43302   ', '43306   ',
     &'43308   ', '43353   ', '43551   ', '43553   ', '43604   ',
     &'43801   ', '43805   ', '43807   ', '43808   ', '43809   ',
     &'43812   ', '43821   ', '43822   ', '43831   ', '43871   ',
     &'43882   ', '43921   ', '43931   ', '41050   ', '41060   ',
C 276-300
     &'41070   ', '41080   ', '41140   ', '41160   ', '41195   ',
     &'41220   ', '41221   ', '41260   ', '41265   ', '41266   ',
     &'41280   ', '41310   ', '41313   ', '41320   ', '41323   ',
     &'41340   ', '41341   ', '41343   ', '41360   ', '41370   ',
     &'41371   ', '41372   ', '41375   ', '41380   ', '41385   '/
      DATA (R4HABT(I),I=301,350)/
C 301-325
     &'41390   ', '41395   ', '41396   ', '41397   ', '41398   ',
     &'41399   ', '41407   ', '41410   ', '41415   ', '41440   ',
     &'41485   ', '41490   ', '41493   ', '41497   ', '41601   ',
     &'41603   ', '41635   ', '41636   ', '41640   ', '41645   ',
     &'41650   ', '41651   ', '41654   ', '41655   ', '41660   ',
C 326-350
     &'41661   ', '41663   ', '41670   ', '41671   ', '41690   ',
     &'41691   ', '41692   ', '41702   ', '41703   ', '41705   ',
     &'41706   ', '41707   ', '41714   ', '41720   ', '41721   ',
     &'41723   ', '41730   ', '41731   ', '41732   ', '41734   ',
     &'41745   ', '41750   ', '41760   ', '41780   ', '41790   '/
      DATA (R4HABT(I),I=351,NR4)/
C 351-363
     &'41791   ', '41795   ', '41810   ', '41811   ', '41830   ',
     &'41831   ', '41861   ', '41863   ', '41865   ', '41920   ',
     &'41940   ', '41955   ', '41960   '/
C
      LPVREF=.FALSE.
      LPVCOD=.FALSE.
      LPVXXX=.FALSE.
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'HABTYP',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,*)
     &'ENTERING HABTYP CYCLE,KODTYP,KODFOR,KARD2,ARRAY2,CPVREF= ',
     &ICYC,KODTYP,KODFOR,KARD2,ARRAY2,CPVREF
C----------
C  DETERMINE IF A CODE WAS ENTERED
C----------
      I1=0
      I2=0
      DO 20 I=1,10
         IF (KARD2(I:I).NE.' ') THEN
            IF (I1.EQ.0) I1=I
         ELSE
            IF (I1.GT.0.AND. I2.EQ.0) I2=I-1
         ENDIF
 20   CONTINUE
      IF (I2.EQ.0 .AND. I1.GT.0) I2=10
C
      IF (I2-I1.GT.0) THEN
C----------
C  IF REFERENCE CODE IS NON-ZERO THEN MAP PV CODE/REF. CODE TO
C  FVS HABITAT TYPE/ECOCLASS CODE. THEN PROCESS FVS CODE
C----------
        IF(CPVREF.NE.'          ') THEN
          CALL PVREF4(KARD2,ARRAY2,LPVCOD,LPVREF)
          ICL5=0
          IF((LPVCOD.AND.LPVREF).AND.
     &        (KARD2.EQ.'          ').AND.(KARD2.NE.'UNKNOWN   '))THEN
            CALL ERRGRO(.TRUE.,34)
            LPVXXX=.TRUE.
            PCOMX='2NDPASS '
          ELSEIF((.NOT.LPVCOD).AND.(.NOT.LPVREF).AND.
     &           (KARD2.NE.'UNKNOWN   '))THEN
            CALL ERRGRO(.TRUE.,33)
            CALL ERRGRO(.TRUE.,32)
            LPVXXX=.TRUE.
             PCOMX='2NDPASS '
         ELSEIF((.NOT.LPVREF).AND.LPVCOD.AND.
     &           (KARD2.NE.'UNKNOWN   '))THEN
            CALL ERRGRO(.TRUE.,32)
            LPVXXX=.TRUE.
            PCOMX='2NDPASS '
          ELSEIF((.NOT.LPVCOD).AND.LPVREF.AND.
     &           (KARD2.NE.'UNKNOWN   '))THEN
            CALL ERRGRO(.TRUE.,33)
            LPVXXX=.TRUE.
            PCOMX='2NDPASS '
          ENDIF
        ENDIF
C----------
C  DIGEST HABITAT TYPE CODE
C----------
        IF(DEBUG)WRITE(JOSTND,*)'DIGESTING HABITAT CODE: KODFOR,',
     &  'KODTYP= ',KODFOR,KODTYP
        CALL CRDECD(KODTYP,R4HABT(1),NR4,ARRAY2,KARD2)
        IF(DEBUG)WRITE(JOSTND,*)'AFTER R4 DECODE,KODTYP= ',KODTYP
        IF(KODTYP .LT. 0) KODTYP=0
        ITYPE=KODTYP
      ELSE
        KODTYP=0
        ITYPE=0
      ENDIF
C----------
C  IF NO MATCH WAS FOUND, TREAT IT AS A SEQUENCE NUMBER.
C----------
      IF(KODTYP .EQ. 0)THEN 
        IF(DEBUG)WRITE(JOSTND,*)'EXAMINING FOR INDEX, ARRAY2= ',ARRAY2
        IHB = IFIX(ARRAY2)
        IF(IHB.LE.NR4)THEN
          KODTYP=IHB
          ITYPE=IHB
        ENDIF
      ENDIF
C----------
C  FINISH FINAL SETTINGS OR SET DEFAULT CONDITIONS
C----------
      IF(KODTYP .NE. 0)THEN
        ICL5=KODTYP 
        KARD2=R4HABT(ITYPE)
        IF(LSTART)WRITE(JOSTND,311) KARD2
  311   FORMAT(/,T12,'HABITAT TYPE CODE USED IN THIS PROJECTION',
     &  ' IS ',A8)
      ELSE
        IF((LSTART).AND.(.NOT.LPVXXX).AND.(PCOMX.NE.'2NDPASS '))
     &  CALL ERRGRO (.TRUE.,14)
          KARD2='UNKNOWN   '
          PCOM='UNKNOWN '
          PCOMX='2NDPASS '
          ICL5=0
      ENDIF
C
      IF(DEBUG)WRITE(JOSTND,*)'LEAVING HABTYP KODTYP,ITYPE,ICL5,KARD2='
     &,KODTYP,ITYPE,ICL5,KARD2
C
      RETURN
      END
