      SUBROUTINE ORGSPC(INSPEC,OUTSPC,OUTSPG)
      IMPLICIT NONE
C----------
C  **ORGSPC--OP   DATE OF LAST REVISION:  11/02/17
C----------
C THIS SUBROUTINE CONVERTS AN FVS SPECIES SEQUENCE NUMBER TO A VALID
C ORGANON SPECIES FIA CODE.
C
C CALLED FROM SUBROUTINES **CRATET** AND **DGDRIV**
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
COMMONS
C----------
      INTEGER OSPMAP(MAXSP),INSPEC,OUTSPC,OSGMAP(MAXSP),OUTSPG
C
C   SPECIES ORDER IN THIS VARIANT
C
C   1=SF,  2=WF,  3=GF,  4=AF,  5=RF,  6=SS,  7=NF,  8=YC,
C   9=IC, 10=ES, 11=LP, 12=JP, 13=SP, 14=WP, 15=PP, 16=DF,
C  17=RW, 18=RC, 19=WH, 20=MH, 21=BM, 22=RA, 23=MA, 24=TO,
C  25=GC, 26=AS, 27=CW, 28=WO, 29=J , 30=LL, 31=WB, 32=KP,
C  33=PY, 34=DG, 35=HT, 36=CH, 37=WI, 38=  , 39=OT
C----------
C MAPPING OF FVS SPECIES TO VALID ORGANON SPECIES FOR THE 
C ORGANON NORTHWEST OREGON AND STAND MANAGEMENT COOP MODEL TYPES:
C
C VALID
C ORGANON   FVS
C 017=GF*   GF, WF, SF, AF, NF, LL
C 202=DF*   DF, RF, SS, ES, LP, JP, SP, WP, PP, WB, KP
C 231=PY    PY, WJ
C 242=RC    RC, YC, IC 
C 263=WH    WH, RW, MH
C 312=BM    BM, TO, AS, CW
C 351=RA    RA 
C 361=MA    MA, GC
C 492=DG    DG, HT, CH, OT 
C 815=WO    WO
C 920=WI    WI
C *SPECIES INCLUDED IN ORGANON "BIG 6"
C----------
      DATA OSPMAP/
C       SF   WF   GF   AF   RF   SS   NF   YC   IC   ES      
     & 017, 017, 017, 017, 202, 202, 017, 242, 242, 202, 
C       LP   JP   SP   WP   PP   DF   RW   RC   WH   MH      
     & 202, 202, 202, 202, 202, 202, 263, 242, 263, 263, 
C       BM   RA   MA   TO   GC   AS   CW   WO   WJ   LL      
     & 312, 351, 361, 312, 361, 312, 312, 815, 231, 017, 
C       WB   KP   PY   DG   HT   CH   WI        OT    
     & 202, 202, 231, 492, 492, 492, 920, 492, 492/
C----------
C  MAPPING TO THE ORGANON SPECIES GROUP NUMBER (ALSO SEE 
C  **SPGROUP_RUN** IN **EXECUTE2**)
C----------
      DATA OSGMAP/
C       SF   WF   GF   AF   RF   SS   NF   YC   IC   ES      
     &   2,   2,   2,   2,   1,   1,   2,   4,   4,   1, 
C       LP   JP   SP   WP   PP   DF   RW   RC   WH   MH      
     &   1,   1,   1,   1,   1,   1,   3,   4,   3,   3, 
C       BM   RA   MA   TO   GC   AS   CW   WO   WJ   LL      
     &   7,   9,   6,   7,   6,   7,   7,   8,   5,   2, 
C       WB   KP   PY   DG   HT   CH   WI        OT    
     &   1,   1,   5,  10,  10,  10,  11,  10,  10/
C
      OUTSPC = OSPMAP(INSPEC)
      OUTSPG = OSGMAP(INSPEC)
C
      RETURN
      END
