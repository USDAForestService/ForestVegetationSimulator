!== last modified  10-31-2011
      SUBROUTINE R10VOLO(EQNUM,DBHOB,HTTOT,HT1PRD,HTYPE,MTOPP,TLOGS, 
     >       VOL,LOGSPAN,LOGDIA,logvol,BFPFLG,CUPFLG,ERRFLAG)

*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
*  SUBROUTINE NAME: R10VOL                                            *
*          AUTHOR: D.DeMars/M.Schmidt                                 *
*    CONVERTED BY: K.CORMIER/B.JONES                                  *
***********************************************************************

      CHARACTER*10 EQNUM
      CHARACTER*1 LTYPE,HTYPE

      INTEGER DI1,DI2,bfpflg, cupflg,ERRFLAG,I
      INTEGER NCNT,NCHK,JBF,KBF,KAJ,IKD,KBF2,KDBFF,KDBFS,KDBFC
      INTEGER ITEMP,CHK

      REAL TAB16(66),TAB32(66),BF1(21),BF2(21),DBF(21)
      REAL VOL(15),LOGVOL(7,20),DIB(21),VS(21),V(21),HTSEG(21)
      REAL CV(21),SCF(21),XINT(21),CUBVOL
      REAL MTOPP,BF16,BF32,TDBF,AJ
      REAL LOGDIA(21,3),CL,DI,W2,W1,BF11,BF12,LENGTH,LOGV
      REAL LOGSPAN(20),BF21,BF22,XINTT,SCFT,DIBL,DIBS
      REAL DBHOB,HTTOT,HT1PRD,THT,HMERCH,BFCHUG,BFTONG
      REAL CUBMS,TLOGS,NLOG

      LOGICAL CHECK

C     (BD.FT.)/10 SCRIBNER 16 FT. LOGS; 5 IN. TO 70 IN. 
   
      DATA TAB16/2.,2.,3.,3.,4.,6.,7.,8.,10.,11.,14.,16.,18.,21.,24.,   
     >   28.,30.,33.,38.,40.,46.,50.,55.,58.,61.,66.,71.,74.,78.,80.,
     >   88.,92.,103.,107.,112.,120.,127.,134.,140.,148.,152.,159., 
     >  166.,173.,180.,187.,195.,202.,210.,218.,227.,235.,244.,252.,
     >  261.,270.,280.,289.,299.,309.,319.,329.,339.,350.,361.,372./

C      (BD.FT.)/10 SCRIBNER 32 FT. LOGS; 5 IN. TO 70 IN.
   
      DATA TAB32/3.,5.,6.,7.,9.,12.,14.,16.,19.,23.,28.,32.,37.,43.,
     >   48.,56.,61.,67.,75.,81.,92.,100.,110.,116.,122.,131.,142., 
     >  147.,157.,160.,175.,185.,206.,214.,224.,241.,254.,269.,279., 
     >  296.,304.,317.,331.,346.,359.,374.,389.,405.,421.,437.,453.,
     >  470.,487.,505.,523.,541.,560.,578.,597.,618.,637.,658.,678.,
     >  699.,722.,744./  

      ERRFLAG = 0

C *************  RESET ZEROES  **************

      DO 10 I=1,15
          VOL(I) = 0.0
 10   CONTINUE

      DO 25, I=1,21  
        BF1(I) = 0.0
        BF2(I) = 0.0
        DBF(I) = 0.0
        CV(I) = 0.0
        XINT(I) = 0.0
        SCF(I) = 0.0
        VS(I) = 0.0
        V(I) = 0.0
 25   CONTINUE
  
      BF16 = 0.0  
      BF32 = 0.0  
      TDBF = 0.0
      BFCHUG = 0.0
      BFTONG = 0.0
      CUBMS = 0.0
      CHECK = .FALSE.
  
C     IF DBHOB OR HT1PRD EQUALS ZERO THEN DON'T CALCULATE THE VOLUME
      IF (DBHOB.LT.1.0)THEN
        ERRFLAG = 3
        GOTO 1000
      ENDIF
      IF(HTTOT.LE.0.0 .AND. HT1PRD.LE.0.0) THEN
        ERRFLAG = 4
        GOTO 1000
      ENDIF

C***********HEIGHT TYPE IS CHECKED

      IF(HTYPE.EQ.'L' .OR. HTYPE.EQ.'l') THEN
         LTYPE='L'
         THT = HT1PRD
      ELSE
         LTYPE='F'
         THT = HTTOT

      ENDIF

c do this only once for both cubic and boards
c place values into logdia and loglen first

      IF (BFPFLG.EQ.1 .OR. CUPFLG.EQ.1) THEN
        CHK=1

        CALL R10TAPO (DBHOB,THT,LTYPE,MTOPP,CHK,NLOG,HMERCH,CL,HTSEG,
     >                                                         DIB,VS,V)

        NCNT = NLOG+4

c fill logdia array      
        HTSEG(1) = 1.0
        LOGDIA(1,2) = DIB(1)
        LOGDIA(1,1) = ANINT(DIB(1))
        DO 30, I=1,INT(NLOG)   
           LOGDIA(I+1,2) = DIB(I+1)
           LOGDIA(I+1,1) = ANINT(DIB(I+1))

           LOGSPAN(I) = HTSEG(I+1) - HTSEG(I) - .3
 30     CONTINUE    
        IF(LOGDIA(INT(NLOG)+1,1).LT.MTOPP) LOGDIA(INT(NLOG)+1,1)=MTOPP

        TLOGS = int(nlog)
        
C BOARD FOOT LOGIC
C******************************************************************
        IF(BFPFLG.EQ.1) THEN
           DO 100, I=1,INT(TLOGS)
   
              DI = LOGDIA(I+1,1)
              DI1 = DI
              DI2 = DI1 + 1
              W2 = 10. * (DI - DI1)
              W1 = 10. - W2
C   
C      LOOK UP BD. FT. FOR 16 FT. LOGS  
C   
              BF11 = TAB16(DI1-4) 
              BF12 = TAB16(DI2-4) 
              BF1(I) = W1 * BF11 + W2 * BF12  
              IF(LTYPE.EQ.'L' .OR. LTYPE.EQ.'l') THEN
                 BF16 = BF16 + BF1(I)   
                 CHK = I/2   
                 NCHK = CHK  
                 NCHK = 2 * NCHK  
              ELSE
                 IF(I.EQ.INT(NLOG)) BF1(I) = 20. * CL/16.3
                 JBF = BF1(I) +0.5
                 BF1(I) = JBF
                 BF16 = BF16 + BF1(I)   
                 CHK = I/2   
                 NCHK = CHK  
                 NCHK = 2 * NCHK  
              ENDIF

              IF(NCHK.EQ.I) THEN  
   
C  LOOK UP BD. FT. 32 FT. LOGS  
   
                 BF21 = TAB32(DI1-4)
                 BF22 = TAB32(DI2-4)
                 BF2(I) = W1 * BF21 + W2 * BF22  

                 IF(LTYPE.EQ.'L' .OR. LTYPE.EQ.'l') THEN
                    BF32 = BF32 + BF2(I)  
                 ELSE
                    IF(I.EQ.INT(NLOG)) BF2(I) = 50. * (16.3 + CL)/32.6
                    KBF = BF2(I) + 0.5
                    BF2(I) = KBF
                    BF32 = BF32 + BF2(I)  
                 ENDIF

C      DISTRIBUTE 32 FT. BD.FT. IN PROPORTION TO 16 FT. BD.FT.  
   
                 DBF(I-1) = BF1(I-1)/(BF1(I-1) + BF1(I)) * BF2(I)
                 DBF(I) = BF2(I) - DBF(I-1)
                 TDBF = TDBF + DBF(I-1) + DBF(I) 

              ELSEIF(I.EQ.TLOGS) THEN

                 BF2(I) = BF1(I) 
                 DBF(I) = BF2(I) 
                 TDBF = TDBF + BF2(I)  
                 BF32 = BF32 + BF2(I)
              ENDIF

C    INTERNATIONAL LOGIC
              CALL INTL14(DI,LOGSPAN(I),LOGV)
              VOL(10) = VOL(10) + LOGV


  100      CONTINUE  

           AJ = NLOG/2
           KAJ = AJ
           KAJ = KAJ * 2 

           DO 140, IKD = 2,KAJ,2
              KBF2 = BF2(IKD) + 0.5  
              KDBFF = DBF(IKD-1) + 0.50000001
              KDBFS = DBF(IKD) + 0.50000001  
              KDBFC = KDBFF + KDBFS 

              IF(KDBFC.NE.KBF2) THEN
                 DBF(IKD-1) = KDBFF  
                 DBF(IKD) = KDBFS - 1  
              ENDIF

  140      CONTINUE
  
           DO  150, I=1,INT(TLOGS)
              ITEMP = BF1(I) + 0.5   
              BF1(I) = ITEMP
              ITEMP = DBF(I) + 0.5   
              DBF(I) = ITEMP

              BFCHUG = BFCHUG + ANINT(BF1(I)*10.0)/10.0
              BFTONG = BFTONG + ANINT(DBF(I)*10.0)/10.0

              IF (EQNUM(1:3).EQ.'A01' .OR. EQNUM(1:3).EQ.'a01') THEN
                 LOGVOL(1,I) = ANINT(BF1(I)*10.0)/10.0
              ELSEIF (EQNUM(1:3).EQ.'A02' .OR. EQNUM(1:3).EQ.'a02') THEN
                 LOGVOL(1,I) = ANINT(DBF(I)*10.0)/10.0
              ENDIF
C          LOGS=NLOG
              CHECK = .TRUE.

 150       CONTINUE  

           IF (EQNUM(1:3).EQ.'A01' .OR. EQNUM(1:3) .EQ. 'a01') THEN
              VOL(2) = BFCHUG
           ELSEIF (EQNUM(1:3).EQ.'A02' .OR. EQNUM(1:3).EQ.'a02') THEN
              VOL(2) = BFTONG
           ENDIF
       
C  END BOARD FOOT ROUTINES

        ENDIF

C*************************************************
C***               CUBIC VOLUMES               ***
C*************************************************

        IF (CUPFLG.EQ.1) THEN

C       VTIP=VS(NLOG3)
     
           XINTT=0.0
           SCFT=0.0  
C--   GET CUBIC VOLUME FOR THE PIECES SMALIAN FORMULA      

C--   USE DIB AT DBHOB FOR LARGE END BUTT LOG
           DIBL = LOGDIA(1,1)
           DO 350 I=1,INT(TLOGS)
              DIBS = LOGDIA(I+1,1)
              LENGTH=LOGSPAN(I)
              LOGV = .00272708*(DIBL*DIBL+DIBS*DIBS)*LENGTH
              LOGVOL(4,I) = ANINT(LOGV*10.0)/10.0
              VOL(4)=VOL(4)+LOGVOL(4,I)
              DIBL = DIBS
  350      CONTINUE
        ENDIF

        IF(VOL(4).LE.0.0) THEN
          VOL(4) = 0.0
        ENDIF

      ENDIF
 
C*************************************************************
C*************************************************************
C--    DETERMINE TOTAL CUBIC FIBER CONTENT (ALL TREES)       *
C*************************************************************

C--   ENTER LOGIC FOR TOTAL CUBIC VOLUME HERE
      IF ((DBHOB.GE.9.0 .AND. HTTOT.GT.40.0) .AND. CUPFLG.EQ.1) THEN
         call R10TCO(EQNUM,DBHOB,HTTOT,CUBVOL)
        VOL(1) = CUBVOL
      ENDIF

C********************************************
C         TOPWOOD CUBIC VOLUME          *****
C********************************************

c        IF(CUB64.LE.0.0) THEN
c          VOL(7) = 0.0
c        ELSE
c          VOL(7) = CUB64
c        ENDIF

c        VOL(8) = VOL(7)

c      ENDIF

C**************************************************************
C--  CHECK FOR LOG DETAIL INFORMATION AND ASSIGN LOG GRADES,  *
C--     LOG DEFECTS, AND NET VOLUMES FOR THE TREE             *
C--        MOVED TO NETVOL.FOR
C**************************************************************

 1000 CONTINUE

      RETURN
      END   

C**************************************************************
C**************************************************************
      SUBROUTINE R10TCO(VOLEQ,DBHOB,HTTOT,TCVOL)
C**************************************************************

C--   DETERMINE TOTAL CUBIC FIBER CONTENT.  THE VOLUME WILL BE
C--   DETERMINED BY USING THE VOLUME OF A CYLINDER WITH A DIAMETER
C--   OF DIB AT 1 FOOT FOR THE STUMP VOLUME AND USING THE SMALIAN
C--   FORMULA TO COMPUTE VOLUMES FOR 4 FOOT SECTIONS FROM THE STUMP
C--   TO THE TIP.

C--   VARIABLES OF INTEREST ARE:
C--   totht - REAL - **TOTAL TREE HT INCLUDING THE STUMP.**
C--   TCVOL - REAL - TOTAL VOLUME FOR THE TREE IN CUBIC FOOT,
C--                  DOES NOT INCLUDE THE LIMBS OR ROOTS.

      CHARACTER*10 VOLEQ
      INTEGER HTLOOP,I
      REAL D2, D2OLD, HT2, TCVOL, VOLTMP
      REAL DBHOB,HTTOT,R
                                      
      HTLOOP = INT((HTTOT - 1.0)/4.0)
      HT2=1.0
C##########
      CALL R10TAP(VOLEQ,DBHOB,HTTOT,HT2,D2)
C--   STUMP VOLUME IS VOLUME FOR A 1 FT HIGH CYLINDER
      R=D2/2.0
      TCVOL = (3.1416*R*R)/144.0
      DO 10 I = 1,HTLOOP
        D2OLD = D2
        HT2=HT2+4.0
C##########
         CALL R10TAP(VOLEQ,DBHOB,HTTOT,HT2,D2)
C--     USE SMALIAN FOR NON STUMP SEGMENTS
        VOLTMP = .00272708*(D2OLD*D2OLD+D2*D2)*4.0
        TCVOL = TCVOL + VOLTMP
   10 CONTINUE
C--   USE SMALIAN FOR TIP, WITH A TIP DIAMETER OF 0.0
      IF ( (HTTOT-HT2) .GT. 0.0 ) THEN
         VOLTMP = .00272708*(D2*D2)*(HTTOT-HT2)
         TCVOL = TCVOL + VOLTMP
      ENDIF
      RETURN
      END
***********************************************************************
*                              ARGUMENTS                              *
***********************************************************************
*                                                                     *
*  EQNUM    - (Input)  - Holds equation numbers and top dibs          *
*  DBHOB      - (Input)  - DBHOB                                          *
*  HT1PRD      - (Input)  - Number 16' Logs                              *
*  TYPE     - (Input)  - L if height is in logs (ALWAYS L FOR THIS    *
*                        PROGRAM 
*  PCTDF    - (Input)  - Tree Defect                                  *
*  NLOG     - (Output) - Number 16' Logs Calculated                   *
*  HMERCH   - (Output) - Height, Merch                                *
C**********************************************************************
C--  VOL      - (Output) - Volume array                                 
C--    VOL(1) -  TOTAL VOLUME FOR THE TREE IN CUBIC FOOT, DOES NOT
C--              INCLUDE LIMBS AND ROOTS.
C--    VOL(2) - GROSS AMOUNT OF BOARD FOOT PRODUCT IN MAIN STEM.
C--    VOL(3) - NET AMOUNT OF BOARD FOOT PRODUCT IN MAIN STEM.
C--    VOL(4) - GROSS AMOUNT OF CUBIC FOOT PRODUCT IN MAIN STEM.
C--    VOL(5) - NET AMOUNT OF CUBIC FOOT PRODUCT IN MAIN STEM.
C--    VOL(6) - AMOUNT OF CORD WOOD PRODUCT IN MAIN STEM
C--    VOL(7) - GROSS AMOUNT OF CUBIC PRODUCT IN TOP WOOD.
C--    VOL(8) - NET AMOUNT OF CUBIC PRODUCT IN TOP WOOD.
C--    VOL(9) - AMOUNT OF CORD WOOD PRODUCT IN TOP WOOD.
C--    VOL(10) - GROSS AMOUNT OF INT'L 1/4" BOARD FOOT PRODUCT IN MAIN STEM.
C--    VOL(11) - NET AMOUNT OF INT'L 1/4" BOARD FOOT PRODUCT IN MAIN STEM.
C--    VOL(12) - GROSS AMOUNT OF BOARD FOOT PRODUCT IN TOPWOOD.
C--    VOL(13) - NET AMOUNT OF BOARD FOOT PRODUCT IN TOPWOOD.
C--
C******
C--    LOGVOL(1,X) - GROSS BOARD FT LOG VOLUME (20 LOGS)
C--    LOGVOL(2,X) - GROSS REMOVED BOARD FT LOG VOLUME (20 LOGS)
C--    LOGVOL(3,X) - NET BOARD FT LOG VOLUME (20 LOGS)
C--    LOGVOL(4,X) - GROSS CUBIC FT LOG VOLUME (20 LOGS)
C--    LOGVOL(5,X) - GROSS REMOVED CUBIC FT LOG VOLUME (20 LOGS)
C--    LOGVOL(6,X) - NET CUBIC FT LOG VOLUME (20 LOGS)
C--    LOGVOL(7,X) - GROSS INT'L 1/4 VOLUME (20 LOGS)
C*********************************************************************
************************************************************************
