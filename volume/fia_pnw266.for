! This is from FIA package code NIMS_VOL_PNW266
! CU015005   ABCO_MAC_CV4(DIA IN NUMBER, THT IN NUMBER) 	
! BD015004   ABCO_MAC_BD(DBH IN NUMBER, THT IN NUMBER) 		
! BD015003   ABCO_MAC_INTBD(DBH IN NUMBER, THT IN NUMBER) 
! CU020001   ABMA_MAC_CV4(DIA IN NUMBER, THT IN NUMBER) 	
! BD020002   ABMA_MAC_BD(DBH IN NUMBER, THT IN NUMBER) 		
! BD020001   ABMA_MAC_INTBD(DBH IN NUMBER, THT IN NUMBER) 
! CU081001   CADE27_MAC_CV4(DIA IN NUMBER, THT IN NUMBER) 
! BD081002   CADE27_MAC_BD(DBH IN NUMBER, THT IN NUMBER) 	
! BD081001   CADE27_MAC_INTBD(DBH IN NUMBER, THT IN NUMBER) 
! CU108003   PICO_MAC_CV4(DIA IN NUMBER, THT IN NUMBER) 	
! BD108005   PICO_MAC_BD(DBH IN NUMBER, THT IN NUMBER) 		
! BD108004   PICO_MAC_INTBD(DBH IN NUMBER, THT IN NUMBER) 
! CU117001   PILA_MAC_CV4(DIA IN NUMBER, THT IN NUMBER) 	
! BD117002   PILA_MAC_BD(DBH IN NUMBER, THT IN NUMBER)
! BD119002
! BD117001   PILA_MAC_INTBD(DBH IN NUMBER, THT IN NUMBER) 	
! BD119001
! CU122011   PIPO_MAC_CV4(DIA IN NUMBER, THT IN NUMBER) 		
! BD122011   PIPO_MAC_BD(DBH IN NUMBER, THT IN NUMBER) 		
! BD122010   PIPO_MAC_INTBD(DBH IN NUMBER, THT IN NUMBER) 
! CU202007   PSME_MAC_CV4(DIA IN NUMBER, THT IN NUMBER) 	
! BD202007   PSME_MAC_BD(DBH IN NUMBER, THT IN NUMBER) 		
! BD202006   PSME_MAC_INTBD(DBH IN NUMBER, THT IN NUMBER) 
! CU000124   MACLEAN_CV4(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER) 
! BD000096   MACLEAN_IV6(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER) 
! BD000097   MACLEAN_SV6(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER) 
! CU000125   NIMS_VOL_PNW266.CVTS_TARIF(VOLSPN, DBH, THT)
! CU000126   NIMS_VOL_PNW266.CVT_TARIF(VOLSPN, DBH, THT)
! CU000127   NIMS_VOL_PNW266.CV6_TARIF(VOLSPN, DBH, THT)
! CU000128   NIMS_VOL_PNW266.CV8_TARIF(VOLSPN, DBH, THT)
! BD000098   NIMS_VOL_PNW266.SV616_TARIF(VOLSPN, DBH, THT)
! BD000099   NIMS_VOL_PNW266.SV632_TARIF(VOLSPN, DBH, THT)
! BD000100   NIMS_VOL_PNW266.SV816_TARIF(VOLSPN, DBH, THT)
! BD000101   NIMS_VOL_PNW266.IV6_TARIF(VOLSPN, DBH, THT)
! BD000102   NIMS_VOL_PNW266.IV8_TARIF(VOLSPN, DBH, THT)
! Reference:
! MacLean, Colin D. and Berger, John M. 1976. Softwood Tree Volume Equations for California species.
! USDA Forest Service Research Note PNW-266
! NVEL Equation Number:
! P02MAC1015, P02MAC1020, P02MAC1081, P02MAC1108, P02MAC1116, P02MAC1117,P02MAC1119, P02MAC1122, P02MAC1202, 
! P16MAC1015, P16MAC1020, P16MAC1081, P16MAC1108, P16MAC1116, P16MAC1117,P16MAC1119, P16MAC1122, P16MAC1202, 
! P32MAC1015, P32MAC1020, P32MAC1081, P32MAC1108, P32MAC1116, P32MAC1117,P32MAC1119, P32MAC1122, P32MAC1202, 
! NOTE: THE EQUATION NUMBER: 
! P02 = THE ORIGINAL CALCULATION FROM PNW-266 
! P16/32 = USING TARIF FROM DNR-24 (CONVERSION FROM CV4)
      SUBROUTINE PNW266(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,ERRFLG)
      CHARACTER*10 VOLEQ
      REAL DBHOB,HTTOT,VOL(15),BFMIND,MTOPP
      INTEGER ERRFLG,SPN
      REAL DBH,THT,CF4,IVF,SVF,D2H
      V(DBH,THT,CF4) = 0.005454154*DBH*DBH*THT*CF4
      ERRFLG = 0
      VOL = 0.0
      CF4 = 0.0
      IVF = 0.0
      SVF = 0.0
      DBH = DBHOB
      THT = HTTOT
      READ(VOLEQ(8:10),'(I3)')SPN
      IF(BFMIND.LT.0.1) BFMIND = 9.0
      IF(SPN.EQ.15)THEN
        CF4 = 0.299039+(1.91272*(1.0/THT))+(0.0000367217*((THT**2)/DBH))
        IF(DBH.GE.BFMIND)THEN
          IF(DBH.LT.11.0)THEN
            IVF = (0.45 + (0.05*DBH))*(1.09597 + (0.000056389*THT*THT))
          ELSE
           IVF =2.08637-(119.839*(1.0/DBH**2))+(.000620285*(THT**2/DBH))
          ENDIF
          SVF = 2.31733-(16.9592*(1.0/DBH))+(.000548156*((THT*THT)/DBH))
        ENDIF
      ELSEIF(SPN.EQ.20)THEN
        CF4 = 0.231237 + .028176*(THT/DBH)
        IF(DBH.GE.BFMIND)THEN
          IVF = 1.54320 + .00133466*(THT*THT/DBH)
         SVF=1.59669-(464.752*(1.0/(DBH*THT)))+(0.00105105*(THT**2/DBH))
        ENDIF
      ELSEIF(SPN.EQ.81)THEN
        CF4 = .225786 + 4.44236*(1/THT)
        IF(CF4.LT.0.27) CF4 = 0.27
        IF(DBH.GE.BFMIND)THEN
          IVF = 1.39269 + .0000259631*THT*THT
          SVF = 1.82080 - 11.7184*(1/DBH)
        ENDIF 
      ELSEIF(SPN.EQ.108)THEN
        CF4 = 0.422709 - .0000612236*(THT*THT/DBH)
        IF(DBH.GE.BFMIND)THEN
          IVF = 2.86258 - (716.659*(1.0/(DBH*THT)))
          SVF = 2.63048 - (850.630*(1.0/(DBH*THT)))
        ENDIF
      ELSEIF(SPN.EQ.117.OR.SPN.EQ.119)THEN
        CF4 = 0.358550 - .488134*(1.0/DBH)
        IF(DBH.GE.BFMIND)THEN
          IVF = 2.75889 - 18.1229*(1.0/DBH) + .000225065*(THT*THT/DBH)
          SVF = 2.88706 - 25.2838*(1.0/DBH)
        ENDIF
      ELSEIF(SPN.EQ.122.OR.SPN.EQ.116)THEN
        CF4 = 0.40206 - .899914*(1.0/DBH)
        IF(DBH.GE.BFMIND)THEN
          IVF = 3.02027 - 22.0313*(1.0/DBH) + .00201362*THT
          SVF = 3.2294 - (585.5*(1.0/(DBH*THT))) - (21.7575*(1.0/DBH))
          IF(SVF.LE.0.7) SVF = 0.7
        ENDIF
      ELSEIF(SPN.EQ.202)THEN
        CF4=0.248569+(.0253524*(THT/DBH))-(0.0000560175*((THT*THT)/DBH))
        IF(DBH.GE.BFMIND)THEN
          IF(THT.LT.57)THEN
            IVF = 1.57535 - (1269.84*(1.0/(DBH*THT)))
     &        + (20.4816*(1.0/DBH)) + (0.0000135387*(THT**2))
     &        + (7333.86*(1.0/(DBH*DBH*THT))) - (128.342*(1.0/(DBH**2)))
          ELSE
            IVF = 1.57535 - (1269.84*(1.0/(DBH*THT)))
     &        + (20.4816*(1.0/DBH)) + (0.0000135387*(THT**2))
          ENDIF
          SVF = 2.58530 - 83.5*(1.0/THT)
        ENDIF
      ELSE
        ERRFLG = 6
        RETURN
      ENDIF
      IF(SPN.NE.81)THEN
        IF(CF4.GT.0.4) CF4 = 0.4
        IF(CF4.LT.0.3) CF4 = 0.3
      ENDIF
      VOL(4) = V(DBH,THT,CF4)
      VOL(2) = V(DBH,THT,SVF)
      VOL(10) = V(DBH,THT,IVF)
      RETURN
      END SUBROUTINE PNW266
! ---------------------------------------------------------------------
      SUBROUTINE PNW266_TARIF(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,ERRFLG)
      CHARACTER*10 VOLEQ
      REAL DBHOB,HTTOT,VOL(15),BFMIND,MTOPP
      INTEGER ERRFLG,SPN  
      REAL TARIF,TARIF_TMP,CV4_TMP,DBH,THT,DBH6,T,BA
      REAL CV4,CVTS,CVT,CV6,CV8,IV6,IV8,SV616,SV632,SV816   
      TARIF_FROM_CVTS(CVTS,DBH) = (CVTS*.912733)/
     & ((1.033*(1.0+1.382937*EXP(-4.015292*(DBH/10.0))))
     &        *(.005454*DBH**2+.087266)-.174533)
      ERRFLG = 0
      VOL = 0.0
      IF(BFMIND.LT.0.1) BFMIND = 9.0
      CALL PNW266(VOLEQ,DBHOB,HTTOT,MTOPP,BFMIND,VOL,ERRFLG)
      IF(ERRFLG.GT.0) RETURN
!     For P16MAC1*** and P32MAC1** equations      
      IF(VOLEQ(2:3).EQ.'16'.OR.VOLEQ(2:3).EQ.'32')THEN
!     Tarif conversion of MacLean CV4 to CVTS,CVT,CV6,CV8,IV6,IV8,SV616,SV632,SV816
        CV4 = VOL(4)
        DBH = DBHOB
        DBH6 = 6.0
        THT = HTTOT
! --Tarif conversion of MacLean CV4 to CVTS
! --Based on Washington State DNR Report 24, Brackett, Micheal
! --and PNW FIA adjustments for trees under 6 inches
!  FUNCTION CVTS_TARIF(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER)
        T = ((1.033*(1.0+1.382937*EXP(-4.015292*(DBH/10.0))))*
     &      (.005454*DBH**2+0.087266)-0.174533)  
        IF(DBH.GE.6.0)THEN     
          BA = DBH**2*.005454154  
          CVTS = CV4*T/(BA-.087266)
        ELSEIF(DBH.LT.5.0)THEN
          TARIF_TMP = CV4*.912733/(36.0*.005454-.087266)
          IF(TARIF_TMP.LE.0.0) TARIF_TMP = 0.01
          TARIF = (0.5*(6.0-DBH)**2)+(1.0+.063*(6.0-DBH)**2)*TARIF_TMP
          IF(TARIF.LE.0.0) TARIF = 0.01
          CVTS = TARIF*T
        ELSE
          CALL PNW266(VOLEQ,DBH6,THT,MTOPP,BFMIND,VOL,ERRFLG)
          CV4_TMP = VOL(4)
          TARIF_TMP = CV4_TMP*.912733/(36.0*.005454-.087266)
          IF(TARIF_TMP.LE.0.0) TARIF_TMP = 0.01
          CVTS = TARIF_TMP*T
        ENDIF
!       --Tarif conversion of MacLean CV4 to CVT
!       FUNCTION CVT_TARIF(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER)
        CVT = CVTS*(.9679-.1051*.5523**(DBH-1.5))
        VOL(1) = CVT
        VOL(14) = CVTS - CVT
        IF(DBH.GE.BFMIND)THEN
!         --Tarif conversion of MacLean CV4 to CV6
!         FUNCTION CV6_TARIF(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER)
          CV6 = CV4*(.993-.993*.62**(DBH-6.0))  
          VOL(4) = CV6
          IF(CV4.GT.CV6) VOL(7) = CV4-CV6
!         --Tarif conversion of MacLean CV4 to IV6 (International)      
!         FUNCTION IV6_TARIF(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER)
          TARIF = TARIF_FROM_CVTS(CVTS,DBH)
          IV6 = CV6*(-2.904154+3.466328*LOG10(DBH*TARIF)
     &          -.02765985*DBH-.00008205*TARIF**2+11.29598/DBH**2)
          VOL(10) = IV6
!         --Tarif conversion of MacLean CV4 to SV6 (Scribner 16 foot rule)
!         FUNCTION SV616_TARIF(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER)
          SV616 =(10**(.174439+.117594*LOG10(DBH)*LOG10((TARIF/.912733))
     &        -8.210585/DBH**2+.236693*LOG10((TARIF/.912733))
     &        -.00001345*(TARIF/.912733)**2-.00001937*DBH**2))*CV6
          VOL(2) = SV616
!         --Tarif conversion of MacLean CV4 to SV6 (Scribner 32 foot rule)
!         FUNCTION SV632_TARIF(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER)
          IF(VOLEQ(2:3).EQ.'32')THEN
            SV632 = SV616*(1.001491-6.924097/TARIF+.00001351*DBH**2)  
            VOL(2) = SV632
          ENDIF 
          IF(MTOPP.GE.8.0)THEN
!           --Tarif conversion of MacLean CV4 to CV8
!           FUNCTION CV8_TARIF(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER)          
            CV8 = CV4*(.983-.983*.65**(DBH-8.6))
            VOL(4) = CV8
            VOL(7) = CV4-CV8
!           --Tarif conversion of MacLean CV4 to IV8 (International)
!           FUNCTION IV8_TARIF(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER)    
            IV8 = IV6*(0.99-0.55*0.485 ** (DBH-9.5))
            VOL(10) = IV8
!           --Tarif conversion of MacLean CV4 to SV8 (Scribner 16 foot rule)
!           FUNCTION SV816_TARIF(SPN IN NUMBER, DBH IN NUMBER, THT IN NUMBER)
            SV816 = SV616*(.99-.58*.484**(DBH-9.5))
            VOL(2) = SV816
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END SUBROUTINE PNW266_TARIF
  