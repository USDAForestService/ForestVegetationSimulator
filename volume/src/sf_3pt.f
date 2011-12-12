!== last modified  09-14-2007
      SUBROUTINE SF_3pt(JSP,geosub,setopt,DBH,TOTALH,DBT_BH,NEXTRA,HEX,
     >                  DEX,ZEX,RHFW,RFLW,TAPCOE,F,PINV_Z,FMOD)
c     given meausremts at 1 or 2 heights besides BH, perform
c     necessary calculations prior to estimating all conditional diameters
c     This routine assumes that NEXTRA, hextra(*) and dobex(*) are filled
c           in with correct values.
c
c     NEXTRA    Input   I       Number of extra measurement points (besides HT, DBH)
c                            (currently must = 1 or 2)
c     HEX    Input   R*4()   Vector of extra measurement heights
c     DEX    Input   R*4()   Vector of measured diameters at stated heights.
c     IDTYPE Input  I       1: DEX's are outside bark; 2: DEX's are inside bark.
c     ZEX    Output  R*4()   Standardized errors at those heights. 
c                                (These are Never adjusted for white noise).
      INTEGER NSP
      CHARACTER*2 GEOSUB
      INTEGER SETOPT(6),JSP,NEXTRA,IDTYPE,ineedsl,JX

      REAL DBH,TOTALH,DBT_BH,BH,slope 
      REAL*4 RFLW(6),RHFW(4),TAPCOE(12),ABS_CHNG
      REAL*4 HEX(2),DEX(2),ZEX(2),X,F,DIB,DBT,DIBMOD,DIBACT,SE_LNX
      REAL SF_YHAT,BRK_UPB2,BRK_UPA2,BRK_WS,SF_CORR
      REAL*8 p_inv(2,2)
      REAL FMODMAX ,HFIRSTUP, FMODMAXU, PINV_Z(2),FMOD(3)
      REAL hextra(2), dhatex(2), bark_r(2), dobex(2), z(2)       
     
      REAL*8 P12, P12sq

      PARAMETER (NSP=25)
      Real*4 whitenoise(NSP)/ 1.0, 1.0, 1.0, 1.0, 1.0,
     &                        1.0, 1.0, 1.0, 1.0, 1.0,
     &                        1.0, 1.0, 1.0, 1.0, 1.0,
     &                        1.0, 1.0, 1.0, 1.0, 1.0,
     &                        1.0, 1.0, 1.0, 1.0, 1.0/

      BH=4.5
      IDTYPE = 1
      INEEDSL = 0
      IF(NEXTRA.lt. 1  .or. NEXTRA.gt. 2) then
c            write(*,1) NEXTRA
c1           format(' Subroutine SF_3pt given invalid NEXTRA value =',i4/
c     1             ' Please correct the calling program. STOP. ')
            return
      ENDIF
      if(setopt(1).eq.1) then  
C            FMODMAX=SETC(1)
            FMODMAX=.15
            HFIRSTUP = 0.0
      ENDIF  

      DO 100 JX=1,NEXTRA
          hextra(jx)=HEX(jx)        
          X = hextra(jx)/TOTALH
      
          dhatex(JX)=sf_yhat(JSP,X,TOTALH,ineedsl,slope,RHFW,RFLW,
     >                                                 TAPCOE,F)            
         IF(JSP.GE.11 .AND. JSP.LE.21)THEN
c                           Inside bark model, use upper stem inside bark
            idtype = 2
            IF(IDTYPE.eq.1) then
              dobex(jx) =DEX(jx)                
c                           INGY bark routines in file brk_up.for
              bark_r(jx)=brk_upb2(JSP,DBH,TOTALH,DBT_BH,
     >                                   hextra(jx), dobex(jx))     
              dbt = bark_r(jx)* dobex(jx)
              dib = dobex(jx) - dbt
            else
              dib = DEX(jx)                     
c              bark_r(jx)=brk_upa2(JSP,DBH,TOTALH,DBT_BH,
c     >                                         hextra(jx), dib)          
c              dbt = dib * (bark_r(jx) / (1.0-bark_r(jx)))
c              dobex(jx) = dib + dbt
c              write(*,15)jsp,dbh,totalh,hextra(1),dib,dobex(1) 
c   15         format(i3,5f6.1)
            ENDIF
          ELSEIF(JSP.GE.3 .AND. JSP.LE.5)THEN
c                           Inside bark model
c            idtype = 2
            IF(IDTYPE.eq.1) then
              dobex(jx) =DEX(jx)                
C                westside bark routines in file brk_up.for
              bark_r(jx)=brk_ws(JSP,DBH,TOTALH,DBT_BH,hextra(jx))     
              dbt = bark_r(jx)* dobex(jx)
              dib = dobex(jx) - dbt
            else
              dib = DEX(jx)                     
C                westside bark routines in file brk_up.for
              bark_r(jx)=brk_ws(JSP,DBH,TOTALH,DBT_BH,hextra(jx))     
              dbt = dib * (bark_r(jx) / (1.0-bark_r(jx)))
              dobex(jx) = dib + dbt
c              write(*,15)jsp,dbh,totalh,hextra(1),dib,dobex(1) 
c   15         format(i3,5f6.1)
            ENDIF
c          ELSEIF(JSP.GE.31 .AND. JSP.LE.32)THEN

          ELSE
C                          Outside bark model
            DIB = DEX(JX) 
          ENDIF  
          DIBmod = DHATEX(JX)
          DIBact = DIB

          IF (JSP .ge. 11  .and. JSP .le.21) then
            CALL VAR_C2(JSP,geosub,DBH,TOTALH,hextra(jx),SE_LNX)
            Z(jx) =log(dibact/DIBmod) /SE_LNX 
          
          ELSEIF (JSP.GE.3 .AND. JSP.LE.5 ) THEN
            CALL VAR_C1(JSP,DBH,TOTALH,Hextra(jx),dibmod,dibact,Z(jx))

          ELSEIF (JSP.GE.23 .AND. JSP.LE.30 ) THEN
            CALL VAR_OT(JSP,DBH,TOTALH,hextra(jx),SE_LNX)
            Z(jx)=log( dibact/DIBmod) /SE_LNX 

          ELSEIF (JSP.GE.31 .AND. JSP.LE.36 ) THEN
            CALL VAR_AK(JSP,DBH,TOTALH,hextra(jx),SE_LNX)
            Z(jx)=log( dibact/DIBmod) /SE_LNX 
            
          ELSEIF (JSP.EQ.22) THEN
            CALL VAR_BH(DBH,TOTALH,HEXTRA(JX),SE_LNX)
            Z(jx) = (DIBact - DIBmod) / SE_LNX 
          ENDIF                                                      
          ZEX(jx) = Z(jx)
          if(setopt(1).eq. 1) then
        
              abs_chng= abs(dib-dhatex(jx)) 
              FMODMAX = max( fmodmax, abs_chng /dhatex(jx) )
              if (HEX(jx).gt. bh ) then
                if(hfirstup .eq. 0.0  .or. HEX(jx).lt. hfirstup) then
                  fmodmaxu = min(dhatex(jx), 2.0*abs_chng)/dhatex(jx) 
                  Hfirstup=HEX(jx)
                ENDIF
              ENDIF
             FMOD(1) = FMODMAX
             FMOD(2) = FMODMAXU
             FMOD(3) = HFIRSTUP 
          ENDIF        
c              white noise adjustments (on Z in common, not ZEX argument)
          IF(setopt(4).eq.1  .and. whitenoise(jsp).lt. 1.0) 
     1                            Z(jx) = Z(jx) * whitenoise(jsp)
100   continue                

      if(NEXTRA.eq.2) then
          P12=SF_CORR(JSP,GEOSUB,TOTALH,hextra(1),hextra(2))
          P12SQ=P12*P12
          if(p12.ne.0.) then
              P_INV(1,1) = 1.0d0/(1.0d0-P12SQ)
              P_INV(2,2) = 1.0d0/(1.0d0-P12SQ)
              P_INV(1,2) = -P12/ (1.0d0-P12SQ)
              P_INV(2,1) = -P12/ (1.0d0-P12SQ)
          else
              P_INV(1,1)=1.
              P_INV(2,2)=1.
              P_INV(1,2)=0.
              P_INV(2,1)=0.
          ENDIF
c                              multiply inverse corr matrix by (Z1 Z2)'
          PINV_Z(1) = p_inv(1,1)*Z(1) + p_inv(1,2)*Z(2)            
          PINV_Z(2) = p_inv(2,1)*Z(1) + p_inv(2,2)*Z(2)
      ENDIF

C      IVSTAT_S= .false.
      Return
      END 
