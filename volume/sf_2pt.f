!== last modified  4-9-2002
      SUBROUTINE SF_2PT(JSP,GEOSUB,SETOPT,DBH,TOTALH,DBT_BH,
     >                               F,RHFW,RFLW,TAPCOE)
c                                                               SF_2PT
c                                  separate entry:              SF_2PTX1
c                                  separate entry:              SF_2PTX
c     Stem form   Specify the 2-point input: DBH and Total height
c     DBH_arg   input   R*4    DBH
c     HT_arg    input   R*4    Total tree height
c     ASP       input   A2     User-defined species code
c                                  (blank is OK  IF specieas already set)
c     IERR      output  I       0  No Error
c                               1  Unrecognized species code
c                               2  Invalid DBH
c                               3  Invalid height
c                               4  This species not available in metric.
c                               5  Supplemental arguments can not be used
c                                  because regression coef file not
c                                  available for this species.
c                               6  Region code not recognized.
      CHARACTER*2 GEOSUB  
C      LOGICAL IVSTAT_2, IVSTAT_S 
c      LOGICAL METRIC, SUPPLEMENT
C      REAL*4 SUPV_ARG(NSUP)
      INTEGER  SETOPT(6),IERR
C      LOGICAL  SF_MCOE(NSP)
      REAL*4 RHFW(4),RFLW(6),TAPCOE(12),F,YHAT_BH
      REAL DBH, TOTALH, DBH_IB, DBT_BH, BH, RH, slope
      REAL DBT_USER,SF_YHAT
      INTEGER JSP, INEEDSL

      DBT_USER = dbt_bh
      bh = 4.5              
C      DO 10 I=1,NSUP
C         Supv(I) =supv_arg(I)
C         if (supv(i).ne. 0.)  SUPPLEMENT=.TRUE.
C10    Continue

      INEEDSL=0

      IF( JSP.eq.0)then
          IERR=1
          RETURN 
      ENDIF

C      if (JSP.ge.3 .and. METRIC) then
C        IERR=4
C        RETURN
C      ENDIF
     
C      IVSTAT_2 = .false.
C      IVSTAT_S = .false.
      F=1.0                             
C       find shape parameters
      CALL SF_SHP(JSP,GEOSUB,SETOPT,DBH,TOTALH,RFLW,RHFW,DBT_BH,
     >              DBT_USER,DBH_IB)

c          use formulas by Flewelling and Raynes to  calc. taper coef's
      
      CALL SF_TAPER (RHFW,RFLW,TAPCOE)

c          calculate scaling factor f
      RH = BH/TOTALH
      
      YHAT_BH = SF_YHAT(JSP,RH,totalh,ineedsl,slope,RHFW,RFLW,TAPCOE,F)
      F  = DBH_IB / YHAT_BH

c                  if segment volumes may be used, define extra sections 
C           IF(DBH_IB .gt. 6.5) then
C           IVEXTRA=3
C           DVEXTRA(1) = 6.0
C           DVEXTRA(2) = 4.0
C           DVEXTRA(3) = 0.0
C           if (metric)  then
C                 dvextra(1)= dvextra(1)*2.54
C                 dvextra(2)= dvextra(2)*2.54
C               ENDIF
C           HVEXTRA(1)=0.
C           HVEXTRA(2)=0.
C           HVEXTRA(3)=RHI1*TOTALH  
C         else
C           IVEXTRA=0.0
C         ENDIF    
 
      RETURN
      END   
