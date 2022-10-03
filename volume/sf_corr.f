!== last modified  09-14-2007
      FUNCTION SF_CORR(JSP,geosub,TOTALH,HI,HJ)
C                     INTERNAL ROUTINES:
C                                          COR_C2
C                                          COR_OT
C                                          COR_BH
C                                          COR_WS
C
c     given 2 heights (hi and hj), estimate the correlation of the
c       errors in the corresponding dib's
      character*2 geosub
      INTEGER JSP
      REAL TOTALH,HI,HJ,SF_CORR,COR_C2,COR_BH,COR_OT,COR_WS,cor_ak
     
      if ( JSP.ge. 11  .and.  jsp.le. 21 ) then
          SF_CORR = COR_C2(JSP,geosub,TOTALH,HI, HJ)
          RETURN     
          
      else if(JSP.eq.22) then
          SF_CORR=COR_BH(TOTALH,HI,HJ)
          RETURN
      else if(JSP.GE.23 .AND. JSP.LE.30) then
          SF_CORR=COR_OT(JSP,TOTALH,HI,HJ)
          RETURN
      else if(JSP.GE.31 .AND. JSP.LE.36) then
          SF_CORR=COR_AK(JSP,TOTALH,HI,HJ)
          RETURN
      else if( JSP.eq.1) then
C          SF_CORR=CORRH(HI,HJ)
          RETURN
      else  if ( JSP.eq.2) then
C          SF_CORR=CORRD(HI,HJ)
          RETURN
      else if (JSP.GE.3 .AND. JSP.LE.5) then
          SF_CORR=COR_WS(JSP,TOTALH,HI,HJ)
          RETURN
      endif   

c      WRITE(*,11)
c11    format(' FUNCTION CORR GIVEN INVALID SPECIES.  STOP')
      RETURN
      END

