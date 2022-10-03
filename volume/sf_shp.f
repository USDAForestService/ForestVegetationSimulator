!== last modified  09-14-2007
      SUBROUTINE SF_SHP(JSP,GEOSUB,SETOPT,DBHOB,TOTALH,RFLW,RHFW,DBTBH,
     >                  DBT_USER,DBHIB)
      CHARACTER*2 GEOSUB
      INTEGER JSP, SETOPT(6)
      REAL DBHOB,TOTALH,dbt_user,dbhib,DBTBH
      REAL RFLW(6),RHFW(4),fdbt_c1,fdbt_c2,fdbt_ak

c                  estimate diameter inside bark at breast height .
      IF(JSP.GE.3 .AND. JSP.LE.5)THEN
        if(DBT_USER .le. 0.) then
           DBTBH = fdbt_c1(JSP,GEOSUB,DBHOB,TOTALH)
        else
           DBTBH = DBT_USER
        endif                     
        DBHIB = DBHOB  - DBTBH
C        write(*,*)dbh,dbh_ib,DBTBH
      ELSEIF(JSP.GE.11 .AND. JSP.LE.21)THEN
        if(DBT_USER .le. 0.) then
           DBTBH = FDBT_C2(JSP,GEOSUB,SETOPT,DBHOB,TOTALH)
        else
           DBTBH = DBT_USER
        endif                     
        DBHIB = DBHOB  - DBTBH

      ELSEIF(JSP.GE.31 .AND. JSP.LE.36)THEN
        if(DBT_USER .le. 0.) then
           DBTBH = FDBT_AK(JSP,SETOPT,DBHOB,TOTALH)
        else
           DBTBH = DBT_USER
        endif                     
        DBHIB = DBHOB - DBTBH  

      ELSEIF(JSP.GE.22 .AND. JSP.LE.30)THEN
        DBHIB = DBHOB  
      ENDIF
c                  apply empirical equations to find shape parameters.
      IF(JSP.EQ.3) THEN
         CALL SHP_W3(DBHOB,TOTALH,GEOSUB,RFLW,RHFW)
      ELSEIF(JSP.EQ.4) THEN
         CALL SHP_W4(DBHOB,TOTALH,GEOSUB,RFLW,RHFW)
      ELSEIF(JSP.EQ.5) THEN
         CALL SHP_W5(DBHOB,TOTALH,GEOSUB,RFLW,RHFW)
      ELSEIF(JSP.GE.11 .AND. JSP.LE.21)THEN
         CALL SHP_C2(jsp,DBHOB,TOTALH,GEOSUB,RFLW,RHFW)
      ELSEIF (JSP.EQ.22) THEN 
        CALL SHP_BH(DBHOB,TOTALH,RFLW,RHFW)
      ELSEIF(JSP.GE.23 .AND. JSP.LE.30)THEN
        CALL SHP_OT(JSP,DBHOB,TOTALH,RFLW,RHFW)
      ELSEIF(JSP.GE.31 .OR. JSP.LE.36)THEN
        CALL SHP_AK(jsp,GEOSUB,DBHOB,TOTALH,RFLW,RHFW)
      ENDIF
           
      RETURN
      END     