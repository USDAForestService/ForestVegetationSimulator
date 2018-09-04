      SUBROUTINE EXESTB
      IMPLICIT NONE
C----------
C BASE $Id$
C----------
C
C     EXTRA REFERENCES FOR REGENERATION ESTABLISHMENT
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'CALDEN.F77'
C
C
COMMONS
C
C
      CHARACTER*8 KEYWRD,NOESTB,PASKEY
      REAL ARRAY(1)
      DIMENSION LNOTBK(1)
      INTEGER IPVARS(1)
      INTEGER KEY,NPNVRS,IMC1,ITREI,IPTKNT,JOSTND,I,ID,ISHAG,JPLOT
      INTEGER JSSP,ICALL,LENGTH
      REAL DBH,PREM
      LOGICAL LACTV,LNOTBK,LFG,LKECHO
      REAL RDANUW
      INTEGER IDANUW
      CHARACTER*8 CDANUW
      LOGICAL LDANUW
C
      DATA NOESTB/'*NO ESTB'/
C----------
C  ENTRY ESIN
C----------
      ENTRY ESIN(PASKEY,ARRAY,LNOTBK,LKECHO)
        CALL ERRGRO (.TRUE.,11)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        CDANUW(1:8) = PASKEY(1:8)
        RDANUW = ARRAY(1)
        LDANUW = LNOTBK(1)
        LDANUW = LKECHO
      RETURN
C----------
C  ENTRY ESNUTR
C----------
      ENTRY ESNUTR
      RETURN
C----------
C  ENTRY ESKEY
C----------
      ENTRY ESKEY(KEY,KEYWRD)
        KEYWRD=NOESTB
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = KEY
      RETURN
C----------
C  ENTRY ESNOAU
C----------
      ENTRY ESNOAU (KEYWRD)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        CDANUW(1:8) = KEYWRD(1:8)
      RETURN
C----------
C  ENTRY ESINIT
C----------
      ENTRY ESINIT
      RETURN
C----------
C  ENTRY ESOUT
C----------
      ENTRY ESOUT(LFG)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        LDANUW = LFG
      RETURN
C----------
C  ENTRY ESPLT1
C----------
      ENTRY ESPLT1(ITREI,IMC1,NPNVRS,IPVARS)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = ITREI
        IDANUW = IMC1
        IDANUW = NPNVRS
        IDANUW = IPVARS(1)
      RETURN
C----------
C  ENTRY ESPLT2
C----------
      ENTRY ESPLT2(JOSTND,IPTKNT)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = JOSTND
        IDANUW = IPTKNT
      RETURN
C----------
C  ENTRY ESEZCR
C----------
      ENTRY ESEZCR(JOSTND)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = JOSTND
      RETURN
C----------
C  ENTRY ESFLTR
C----------
      ENTRY ESFLTR
        DO 10 I=1,MAXPLT
        BAAINV(I)=0.0
   10   CONTINUE
        TPACRE=0.0
      RETURN
C----------
C  ENTRY ESXTRA
C----------
      ENTRY ESXTRA
      RETURN
C----------
C  ENTRY ESHTS
C----------
      ENTRY ESHTS
      RETURN
C----------
C  ENTRY ESDTLS
C----------
      ENTRY ESDTLS (ID)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = ID
      RETURN
C----------
C  ENTRY ESTUMP
C----------
      ENTRY ESTUMP (JSSP,DBH,PREM,JPLOT,ISHAG)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = JSSP
        RDANUW = DBH
        RDANUW = PREM
        IDANUW = JPLOT
        IDANUW = ISHAG
      RETURN
C----------
C  ENTRY ESADDT
C----------
      ENTRY ESADDT (ICALL)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = ICALL
      RETURN
C----------
C     EXTRA REFERENCES FOR THE PARALLEL PROCESSING SYSTEM.
C
C  ENTRY ESGET
C----------
      ENTRY ESGET
      RETURN
C----------
C  ENTRY ESPUT
C----------
      ENTRY ESPUT
      RETURN
C----------
C  ENTRY ESSPRQ
C----------
      ENTRY ESSPRQ (LENGTH)
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
        IF(.TRUE.)RETURN
        IDANUW = LENGTH
      RETURN
C----------
C  ENTRY ESACTV
C----------
      ENTRY ESACTV (LACTV)
      LACTV=.FALSE.
      RETURN
C
      END
