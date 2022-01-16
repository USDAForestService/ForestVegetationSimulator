      SUBROUTINE BRTDEL(IVAC,IREC)
      IMPLICIT NONE
C**********************************************************************
C  **BRTDEL       DATE OF LAST REVISION:  06/05/2014
C----------------------------------------------------------------------
C  Purpose:
C  This Blister Rust model subroutine is called from the FVS routine
C  TREDEL.  It deletes and packs the blister rust tree lists.
C----------------------------------------------------------------------
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BRCOM.F77'
      INCLUDE 'CONTRL.F77'

C.... Local variable declarations.

      INTEGER IVAC, IREC, J
      LOGICAL LGO, DEBUG

C.... See if we need to do some debug.

      CALL DBCHK(DEBUG,'BRTDEL',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,15) ICYC
   15 FORMAT('Entering subroutine BRTDEL: cycle = ',I2)

C.... Call BRATV to see if the Blister Rust Model is active for this
C.... simulation.  If not, then return.

      CALL BRATV(LGO)
      IF(.NOT.LGO) GO TO 1000

C.... Move the Blister Rust records.

      UPMARK(IVAC)=UPMARK(IREC)
      BRHTBC(IVAC)=BRHTBC(IREC)
      GI(IVAC)=GI(IREC)
      RI(IVAC)=RI(IREC)
      BRGD(IVAC)=BRGD(IREC)
      BRPB(IVAC)=BRPB(IREC)
      ESTCAN(IVAC)=ESTCAN(IREC)
      TSTARG(IVAC)=TSTARG(IREC)
      ITCAN(IVAC)=ITCAN(IREC)
      ILCAN(IVAC)=ILCAN(IREC)
      BRAGE(IVAC)=BRAGE(IREC)
      ISTOTY(IVAC)=ISTOTY(IREC)
      IBRTID(IVAC)=IBRTID(IREC)
      ICRED(IVAC)=ICRED(IREC)
      IBRSTAT(IVAC)=IBRSTAT(IREC)

      DO 999 J=1,10
         ISTCAN(J,IVAC)=ISTCAN(J,IREC)
         GIRDL(J,IVAC)=GIRDL(J,IREC)
         DUP(J,IVAC)=DUP(J,IREC)
         DOUT(J,IVAC)=DOUT(J,IREC)
  999 CONTINUE

C.... Common return.

 1000 CONTINUE
      IF(DEBUG) WRITE(JOSTND,1010) ICYC
 1010 FORMAT('Leaving subroutine BRTDEL: cycle = ',I2)
      RETURN
      END
