      SUBROUTINE MSPPPT (WK3, IPNT, ILIMIT)
***********************************************************************
*  **MSPPPT--MS  DATE OF LAST REVISION:  06/25/13
*----------------------------------------------------------------------
*  Purpose:
*     Put (store) the mistletoe model data for a given stand.
*  This is part of the Parallel Processing Extension.
*----------------------------------------------------------------------
*
*  Call list definitions:
*     ILIMIT: (I)  Size of print buffer WK3.
*     IPNT:   (IO) Pointer to curent element in print buffer WK3.
*     WK3:    (IO) Work array used as a print buffer.
*
*  Local variable definitions:
*     I:      Loop counter.
*     INTS:   Array of length MXI to hold integer values.
*     LOGICS: Array of length MXL to hold logical values.
*     REALS:  Array of length XMR to hold real values.
*
*  Common block variables and parameters:
*     CSPARR: From MISCOM; 2-char abbreviations for species names.
*     DGPDMR: From MISCOM; diameter growth potentials by DMR.
*     DMFILE: From MISCOM; DM table output permanent file names.
*     DMFLAG: From MISCOM; true if mistletoe is present in the stand.
*     DMMMLT: From MISCOM; user-adjustable mortality multiplier.
*     DMMTPA: From MISCOM; TPA killed by mistletoe.
*     DMRMIN: From MISCOM; minimum DBH for DMR/DMI statistics.
*     FSTMIS: From MISCOM; flag for summary output table headings.
*     FSTTBL: From MISCOM; flag for detail output table headings.
*     HGPDMR: From MISCOM; height growth potentials by DMR.
*     IMIST:  From MISCOM; individual tree mistletoe ratings.
*     ISVSP4: From MISCOM; top 4 most DM infected species by number.
*     ITRN:   From CONTRL; actual number of trees in stand.
*     JOSTND: From CONTRL; unit number of stand output.
*     JRAN:   From MISCOM; seed for DM random number generator.
*     LSORT4: From MISCOM; flag to set species by infection.
*     MAXCYC: From CONTRL; maximum number of cycles.
*     MAXSP:  From PRGPRM; maximum number of species.
*     MAXTRE: From PRGPRM; maximum number of tree records.
*     MISCYC: From MISCOM; flag to process MISTGMOD once per cycle.
*     MISFIT: From MISCOM; which species affected by DM (see MISINT).
*     MISFLG: From MISCOM; flag to turn mistletoe effects on or off.
*     MISTBL: From MISCOM; what species desired in detailed output.
*     NOHEAD: From MISCOM; flag for printing table headings to files.
*     PMCSP:  From MISCOM; mortality eqn. coefficients by species.
*     PRFMST: From MISCOM; species removal preferences due to DM.
*     PRTMIS: From MISCOM; flag for turning DM printing on or off.
*     USEMRT: From MISCOM; flag for using DM model mortality eqns.
*     YNGMLT: From MISCOM; DM spread probability (-) multiplier.
*     YPLMLT: From MISCOM; DM spread probability (+) multiplier.
*
*  Revision History :
*  01-MAR-95  Lance R. David (MAG)
*     Storage of DMGMLT (from MISCOM; user-adjustable diameter 
*     growth multiplier) removed due to change of MISTGMOD keyword.
*     This variable is no longer used.
*  10-MAR-08; Lance R. David (FHTET)
*     Added new common block variables that control output tables.
*     Variables Added are: DMFLAG, PRTTBL, LSORT4, MISTBL and ISVSP4
*  21-APR-09  Lance R. David (FMSC)
*     Added varaibles IMOUT_ to integer scalars section.
*  07-JUL-11  Lance R. David (FMSC)
*     Added HGPDMR to arrays section.
*  06-JUN-13  Lance R. David (FMSC)
*     Corrected write of MISCYC array, only 1 dimension being processed.
*
***********************************************************************
      IMPLICIT NONE

C.... Parameter statements.
      INTEGER MXI,MXL,MXR
      PARAMETER (MXL=8,MXR=1,MXI=5)

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'

C.... Common include files.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'MISCOM.F77'

C.... Variable declarations.

      LOGICAL LOGICS(MXL)
      INTEGER ILIMIT,INTS(MXI),IPNT
      REAL REALS(MXR),WK3(MAXTRE)

C.... Put the logical scalars.

      LOGICS ( 1) = USEMRT
      LOGICS ( 2) = PRTMIS
      LOGICS ( 3) = MISFLG
      LOGICS ( 4) = FSTTBL
      LOGICS ( 5) = FSTMIS
      LOGICS ( 6) = DMFLAG
      LOGICS ( 7) = PRTTBL
      LOGICS ( 8) = LSORT4
      CALL LFWRIT (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)

C.... Put the real scalars.

      REALS (1) = DMRMIN
      CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, MXR, 2)

C.... Put the integer scalars.

      INTS (1) = JRAN
      INTS (2) = IMOUT1
      INTS (3) = IMOUT2
      INTS (4) = IMOUT3
      INTS (5) = IMOUT4
      CALL IFWRIT (WK3, IPNT, ILIMIT, INTS, MXI, 2)

C.... Put the arrays.
C.... Note that MISCYC, PRFMST, PMCSP, and DGPDMR are 2-d arrays.

      CALL IFWRIT (WK3, IPNT, ILIMIT, MISFIT, MAXSP, 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, IMIST,  ITRN,  2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, IDMSOUT, 4, 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, ISVSP4,  4, 2)
      CALL IFWRIT (WK3, IPNT, ILIMIT, DMPLT, MAXPLT, 2)

      CALL LFWRIT (WK3, IPNT, ILIMIT, MISCYC, MAXCYC*2, 2)
      CALL LFWRIT (WK3, IPNT, ILIMIT, MISTBL, MAXSP, 2)

      CALL BFWRIT (WK3, IPNT, ILIMIT, YPLMLT, MAXSP, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, YNGMLT, MAXSP, 2)

      CALL BFWRIT (WK3, IPNT, ILIMIT, PRFMST, MAXSP*6, 2)

      CALL BFWRIT (WK3, IPNT, ILIMIT, PMCSP, MAXSP*3, 2)

      CALL BFWRIT (WK3, IPNT, ILIMIT, DMMTPA, ITRN, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, DMMMLT, MAXSP, 2)

      CALL BFWRIT (WK3, IPNT, ILIMIT, DGPDMR, MAXSP*7, 2)
      CALL BFWRIT (WK3, IPNT, ILIMIT, HGPDMR, MAXSP*7, 2)

      RETURN
      END
