      SUBROUTINE MISPRT
		  IMPLICIT NONE
C----------
C METRIC-NEWMIST $Id$
C----------
*  Purpose:
*     Produces dwarf mistletoe infection information and mortality
*  summary output by cycle in table format to be appended to the
*  Prognosis standard output listing. Also processes the MISTPRT and
*  MISTABLE keywords.
*----------------------------------------------------------------------
*
*  Call list definitions:
*
*  Local variable definitions:
*     CFMT:   Character array to hold write formats for DBH class loop.
*     CSP:    Array to hold 2 char. rep. of top 4 infected species.
*     DCDMI:  Array - average DMRs inf. trees only by 2" DBH class.
*     DCDMR:  Array - average DMRs by 2" DBH classes.
*     DCINF:  Array - sums of infected TPA by 2" DBH classes.
*     DCMRT:  Array - sums of DM mortality (TPA) by 2" DBH classes.
*     DCSUM:  Array - sums of DM ratings (IMIST) by 2" DBH classes.
*     DCTPA:  Array - sums of TPA by 2" DBH classes.
*     DCTPAX: Array - sums of TPA by 2" DBH classes, DBH >= DMRMIN.
*     DEBUG:  Logical flag to turn debug on or off.
*     DMFLAG: Logical flag, true if mistletoe is present in the stand.
*     IJKLM:  Loop counters.
*     I1:     Beginning tree record pointer for a species.
*     I2:     Ending tree record pointer for a species.
*     I3:     Tree record loop counter.
*     IACTK:  Passed back from OPGET (unused).
*     IDATE:  Passed back from OPGET (unused).
*     IDCLAS: Loop counter for 2" DBH classes.
*     IDMR:   Current tree mistletoe rating.
*     INFNO:  Species number of current (infected) species.
*     INFSPC: Infected species loop counter.
*     IPRINT: Flag to turn mistletoe output on or off.
*     ISPC:   Current species number and species loop counter.
*     ISVSP4: Array of top 4 most DM infected species by number.
*     ITREE:  Tree loop counter.
*     KODE:   Error code returned from MYOPEN.
*     NAGE:   Current stand age.
*     NP:     Passed back from OPGET (unused).
*     NTODO:  Number of actions to perform in a cycle.
*     NYEAR:  Current year in cycle.
*     P:      Current trees per acre.
*     PRM:    Array containing MISTPRT/MISTABLE keyword values.
*     PRTREE: Logical flag to print DM tree-level output file.
*     SDDMI:  2D array - average DMRs inf. only by species & DBH class.
*     SDDMR:  2D array - average DMRs by species & DBH class.
*     SDINF:  2D array - sums of infected TPA by species & DBH class.
*     SDIN0:  2D array - sums of TPA with DMR 0 by species & DBH class.
*     SDIN12: 2D array - sums of TPA with DMR 1-2 by species/DBH class.
*     SDIN34: 2D array - sums of TPA with DMR 3-4 by species/DBH class.
*     SDIN56: 2D array - sums of TPA with DMR 5-6 by species/DBH class.
*     SDMRT:  2D array - sums of mortality TPA by species & DBH class.
*     SDSUM:  2D array - sums of DM ratings (IMIST) by spec./DBH class.
*     SDTPA:  2D array - sums of TPA by species and DBH class.
*     SDTT0:  Array containing totals of SDIN0 by species.
*     SDTT12: Array containing totals of SDIN12 by species.
*     SDTT34: Array containing totals of SDIN34 by species.
*     SDTT56: Array containing totals of SDIN56 by species.
*     SORTSP: Array containing infection levels of top 4 most inf. spec.
*     SPDMI:  Array containing species average DMI.
*     SPDMI4: Array for species average DMI (top 4 only).
*     SPDMR:  Array containing species average DMR.
*     SPDMR4: Array for species average DMR (top 4 only).
*     SPDMRS: Array containing sum of DMR's by species.
*     SPINF4: Array for species TPA infected with DM (top 4 only).
*     SPMRT4: Array for species TPA mortality due to DM (top 4 only).
*     SPPIN4: Array for % of species TPA infected (top 4 only).
*     SPPMR4: Array for % of species TPA mortality (top 4 only).
*     SPPOC4: Array for % species occurance in stand total TPA (top 4).
*     SPTPAI: Array containing total # infected TPA by species.
*     SPTPAM: Array containing species TPA mortality.
*     SPTPAT: Array containing total # TPA by species.
*     SPTPAX: Array containing total # TPA by species, DBH >= DMRMIN.
*     STBAI:  Stand basal area infected with DM.
*     STBAM:  Stand basal area mortality due to DM.
*     STDMI:  Stand average DMI (based on all trees).
*     STDMR:  Stand average DMR (based on infected-only trees).
*     STDMRS: Stand total sum of DMR's.
*     STPIT:  Stand % of TPA infected with DM.
*     STPIV:  Stand % of volume infected with DM.
*     STPMT:  Stand % of TPA lost to DM mortality.
*     STPMV:  Stand % of volume lost to DM mortality.
*     STTPAI: Stand total # infected TPA.
*     STTPAM: Stand TPA mortality due to DM.
*     STTPAT: Stand total # TPA.
*     STTPAX: Stand total # TPA, DBH >= DMRMIN.
*     STVOL:  Stand total volume in cubic feet.
*     STVOLI: Stand volume infected with DM.
*     STVOLM: Stand volume mortality due to DM.
*
*  Common block variables and parameters:
*     BA:     From PLOT; current stand basal area.
*     CFV:    From ARRAYS; current tree total cubic foot volume.
*     CSPARR: From MISCOM; array to hold 2 char. rep. of all species.
*     DBH:    From ARRAYS; current tree DBH.
*     DMFLAG: From MISCOM; true if mistletoe is present in the stand.
*     DMMTPA: From MISCOM; array containing mistletoe TPA mortality.
*     DMRMIN: From MISCOM; minimum tree DBH for DMR/DMI statistics.
*     FSTMIS: From MISCOM; to print summary headings first time only.
*     FSTTBL: From MISCOM; to print detail headings first time only.
*     IAGE:   From PLOT; original stand age.
*     ICYC:   From CONTRL; current cycle index number.
*     IMIST:  From MISCOM; array containing tree record DMRs.
*     IND1:   From ARRAYS; tree list access.
*     ISCT:   From CONTRL; species ordered list pointer.
*     ISP:    From ARRAYS; current tree species.
*     ISVSP4: From MISCOM; Array of top 4 most DM infected species by number.
*     ITRN:   From CONTRL; current number of tree records.
*     IY:     From CONTRL; inventory year.
*     JOSTND: From CONTRL; unit number of stand output.
*     LSORT4: From MISCOM; Logical flag to set species by infection.
*     LSTART: From CONTRL; true during summarization of input.
*     MAXSP:  From PRGPRM; maximum # species.
*     MAXTRE: From PRGPRM; maximum # tree records.
*     MGMID:  From PLTCHR; management code.
*     MISFIT: From MISCOM; which species affected by DM (see MISINT).
*     MISTBL: From MISCOM; Logical array - what species desired in detailed output.
*     NPLT:   From PLTCHR; stand ID.
*     PRTTBL: From MISCOM; Logical flag to print DM detailed output tables.
*     PROB:   From ARRAYS; trees per acre per tree record.
*     PRTMIS: From MISCOM; TRUE if DM output tables are to be printed.
*
* Revision History:
*     24-FEB-98; Update Lance David
*        Added warning on DMR column of Stand Average Summary Table
*        (file 2) indicating inclusion of nonhost species.
*     02-AUG-99; Lance R. David (FHTET-FC)
*        Added definition and data statements for MYACTS array
*        and replaced the activity code literal in the CALL OPFIND
*        statements.
*        This change was provided by Bob Havis (FMSC) to eliminate
*        LF95 FORTRAN compiler warnings.
*     23-DEC-99; Lance R. David (FHTET-FC)
*        Updated for expansion of FVS stand id (variable NPLT)
*        from 8 to 26 characters.
*     21-DEC-00; Lance R. David (FHTET-FC)
*        Added additional condition when setting variable MISTBL so
*        that tables will not be generated for host species not present
*        in the simulation. After processing of the tree list the
*        condition of having infections present on the species was added
*        before the specie/diameter class table is written (array SPTPAI).
*     02-AUG-01; Lance R. David (FHTET-FC)
*        Initialization of SORTSP array.
*     23-JAN-02; Lance R. David (FHTET-FC)
*        Moved if statements controling writing of tables 1, 2 and 3 so
*        that tables 1 and 2 will print automatically when mistletoe damage
*        codes are present in the data set and the MISTOFF keyword has not
*        been issued.
*        The MISTPRT keyword now controls printing of the DBH Class Tables
*        (table 3) only.
*     08-AUG-02; Lance R. David (FHTET-FC)
*        Restricted some controling variables initialization to cycle 0
*        so that once a table/species began to print, it would print for
*        the duration of the simulation. This addresses truncation of
*        tables that resulted from successful dwarf mistletoe sanitation
*        management actions.
*     29-AUG-02; Lance R. David (FHTET-FC)
*        Variables for sorting and ordering of top 4 species, LSORT4 and
*        ISVSP4, were initialized with data statements which resulted in
*        these variables not being reset for the next stand when processing
*        multiple stands in single execution. These variables moved to
*        cycle 0 initialization section.
*     13-JAN-04; Lance R. David (FHTET)
*        Modifications provided by Don Robinson of ESSA to place Mistletoe
*        summary tables into the standard FVS output file instead of
*        directing them to external files. Capability to use the FVS 
*        Database extension the DM summaries was also added.
*     14-MAR-05; Lance R. David (FHTET)
*        Added calls to GETID for assignment of report ID Numbers at
*        time of header writing. Assignments were originally made in
*        subroutine MISIN0.
*     07-MAR-08; Lance R. David (FHTET)
*        Moved local variables to common, necessary for output table 
*        control that need to be maintained for each stand in PPE mode.
*        Variables moved are: DMFLAG, PRTTBL, LSORT4, MISTBL and ISVSP4
*  21-APR-09; Lance R. David (FMSC)
*        Variables IMOUT_ moved to MISCOM.(thanks to Don Robinson)
***********************************************************************
      
C     Parameter statements.

C     Parameter include files.

      INCLUDE 'PRGPRM.F77'

C     Common include files.

      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'MISCOM.F77'
      INCLUDE 'DMCOM.F77'
      INCLUDE 'METRIC.F77'

C     Variable declarations.

      LOGICAL      DEBUG
      INTEGER      MYACTS(1)
      INTEGER I,II,I1,I2,I3,IACTK,IDATE,IDBH,IDBSKODE,IDCLAS,IDMR,
     &        INFNO,INFSPC,ISPC,ITEMP,ITREE,IVAL,J,K,L,M,N,NAGE,NP,
     &        NTODO,NYEAR
      REAL    P,STBAI,STBAM,STDMI,STDMR,STDMRS,STPIT,STPIV,STPMT,STPMV,
     &        STTPAI,STTPAM,STTPAT,STTPAX,STVOL,STVOLI,STVOLM,TEMP
      REAL    SPTPAM(MAXSP),SPDMR(MAXSP),SPDMI(MAXSP),
     &   SPDMR4(4),SPDMI4(4),SPINF4(4),
     &   SPMRT4(4),SPPIN4(4),SPPMR4(4),SPPOC4(4),PRM(2),DCTPA(20),
     &   DCINF(20),DCMRT(20),DCSUM(20),DCDMR(20),DCDMI(20),DCTPAX(20),
     &   SDTPA(MAXSP,20),SDINF(MAXSP,20),SDMRT(MAXSP,20),
     &   SDSUM(MAXSP,20),SDDMR(MAXSP,20),SDDMI(MAXSP,20),
     &   SDIN0(MAXSP,20),SDIN12(MAXSP,20),SDIN34(MAXSP,20),
     &   SDIN56(MAXSP,20),SDTT0(MAXSP),SDTT12(MAXSP),SDTT34(MAXSP),
     &   SDTT56(MAXSP),SORTSP(4),SPDMRS(MAXSP),SPTPAI(MAXSP),
     &   SPTPAT(MAXSP),SPTPAX(MAXSP)

      REAL         SPINF4_M(4),SPMRT4_M(4),DCTPA_M(10),
     &             DCINF_M(10),DCMRT_M(10)
      CHARACTER*2  CSP(4)
      CHARACTER*3  NLABS(5)
      CHARACTER*42 CFMT(20)

C     Data statements.

      DATA NLABS  /'TPH','INF','MRT','DMR','DMI'/

C     These are formats for writing out the species/DBH class data
C     to the DM detail output file within a DBH class loop.

      DATA (CFMT(I),I=1,10)
     &   /'(1X,I5,''   0- 5cm '',7(6X,F7.1),2(4X,F3.1))',
     &    '(1X,I5,''   5-10cm '',7(6X,F7.1),2(4X,F3.1))',
     &    '(1X,I5,''  10-15cm '',7(6X,F7.1),2(4X,F3.1))',
     &    '(1X,I5,''  15-20cm '',7(6X,F7.1),2(4X,F3.1))',
     &    '(1X,I5,''  20-25cm '',7(6X,F7.1),2(4X,F3.1))',
     &    '(1X,I5,''  25-30cm '',7(6X,F7.1),2(4X,F3.1))',
     &    '(1X,I5,''  30-35cm '',7(6X,F7.1),2(4X,F3.1))',
     &    '(1X,I5,''  35-40cm '',7(6X,F7.1),2(4X,F3.1))',
     &    '(1X,I5,''  40-45cm '',7(6X,F7.1),2(4X,F3.1))',
     &    '(1X,I5,''  45-50cm '',7(6X,F7.1),2(4X,F3.1))'/
      DATA (CFMT(I),I=11,20)
     &   /'(1X,I5,''  50-55cm '',7(6X,F7.1),2(4X,F3.1))',
     &    '(1X,I5,''  55-60cm '',7(6X,F7.1),2(4X,F3.1))',
     &    '(1X,I5,''  60-65cm '',7(6X,F7.1),2(4X,F3.1))',
     &    '(1X,I5,''  65-70cm '',7(6X,F7.1),2(4X,F3.1))',
     &    '(1X,I5,''  70-75cm '',7(6X,F7.1),2(4X,F3.1))',
     &    '(1X,I5,''  75-80cm '',7(6X,F7.1),2(4X,F3.1))',
     &    '(1X,I5,''  80-85cm '',7(6X,F7.1),2(4X,F3.1))',
     &    '(1X,I5,''  85-90cm '',7(6X,F7.1),2(4X,F3.1))',
     &    '(1X,I5,''  90-95cm '',7(6X,F7.1),2(4X,F3.1))',
     &    '(1X,I5,''  95+  cm '',7(6X,F7.1),2(4X,F3.1))'/

      DATA MYACTS /2007/

C     Check for debug.

      CALL DBCHK(DEBUG,'MISPRT',6,ICYC)

      IF (DEBUG) WRITE(JOSTND,5)ICYC,LSTART
    5 FORMAT(' Begin MISPRT: Cycle, LSTART = ',I5,L8)

C     Print control done in cycle 0 only so that once started, a table
C     prints for full duration of the simulation.(LRD 08/09/02)
C     LSORT4 and ISVSP4 initializations (LRD 08/29/02)

      IF (ICYC .EQ. 0) THEN
        DMFLAG = .FALSE.
        PRTTBL = .FALSE.
        LSORT4 = .FALSE.

        DO I = 1,4
          ISVSP4(I) = 0
        ENDDO

        DO I = 1,MAXSP
          MISTBL(I) = .FALSE.
        ENDDO
      ENDIF

C     Stand total variable initializations.

      STTPAM = 0.0
      STVOL  = 0.0
      STVOLI = 0.0
      STVOLM = 0.0
      STBAI  = 0.0
      STBAM  = 0.0
      STDMR  = 0.0
      STDMI  = 0.0
      STPIT  = 0.0
      STPIV  = 0.0
      STPMT  = 0.0
      STPMV  = 0.0
      STTPAI = 0.0
      STTPAT = 0.0
      STTPAX = 0.0
      STDMRS = 0.0

C     Species variable initializations.

      DO I = 1,MAXSP
         SPTPAM(I) = 0.0
         SPDMR(I)  = 0.0
         SPDMI(I)  = 0.0
         SDTT0(I)  = 0.0
         SDTT12(I) = 0.0
         SDTT34(I) = 0.0
         SDTT56(I) = 0.0
         SPTPAI(I) = 0.0
         SPTPAT(I) = 0.0
         SPTPAX(I) = 0.0
         SPDMRS(I) = 0.0
      ENDDO

C     Species variable initializations (top 4 most infected species).

      DO I = 1,4
         SORTSP(I) = 0.0
         SPDMR4(I) = 0.0
         SPDMI4(I) = 0.0
         SPINF4(I) = 0.0
         SPMRT4(I) = 0.0
         SPPIN4(I) = 0.0
         SPPMR4(I) = 0.0
         SPPOC4(I) = 0.0
      ENDDO

C     DBH class variable initializations.
C     20 DBH classes at 2 inch intervals; 0-3, 3-5, 5-7,..., 37-39, 39+.

      DO I = 1,20
         DCTPA(I)  = 0.0
         DCTPAX(I) = 0.0
         DCINF(I)  = 0.0
         DCMRT(I)  = 0.0
         DCSUM(I)  = 0.0
         DCDMR(I)  = 0.0
         DCDMI(I)  = 0.0
      ENDDO

C     Species/DBH class variable initializations.
C     11 species by 20 DBH classes at 2 inch intervals (see above).

      DO I = 1,MAXSP
         DO J = 1,20
            SDTPA(I,J)  = 0.0
            SDINF(I,J)  = 0.0
            SDIN0(I,J)  = 0.0
            SDIN12(I,J) = 0.0
            SDIN34(I,J) = 0.0
            SDIN56(I,J) = 0.0
            SDMRT(I,J)  = 0.0
            SDSUM(I,J)  = 0.0
            SDDMR(I,J)  = 0.0
            SDDMI(I,J)  = 0.0
         ENDDO
      ENDDO

      IF(DEBUG) WRITE(JOSTND,50)
   50 FORMAT(' in MISPRT: Begin summary processing')

C        Calculate species mistletoe infection totals.

         DO 90 ISPC=1,MAXSP
            I1=ISCT(ISPC,1)

C           If there are no trees of this species, skip out.

            IF(I1.EQ.0) GO TO 90
            I2=ISCT(ISPC,2)
            DO 80 I3=I1,I2
               I=IND1(I3)
               P=PROB(I)
               IDMR=IMIST(I)

               IF(IDMR.GT.0) DMFLAG=.TRUE.

               IF(DBH(I).GE.DMRMIN) THEN
                  IF(IDMR.GT.0) SPTPAI(ISPC)=SPTPAI(ISPC)+P
                  SPDMRS(ISPC)=SPDMRS(ISPC)+IDMR*P
                  SPTPAX(ISPC)=SPTPAX(ISPC)+P
               ENDIF
               SPTPAT(ISPC)=SPTPAT(ISPC)+P
   80       CONTINUE
         STTPAI=STTPAI+SPTPAI(ISPC)
         STTPAT=STTPAT+SPTPAT(ISPC)
         STTPAX=STTPAX+SPTPAX(ISPC)
         STDMRS=STDMRS+SPDMRS(ISPC)
   90 CONTINUE

C     If DMFLAG is false, there is no mistletoe in any species
C     in this cycle. Skip the printing.

      IF(.NOT.DMFLAG .AND. .NOT.NEWMOD) GOTO 9000

C     Process MISTABLE keyword (activity code 2007).

      NTODO=0
      CALL OPFIND(1,MYACTS(1),NTODO)

      IF(NTODO.NE.0) THEN
         DO 120 I=1,NTODO
            CALL OPGET(I,2,IDATE,IACTK,NP,PRM)

C           Test for zero/first cycle.

            IF(.NOT.LSTART) CALL OPDONE(I,IY(ICYC))

C           Check for a particular species number.
C           MISFIT insures that only species of this variant that
C           are affected by mistletoe will be printed in detailed
C           output - others will not be included in printing.
C           The presence of trees (records) of the particular species
C           is determined from the ISCT array and table will also not
C           be generated it the species is not currently in the simulation.

            ISPC=IFIX(PRM(1))
            IF(ISPC.NE.0) THEN
               PRTTBL=.TRUE.
               MISTBL(ISPC) = (MISFIT(ISPC) .EQ. 1 .AND.
     &                         ISCT(ISPC,1) .GT. 0)
            ELSE

C              Otherwise defaults to all species.

               PRTTBL=.TRUE.
               DO 110 ISPC=I,MAXSP
                  MISTBL(ISPC) = (MISFIT(ISPC) .EQ. 1 .AND.
     &                            ISCT(ISPC,1) .GT. 0)
  110          CONTINUE
            ENDIF
  120    CONTINUE
      ENDIF

C     PRINT DETAILED TREELIST AND LIGHT TABLES, CONTINGENT
C     ON 'DMTABLE' KEYWORD

      IF (LDETAIL) THEN
        CALL DBSMIS5(NYEAR,IDBSKODE)
        CALL DBSMIS6(NYEAR,IDBSKODE)
      ENDIF

C     Sort for the top four most infected species only once. This may
C     happen in the first cycle or it may happen later if infections
C     were introduced. Anyway, LSORT4 will be FALSE if not sorted yet.

      IF(.NOT.LSORT4) THEN
         LSORT4=.TRUE.

         DO 180 ISPC=1,MAXSP

C           Loop and save top 4 most infected species (based on SPTPAI).
C           The species numbers are saved in ISVSP4 and sorted by
C           infection level (saved in SORTSP) from highest to lowest.

            IF(SPTPAI(ISPC).GT.SORTSP(4)) THEN
               SORTSP(4)=SPTPAI(ISPC)
               ISVSP4(4)=ISPC

               DO 170 I=1,3
                  DO 160 J=I+1,4
                     IF(SORTSP(J).GT.SORTSP(I)) THEN
                        TEMP=SORTSP(I)
                        ITEMP=ISVSP4(I)
                        SORTSP(I)=SORTSP(J)
                        ISVSP4(I)=ISVSP4(J)
                        SORTSP(J)=TEMP
                        ISVSP4(J)=ITEMP
                     ENDIF
  160             CONTINUE
  170          CONTINUE
            ENDIF
  180    CONTINUE
      ENDIF

C     Get current year and age.

      NYEAR=IY(ICYC+1)
      NAGE=IAGE+NYEAR-IY(1)

C     Get mistletoe mortality rates.

      CALL MISMRT(.FALSE.)

C     Do stand related calculations.

      DO ITREE = 1,ITRN
         ISPC=ISP(ITREE)

C        Add up total, infected, and mortality TPA and sum DM ratings
C        for: each DBH class (variables begin with 'DC') and each
C        species/DBH class (variables begin with 'SD').
C        Classification into DBH classes made consistent with
C        other models; 0-2.9, 3.0-4.9, etc.

         IVAL = IFIX(DBH(ITREE)*INtoCM)
         IDBH = IVAL / 5 + MOD(IVAL,5)
         IDBH = MAX(1,MIN(20,IDBH))
         P    = PROB(ITREE)
         IDMR = IMIST(ITREE)

         DCTPA(IDBH) = DCTPA(IDBH) + P

         IF (DBH(ITREE) .GE. DMRMIN) DCTPAX(IDBH) = DCTPAX(IDBH) + P

         DCMRT(IDBH) = DCMRT(IDBH) + DMMTPA(ITREE)
         SDTPA(ISPC,IDBH) = SDTPA(ISPC,IDBH) + P
         SDMRT(ISPC,IDBH) = SDMRT(ISPC,IDBH) + DMMTPA(ITREE)

         IF (IDMR .GT. 0 .AND. DBH(ITREE) .GE. DMRMIN) THEN
            DCINF(IDBH) = DCINF(IDBH) + P
            DCSUM(IDBH) = DCSUM(IDBH) + IDMR * P
            SDINF(ISPC,IDBH) = SDINF(ISPC,IDBH) + P
            SDSUM(ISPC,IDBH) = SDSUM(ISPC,IDBH) + IDMR * P

            IF (IDMR .EQ. 1 .OR. IDMR .EQ. 2)
     &         SDIN12(ISPC,IDBH) = SDIN12(ISPC,IDBH) + P
            IF (IDMR .EQ. 3 .OR. IDMR .EQ. 4)
     &         SDIN34(ISPC,IDBH) = SDIN34(ISPC,IDBH) + P
            IF (IDMR .EQ. 5 .OR. IDMR .EQ. 6)
     &         SDIN56(ISPC,IDBH) = SDIN56(ISPC,IDBH) + P
         ELSE
            SDIN0(ISPC,IDBH) = SDIN0(ISPC,IDBH) + P
         ENDIF

C        Calculate stand volumes, BAs, and TPAs at beginning of
C        cycle, infected with DM, and mortality due to DM.

         STVOL = STVOL + CFV(ITREE) * P
         IF (IMIST(ITREE) .GT. 0 .AND. DBH(ITREE) .GE. DMRMIN) THEN
            STVOLI = STVOLI + CFV(ITREE) * P
         ENDIF

         IF (DMMTPA(ITREE) .GT. 0.0) THEN
            STTPAM = STTPAM + DMMTPA(ITREE)
            STVOLM = STVOLM + CFV(ITREE) * DMMTPA(ITREE)
         ENDIF
      ENDDO

      IF (STVOL .NE. 0) THEN
         STBAI = BA / STVOL * STVOLI
         STBAM = BA / STVOL * STVOLM
      ENDIF

C     Lump the largest 10 DBH classes into the tenth class
C     because the (file 3) table only prints the first 10.

      DO IDCLAS = 11,20
         DCTPA(10)  = DCTPA(10)  + DCTPA(IDCLAS)
         DCTPAX(10) = DCTPAX(10) + DCTPAX(IDCLAS)
         DCINF(10)  = DCINF(10)  + DCINF(IDCLAS)
         DCMRT(10)  = DCMRT(10)  + DCMRT(IDCLAS)
         DCSUM(10)  = DCSUM(10)  + DCSUM(IDCLAS)
      ENDDO

C     Calculate average mistletoe ratings by 2 inch DBH classes
C     using all trees and infected-only trees.

      DO IDCLAS = 1,10
         IF (DCTPAX(IDCLAS) .NE. 0)
     &      DCDMR(IDCLAS) = DCSUM(IDCLAS) / DCTPAX(IDCLAS)
         IF(DCINF(IDCLAS) .NE. 0)
     &      DCDMI(IDCLAS) = DCSUM(IDCLAS) / DCINF(IDCLAS)
      ENDDO

C     Calculate average mistletoe ratings by species/DBH classes and
C     total infected by DMR class for each species using all trees
C     and infected-only trees.

      DO ISPC = 1,MAXSP
         DO IDCLAS = 1,20
            IF (SDTPA(ISPC,IDCLAS) .NE. 0) SDDMR(ISPC,IDCLAS) =
     &         SDSUM(ISPC,IDCLAS) / SDTPA(ISPC,IDCLAS)
            IF (SDINF(ISPC,IDCLAS) .NE. 0) SDDMI(ISPC,IDCLAS) =
     &         SDSUM(ISPC,IDCLAS) / SDINF(ISPC,IDCLAS)
            SDTT0(ISPC) = SDTT0(ISPC) + SDIN0(ISPC,IDCLAS)
            SDTT12(ISPC) = SDTT12(ISPC) + SDIN12(ISPC,IDCLAS)
            SDTT34(ISPC) = SDTT34(ISPC) + SDIN34(ISPC,IDCLAS)
            SDTT56(ISPC) = SDTT56(ISPC) + SDIN56(ISPC,IDCLAS)
         ENDDO
      ENDDO

C     Calculate stand average mistletoe ratings and infection levels
C     using all trees and infected-only trees.

      IF (STTPAX .NE. 0) THEN
         STDMR = STDMRS / STTPAX
         STPIT = STTPAI / STTPAT * 100.0
         STPMT = STTPAM / STTPAT * 100.0
      ENDIF

      IF (STTPAI .NE. 0) STDMI = STDMRS / STTPAI
      IF (STVOL .NE. 0) THEN
         STPIV = STVOLI / STVOL * 100.0
         STPMV = STVOLM / STVOL * 100.0
      ENDIF

C     Do species specific calculations.

      DO 265 ISPC = 1,MAXSP

C        Calculate species average mistletoe ratings and infection
C        levels using all trees and infected-only trees.

         IF (SPTPAX(ISPC) .NE. 0) SPDMR(ISPC)=SPDMRS(ISPC)/SPTPAX(ISPC)
         IF (SPTPAI(ISPC) .NE. 0) SPDMI(ISPC)=SPDMRS(ISPC)/SPTPAI(ISPC)

C        Add up the total TPA of this species that are infected by
C        or died due to DM and calculate mortality levels.

         I1 = ISCT(ISPC,1)
         IF (I1 .EQ. 0) GOTO 265
         I2 = ISCT(ISPC,2)

         DO I3 = I1,I2
            I = IND1(I3)
            SPTPAM(ISPC) = SPTPAM(ISPC) + DMMTPA(I)
         ENDDO
  265 CONTINUE

C     Pick off top 4 most infected species statistics for species
C     summary table (top 4 saved in ISVSP4).

      DO 270 INFSPC = 1,4

C        Get a current species number (i.e. between 1 & 11; INFNO=0
C        implies there are less than 4 infected species in the stand)

         INFNO = ISVSP4(INFSPC)
         IF (INFNO .EQ. 0) GOTO 270

         SPDMR4(INFSPC) = SPDMR(INFNO)
         SPDMI4(INFSPC) = SPDMI(INFNO)
         SPINF4(INFSPC) = SPTPAI(INFNO)
         SPMRT4(INFSPC) = SPTPAM(INFNO)

         IF (SPTPAT(INFNO) .NE. 0.0) THEN
            SPPIN4(INFSPC) = SPTPAI(INFNO) / SPTPAT(INFNO) * 100.0
            SPPMR4(INFSPC) = SPTPAM(INFNO) / SPTPAT(INFNO) * 100.0
         ENDIF
         IF (STTPAT .NE. 0) SPPOC4(INFSPC) = SPTPAT(INFNO) / STTPAT *
     &                                       100.0
  270 CONTINUE

C       TRANSLATE TOP 4 INFECTED SPECIES NUMBERS TO CHAR. STRINGS.

      IF (FSTMIS) THEN
        DO I = 1,4
          INFNO = ISVSP4(I)
          CSP(I) = '**'
          IF(INFNO.NE.0) CSP(I) = CSPARR(INFNO)
        ENDDO
      ENDIF

      DO I = 1,4
        SPINF4_M(I) = SPINF4(I)/ACRtoHA
        SPMRT4_M(I) = SPMRT4(I)/ACRtoHA
      ENDDO
      DO I = 1,10
        DCTPA_M(I) = DCTPA(I)/ACRtoHA
        DCINF_M(I) = DCINF(I)/ACRtoHA
        DCMRT_M(I) = DCMRT(I)/ACRtoHA
      ENDDO

C     CALL THE DBS MODULE TO OUTPUT DMDATA TO A DATABASE

      IDBSKODE = 1
      CALL DBSMIS1(NYEAR,NPLT,CSP,
     &  SPDMR4,SPDMI4,SPINF4_M,SPMRT4_M,SPPIN4,SPPMR4,SPPOC4,
     &  IDBSKODE)

      CALL DBSMIS2(NYEAR,NPLT,NAGE,
     &  NINT(STTPAT/ACRtoHA),NINT(BA*FT2pACRtoM2pHA),
     &  NINT(STVOL*FT3pACRtoM3pHA),NINT(STTPAI/ACRtoHA),
     &  NINT(STBAI*FT2pACRtoM2pHA),NINT(STVOLI*FT3pACRtoM3pHA),
     &  NINT(STTPAM/ACRtoHA),NINT(STBAM*FT2pACRtoM2pHA),
     &  NINT(STVOLM*FT3pACRtoM3pHA),NINT(STPIT),
     &  NINT(STPIV),NINT(STPMT),NINT(STPMV),STDMR,STDMI,
     &  IDBSKODE)

      IF (PRTMIS) THEN
        CALL DBSMIS3(NYEAR,NPLT,NLABS,
     &    DCTPA_M,DCINF_M,DCMRT_M,DCDMR,DCDMI,
     &    IDBSKODE)
      ENDIF

C     BRANCH AROUND MAIN OUTPUT IF THAT OUTPUT STREAM IS SILENCED

      IF(IDBSKODE.EQ.0) GOTO 280

C     Check for first time through this routine for opening summary
C     table files and printing summary table headings.
C     Summary tables 1 and 2 are generated by default. Table 3 is
C     generated when the MISTPRT keyword is used (PRTMIS = true).

      IF (FSTMIS) THEN
        FSTMIS=.FALSE.
	  CALL GETID(IDMSOUT(1))
	  CALL GETID(IDMSOUT(2))

        CALL GETLUN(IMOUT1)
        CALL GETLUN(IMOUT2)

        WRITE(IMOUT1,'(2(/1X,I5))') IDMSOUT(1),IDMSOUT(1)
        WRITE(IMOUT1,610) IDMSOUT(1)
        WRITE(IMOUT1,600) IDMSOUT(1)
        WRITE(IMOUT1,605) IDMSOUT(1)
        WRITE(IMOUT1,610) IDMSOUT(1)
        WRITE(IMOUT1,615) IDMSOUT(1)
        WRITE(IMOUT1,620) IDMSOUT(1),NPLT,MGMID
        WRITE(IMOUT1,615) IDMSOUT(1)
        WRITE(IMOUT1,720) IDMSOUT(1),DMRMIN*INtoCM
        WRITE(IMOUT1,625) IDMSOUT(1)
        WRITE(IMOUT1,630) IDMSOUT(1)
        WRITE(IMOUT1,615) IDMSOUT(1)
        WRITE(IMOUT1,635) IDMSOUT(1)
        WRITE(IMOUT1,640) IDMSOUT(1)
        WRITE(IMOUT1,645) IDMSOUT(1)

        WRITE(IMOUT1,650) IDMSOUT(1),((CSP(I),I=1,4),J=1,7)
        WRITE(IMOUT1,655) IDMSOUT(1)

        WRITE(IMOUT2,'(2(/1X,I5))') IDMSOUT(2),IDMSOUT(2)
        WRITE(IMOUT2,610) IDMSOUT(2)
        WRITE(IMOUT2,700) IDMSOUT(2)
        WRITE(IMOUT2,705) IDMSOUT(2)
        WRITE(IMOUT2,710) IDMSOUT(2)
        WRITE(IMOUT2,615) IDMSOUT(2)
        WRITE(IMOUT2,620) IDMSOUT(2),NPLT,MGMID
        WRITE(IMOUT2,615) IDMSOUT(2)
        WRITE(IMOUT2,720) IDMSOUT(2),DMRMIN*INtoCM
        WRITE(IMOUT2,725) IDMSOUT(2)
        WRITE(IMOUT2,730) IDMSOUT(2)
        WRITE(IMOUT2,615) IDMSOUT(2)
        WRITE(IMOUT2,735) IDMSOUT(2)
        WRITE(IMOUT2,740) IDMSOUT(2)
        WRITE(IMOUT2,745) IDMSOUT(2)
        WRITE(IMOUT2,750) IDMSOUT(2)
        WRITE(IMOUT2,755) IDMSOUT(2)

        IF (PRTMIS) THEN

	    CALL GETID(IDMSOUT(3))
          CALL GETLUN(IMOUT3)

          WRITE(IMOUT3,'(2(/1X,I5))') IDMSOUT(3),IDMSOUT(3)
          WRITE(IMOUT3,610) IDMSOUT(3)
          WRITE(IMOUT3,800) IDMSOUT(3)
          WRITE(IMOUT3,805) IDMSOUT(3)
          WRITE(IMOUT3,810) IDMSOUT(3)
          WRITE(IMOUT3,615) IDMSOUT(3)
          WRITE(IMOUT3,620) IDMSOUT(3),NPLT,MGMID
          WRITE(IMOUT3,615) IDMSOUT(3)
          WRITE(IMOUT3,720) IDMSOUT(3),DMRMIN*INtoCM
          WRITE(IMOUT3,825) IDMSOUT(3)
          WRITE(IMOUT3,830) IDMSOUT(3)
          WRITE(IMOUT3,615) IDMSOUT(3)
          WRITE(IMOUT3,835) IDMSOUT(3)
          WRITE(IMOUT3,840) IDMSOUT(3)
          WRITE(IMOUT3,845) IDMSOUT(3)
          WRITE(IMOUT3,850) IDMSOUT(3)
        ENDIF
      ENDIF

      WRITE(IMOUT1,660) IDMSOUT(1),NYEAR,
     &  (SPDMR4(I),I=1,4),(SPDMI4(J),J=1,4),
     &  (NINT(SPINF4(K)/ACRtoHA),K=1,4),
     &  (NINT(SPMRT4(L)/ACRtoHA),L=1,4),
     &  (NINT(SPPIN4(M)),M=1,4),(NINT(SPPMR4(N)),N=1,4),
     &  (NINT(SPPOC4(II)),II=1,4)

      WRITE(IMOUT2,760) IDMSOUT(2),NYEAR,NAGE,
     &  NINT(STTPAT/ACRtoHA),
     &  NINT(BA*FT2pACRtoM2pHA),NINT(STVOL*FT3pACRtoM3pHA),
     &  NINT(STTPAI/ACRtoHA),
     &  NINT(STBAI*FT2pACRtoM2pHA),NINT(STVOLI*FT3pACRtoM3pHA),
     &  NINT(STTPAM/ACRtoHA),
     &  NINT(STBAM*FT2pACRtoM2pHA),NINT(STVOLM*FT3pACRtoM3pHA),
     &  NINT(STPIT),
     &  NINT(STPIV),NINT(STPMT),NINT(STPMV),STDMR,STDMI

      IF (PRTMIS) THEN
        WRITE(IMOUT3,855) IDMSOUT(3),NYEAR
        WRITE(IMOUT3,860) IDMSOUT(3),NLABS(1),
     -    ((DCTPA(I)/ACRtoHA),I=1,10)
        WRITE(IMOUT3,860) IDMSOUT(3),NLABS(2),
     -    ((DCINF(I)/ACRtoHA),I=1,10)
        WRITE(IMOUT3,860) IDMSOUT(3),NLABS(3),
     -    ((DCMRT(I)/ACRtoHA),I=1,10)
        WRITE(IMOUT3,861) IDMSOUT(3),NLABS(4),(DCDMR(I),I=1,10)
        WRITE(IMOUT3,861) IDMSOUT(3),NLABS(5),(DCDMI(j),j=1,10)
      ENDIF

C     CHECK FOR PRINTING THE DETAILED OUTPUT TABLES.

  280 IF (PRTTBL) THEN

C       CHECK FOR FIRST TIME HERE FOR OPENING DETAIL TABLE FILE
C       AND PRINTING DETAIL TABLE HEADINGS.

        IF (FSTTBL) THEN

          FSTTBL=.FALSE.
	    CALL GETID(IDMSOUT(4))
          CALL GETLUN(IMOUT4)

          WRITE(IMOUT4,'(2(/1X,I5))') IDMSOUT(4),IDMSOUT(4)
          WRITE(IMOUT4,610) IDMSOUT(4)
          WRITE(IMOUT4,900) IDMSOUT(4)
          WRITE(IMOUT4,905) IDMSOUT(4)
          WRITE(IMOUT4,910) IDMSOUT(4)
          WRITE(IMOUT4,615) IDMSOUT(4)
          WRITE(IMOUT4,620) IDMSOUT(4),NPLT,MGMID
          WRITE(IMOUT4,615) IDMSOUT(4)
          WRITE(IMOUT4,720) IDMSOUT(4),DMRMIN*INtoCM
          WRITE(IMOUT4,925) IDMSOUT(4)
          WRITE(IMOUT4,930) IDMSOUT(4)
          WRITE(IMOUT4,615) IDMSOUT(4)
        ENDIF

        DO ISPC = 1,MAXSP

C         CHECK IF THIS PARTICULAR SPECIES WAS REQUESTED AND
C         THE SPECIES HAS MISTLETOE INFECTIONS.

          IF (MISTBL(ISPC).AND. SPTPAI(ISPC) .GT. 0.0) THEN

            WRITE(IMOUT4,935) IDMSOUT(4),NYEAR
            WRITE(IMOUT4,940) IDMSOUT(4),CSPARR(ISPC)

C           WRITE SUBHEADINGS.

            WRITE(IMOUT4,615) IDMSOUT(4)
            WRITE(IMOUT4,945) IDMSOUT(4)
            WRITE(IMOUT4,950) IDMSOUT(4)
            WRITE(IMOUT4,955) IDMSOUT(4)

C           WRITE DATA FOR EACH DBH CLASS FOR THIS SPECIES.

            DO IDCLAS = 1,20
              IF (SDINF(ISPC,IDCLAS) .LT. 0.05) THEN
                SDDMR(ISPC,IDCLAS) = 0.0
                SDDMI(ISPC,IDCLAS) = 0.0
              ENDIF

              WRITE(IMOUT4,CFMT(IDCLAS)) IDMSOUT(4),
     &          SDTPA(ISPC,IDCLAS)/ACRtoHA,
     &          SDINF(ISPC,IDCLAS)/ACRtoHA,
     &          SDIN0(ISPC,IDCLAS)/ACRtoHA,
     &          SDIN12(ISPC,IDCLAS)/ACRtoHA,
     &          SDIN34(ISPC,IDCLAS)/ACRtoHA,
     &          SDIN56(ISPC,IDCLAS)/ACRtoHA,
     &          SDMRT(ISPC,IDCLAS)/ACRtoHA,
     &          SDDMR(ISPC,IDCLAS),SDDMI(ISPC,IDCLAS)
            ENDDO

C           WRITE COLUMN TOTALS AND AVERAGES.

            WRITE(IMOUT4,955) IDMSOUT(4)
            WRITE(IMOUT4,960) IDMSOUT(4),SPTPAT(ISPC)/ACRtoHA,
     &        SPTPAI(ISPC)/ACRtoHA,SDTT0(ISPC)/ACRtoHA,
     &        SDTT12(ISPC)/ACRtoHA,SDTT34(ISPC)/ACRtoHA,
     &        SDTT56(ISPC)/ACRtoHA,SPTPAM(ISPC)/ACRtoHA,
     &        SPDMR(ISPC),SPDMI(ISPC)
            WRITE(IMOUT4,615) IDMSOUT(4)
          ENDIF
        ENDDO
      ENDIF

C     FORMAT STATEMENTS USED BY ALL FOUR OUTPUT STREAMS

  615 FORMAT(1X,I5,' ')
  620 FORMAT(1X,I5,' ','Stand ID: ',A26,3X,'Management Code: ',A4,3X,
     &    'Revision code: 991201')
  720 FORMAT(1X,I5,' ','DMRs and DMIs calculated for trees with DBH >=',
     &    F4.1)

C     FORMAT STATEMENTS FOR SPECIES STATISTICS TABLE

  600 FORMAT(1X,I5,'1',30X,'DWARF MISTLETOE INFECTION AND MORTALITY ',
     &   'STATISTICS SUMMARY TABLE')
  605 FORMAT(1X,I5,' ',41X,'(BY SPECIES; TOP 4 MOST INFECTED SPECIES)')
  610 FORMAT(1X,I5,' ',124('-'))
  625 FORMAT(1X,I5,' ','MEAN DMR = Average dwarf mistletoe rating for',
     &   ' all trees of that species.')
  630 FORMAT(1X,I5,' ','MEAN DMI = Average dwarf mistletoe rating for',
     &   ' infected-only trees of that species.')
  635 FORMAT(1X,I5,' ',41X,'TPH INFECTED',7X,'TPH MORTALITY',9X,
     &   '% TPH',11X,'% TPH',8X,'COMPOSITION')
  640 FORMAT(1X,I5,' ','YEAR',4X,'MEAN DMR',8X,'MEAN DMI',11X,
     &   'WITH DM',13X,'FROM DM',11X,'INFECTED',7X,'MORTALITY',4X,
     &   'OF STAND (%TPH)')
  645 FORMAT(1X,I5,' ','----',1X,2(15('-'),1X),2(19('-'),1X),
     &   2(15('-'),1X),15('-'))
  650 FORMAT(1X,I5,' ',4X,9(2X,A),7(3X,A),1X,12(2X,A))
  655 FORMAT(1X,I5,' ',4X,8(1X,3('-')),8(1X,4('-')),12(1X,3('-')))
  660 FORMAT(1X,I5,' ',I4,8(1X,F3.1),8(1X,I4),12(1X,I3))

C     FORMAT STATEMENTS FOR STAND STATISTICS TABLE

  700 FORMAT(1X,I5,'1',24X,'DWARF MISTLETOE INFECTION AND MORTALITY ',
     &   'STATISTICS SUMMARY TABLE')
  705 FORMAT(1X,I5,' ',42X,'(STAND COMPOSITE; PER HA)')
  710 FORMAT(1X,I5,' ',112('-'))
  725 FORMAT(1X,I5,' ','MEAN DMR = Average dwarf mistletoe rating for',
     &   '  all trees in the stand.',14X,
     &   '* --WARNING-- Calculation may')
  730 FORMAT(1X,I5,' ','MEAN DMI = Average dwarf mistletoe rating for',
     &   ' infected-only trees in the stand.',9X,
     &   'include nonhost species.')
  735 FORMAT(1X,I5,' ',15X,'START OF CYCLE',8X,'DM INFECTION',9X,'DM ',
     &   'MORTALITY')
  740 FORMAT(1X,I5,' ',10X,3(3X,18('-')),31X,'*')
  745 FORMAT(1X,I5,' ',9X,3(4X,'TREES',3X,'BA',4X,'VOL'),4X,2('%TPH',2X,
     &   '%VOL',3X),'MEAN',2X,'MEAN')
  750 FORMAT(1X,I5,' ','YEAR',3X,'AGE',3X,3(' /HA ',2X,'SQ M',2X,
     &   'CU  M',3X),1X,2('INF',3X),2('MORT',2X),2X,'DMR',3X,'DMI')
  755 FORMAT(1X,I5,' ','----',3X,'---',3X,3('-----',2X,'----',2X,
     &   '-----',3X),3('----',2X,'----',3X))
  760 FORMAT(1X,I5,' ',I4,3X,I3,3(3X,I5,2X,I4,2X,I5),2(3X,I4,2X,I4),
     &   4X,F3.1,3X,F3.1)

C     FORMAT STATEMENTS FOR 5CM DBH CLASS STATISTICS TABLE

  800 FORMAT(1X,I5,'1',28X,'DWARF MISTLETOE INFECTION AND MORTALITY ',
     &   'STATISTICS SUMMARY TABLE')
  805 FORMAT(1X,I5,' ',46X,'(BY 5 CM DIAMETER CLASSES)')
  810 FORMAT(1X,I5,' ',120('-'))
  825 FORMAT(1X,I5,' ','MEAN DMR = Average dwarf mistletoe rating',
     &   ' for all trees of that diameter class.')
  830 FORMAT(1X,I5,' ','MEAN DMI = Average dwarf mistletoe rating for',
     &   ' infected-only trees of that diameter class.')
  835 FORMAT(1X,I5,' ','YEAR',51X,'5 CM DIAMETER CLASSES')
  840 FORMAT(1X,I5,' ','----',9X,107('-'))
  845 FORMAT(1X,I5,' ',14X,'  0-5cm',4X,' 5-10cm',4X,'10-15cm',4X,
     &   '15-20cm',4X,'20-25cm',4X,'25-30cm',4X,'30-35cm',4X,
     &   '35-40cm',4X,'40-45cm',4X,' 45+ cm')
  850 FORMAT(1X,I5,' ',13X,10(8('-'),3X))
  855 FORMAT(1X,I5,' ',I4)
  860 FORMAT(1X,I5,' ',7X,A3,10(4X,F7.1))
  861 FORMAT(1X,I5,' ',7X,A3,10(8X,F3.1))

C     FORMAT STATEMENTS FOR SPECIES/DBH CLASS STATISTICS TABLE

  900 FORMAT(1X,I5,'1',25X,'DWARF MISTLETOE INFECTION AND MORTALITY ',
     &   'STATISTICS DETAIL TABLE')
  905 FORMAT(1X,I5,' ',32X,'(BY SPECIES, BY 5 CM DIAMETER CLASS,'
     &   ' BY CYCLE)')
  910 FORMAT(1X,I5,' ',114('-'))
  925 FORMAT(1X,I5,' ','MEAN DMR = Average DM rating for all trees of',
     &   ' that species/DBH class.')
  930 FORMAT(1X,I5,' ','MEAN DMI = Average DM rating for infected-only',
     &   ' trees of that species/DBH class.')
  935 FORMAT(1X,I5,' ','Year:    ',I4)
  940 FORMAT(1X,I5,' ','Species:   ',A2)
  945 FORMAT(1X,I5,' ',8X,7(5X,'TREES/HA'),1X,2(3X,'MEAN'))
  950 FORMAT(1X,I5,' ','DBH CLASS',6X,'TOTAL',6X,'INFECTED',4X,
     &   'UNINFECTED',5X,'DMR 1-2',6X,'DMR 3-4',6X,'DMR 5-6',5X,
     &   'MORTALITY',4X,'DMR',4X,'DMI')
  955 FORMAT(1X,I5,' ',9('-'),7(3X,10('-')),2(3X,4('-')))
  960 FORMAT(1X,I5,' ',' SUM/AVG ',7(6X,F7.1),2(4X,F3.1))

C     COMMON RETURN.

 9000 CONTINUE

      IF(DEBUG) WRITE(JOSTND,9010)ICYC
 9010 FORMAT(' End MISPRT: Cycle = ',I5)

      RETURN
      END
