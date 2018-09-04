      SUBROUTINE DMNTRD
      IMPLICIT NONE
C----------
C CANADA-NEWMIST $Id$
C----------
C **DMNTRD -- NISI  Date of last revision: April 10 1994 
C----------------------------------------------------------------------
C Purpose:
C  Height and crown may change as each tree record is projected. In
C contrast, DM infections are fixed on the branches and do not move
C within the crown. This means that infection loads must be modified
C so that they accurately represent the relationship between
C nonmoving infections and moving crown third breakponts. In 
C particular, new and uninfected crown is produced at the top, and
C crown death may occur at the lower crown margin. By locating the
C change in breakpoints, the proper proportions of existing infection
C density can be properly reassigned.
C----------------------------------------------------------------------
C
C Called by:
C
C     DMTREG 
C
C Other routines called:
C
C     DMRANN
C
C Argument list definitions:                        
C
C     [none]
C
C Local variable definitions:
C
C     INTEGER   i         Loop counter for crown thirds.
C     INTEGER   j         Loop counter for crown thirds.
C     INTEGER   k         Crown third in which breakpoint lies
C                          following growth.
C     INTEGER   u         Loop counter for treelist records.
C     INTEGER   v         Loop counter for life history pools
C     REAL      x         Sum of previous breakpoint values.
C     REAL      Wt        Scalar to assign correct proportion 
C                          of infection in old crown third across
C                          breakpoint now lying in that crown third.
C     REAL      Mult      Scalar to adjust infection density as a
C                          function of the change in crown height
C                          over the growth period. See additional 
C                          note in code below.
C     REAL      OldVal    Array of infection loads before growth, for
C                          and individual record. Array is
C                          dimensioned (1:CRTHRD) for each crown
C                          third, and (1:ACTIVE) for each life history
C                          pool.
C     REAL      NewVal    Array of infection loads after growth.
C                          Array is dimensioned like 'OldVal()'.
C
C Common block variables and parameters:
C
C     CRTHRD    DMCOM
C     ACTIVE    DMCOM
C     ITRN      CONTRL
C     BPCNT     DMCOM
C
C**********************************************************************

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'DMCOM.F77'

      INTEGER   i, j, k, L,u, v
      REAL      x, Wt, Mult
      REAL      OldVal(CRTHRD,DEAD_BC)
      REAL      NewVal(CRTHRD,DEAD_BC)
      REAL      OldVal_BC(CRTHRD,ACTIVE,MAXBC)
      REAL      NewVal_BC(CRTHRD,ACTIVE,MAXBC)

C LOOP OVER ALL TREES, REGARDLESS OF SPECIES OR INFECTION STATUS

      DO 100 u = 1,ITRN

C WHEN THE TREELIST IS COMPRESSED, PREVIOUS BREAKPOINTS ARE SET TO
C ZERO. IF THE PREVIOUS BREAKPOINT ARRAY IS ALL ZERO, REALLOCATION OF
C INFECTION IS MEANINGLESS, AND A JUMP TO THE BOTTOM OF THE U-LOOP IS
C REQUIRED TO PREVENT A DIVIDE-BY-ZERO ERROR THAT WOULD OCCUR.

        x = 0.0
        DO i = 1,CRTHRD
          x = x + PBrkPt(u,i)
        ENDDO

        IF (x .EQ. 0.0) GOTO 100

C 'MULT' IS A BIT OF A KLUDGE. ITS INTENT IS TO APPROXIMATE CROWN
C GROWTH, WHICH ENLARGES (MOST) CROWNS, BUT DOES NOT INCREASE THE
C NUMBER OF INFECTIONS. SINCE 'DMINF()' HOLDS A MEASURE OF INFECTION
C DENSITY, IT IS NECESSARY TO SCALE THIS DENSITY DOWN WHEN THE CROWN
C ENLARGES, SO THAT THE ABSOLUTE AMOUNT OF INFECTION WILL REMAIN
C CONSTANT. IT IS A KLUDGE BECAUSE TO DO IT MORE ACCURATELY WOULD
C REQUIRE KEEPING TRACK OF CROWN GEOMETRY DURING TWO TIME PERIODS,
C WHICH WOULD REQUIRE A *LOT* OF MEMORY.

          Mult = (PBrkPt(u,1) - PBrkPt(u,BPCNT)) /
     >            (BrkPnt(u,1) - BrkPnt(u,BPCNT))

          DO i = 1,CRTHRD
            DO v = 1,DEAD_BC
              OldVal(i,v) = DMINF(u,i,v) * Mult
              NewVal(i,v) = 0.0
            ENDDO
            DO v = 1,ACTIVE
              DO L = 1,MAXBC
                OldVal_BC(i,v,L) = DMINF_BC(u,i,v,L) * Mult
                NewVal_BC(i,v,L) = 0.0
              ENDDO
            ENDDO
          ENDDO

C FIND NEW CROWN THIRD 'J' IN WHICH EACH OLD BREAKPOINT 'I' LIES. A
C PREVIOUS CROWNTHIRD BREAKPOINT NO LONGER FOUND IN THE CROWN
C PRODUCES A (K .EQ. 0) CONDITION.

          DO i = 1,CRTHRD
            k = 0
            DO j = 1,CRTHRD
              IF((PBrkPt(u,i) .LE. BrkPnt(u,j))
     &          .AND. (PBrkPt(u,i) .GT. BrkPnt(u,j+1))) THEN
                k = j
                GOTO 450
              END IF
            ENDDO
  450       CONTINUE

C IF ANY CROWN PORTIONS OVERLAP, TRANSFER INFECTION LEVEL. IN THE
C CASE (K .EQ. 0), THE ENTIRE CROWN THIRD HAS OUTGROWN IS PRIOR
C POSITION.

            IF (k .GT. 0) THEN
              IF (PBrkPt(u,i+1) .GE. BrkPnt(u,k+1)) THEN
                DO v = 1,DEAD_BC
                  NewVal(k,v) = NewVal(k,v) + OldVal(i,v)
                ENDDO
                DO v = 1,ACTIVE
	            DO L = 1,MAXBC
                    NewVal_BC(k,v,L) = NewVal_BC(k,v,L)
     >                 + OldVal_BC(i,v,L)
                  ENDDO
                ENDDO
              ELSE
                Wt = (PBrkPt(u,i) - BrkPnt(u,k+1)) /
     >                 (PBrkPt(u,i) - PBrkPt(u,i+1))
                DO v = 1,DEAD_BC
                  NewVal(k,v) = NewVal(k,v) + OldVal(i,v) * Wt
                ENDDO
                DO v = 1,ACTIVE
	            DO L = 1,MAXBC
                    NewVal_BC(k,v,L) = NewVal_BC(k,v,L)
     >                 + OldVal_BC(i,v,L) * Wt
                  ENDDO
                ENDDO
                IF (k .LT. CRTHRD) THEN
                  DO v = 1,DEAD_BC
                    NewVal(k+1,v) = NewVal(k+1,v)
     >                + OldVal(i,v) * (1.0 - Wt)
                  ENDDO
                  DO v = 1,ACTIVE
	              DO L = 1,MAXBC
                      NewVal_BC(k+1,v,L) = NewVal_BC(k+1,v,L)
     >                  + OldVal_BC(i,v,L) * (1.0 - Wt)
                    ENDDO
                  ENDDO
                ENDIF
              ENDIF
            ENDIF
          ENDDO

C PLACE VALUES BACK IN CROWN THIRD CATEGORIES.

        DO i = 1,CRTHRD
          DO v = 1,DEAD_BC
            DMINF(u,i,v) = NewVal(i,v)
          ENDDO
          DO v = 1,ACTIVE
            DO L = 1,MAXBC
              DMINF_BC(u,i,v,L) = NewVal_BC(i,v,L)
            ENDDO
          ENDDO
        ENDDO

  100 CONTINUE

      RETURN
      END
