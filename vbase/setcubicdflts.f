       SUBROUTINE SETCUBICDFLTS
       IMPLICIT NONE

       INCLUDE 'PRGPRM.F77'
       INCLUDE 'PLOT.F77'
       INCLUDE 'CONTRL.F77'
       INCLUDE 'VARCOM.F77'
       INCLUDE 'VOLSTD.F77'
       INCLUDE 'GGCOM.F77'
       INCLUDE 'SNCOM.F77'

       INTEGER AKMERCHCDS(2,17),AKMERCHCAT, I, ISPC, IREGN
       INTEGER CRDEFMT(23)

       DATA CRDEFMT/ 5, 3, 4, 5, 3, 4, 5, 5, 4, 4, 5, 4,
     &            2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2/

C  AKMERCHCDS CONTAINS MERCH SPEC CATEOGORIES (MERCHCAT) FOR EACH 
C  AK LOCATION CODE
       DATA AKMERCHCDS/
     &  713,  1,
     &  720,  2,
     &  7400, 2,
     &  7401, 2,
     &  7402, 2,
     &  7403, 2,
     &  7404, 2,
     &  7405, 2,
     &  7406, 2,
     &  7407, 2,
     &  7408, 2,
     &  703,  3,
     &  1005, 3,
     &  8134, 3,
     &  8135, 3,
     &  8112, 3,
     &  1004, 4/

       SELECT CASE (VARACD)
         CASE('AK')
C----------
C  LOOP THROUGH AKMERCHCDS AND SELECT MERCHCAT BASED ON
C  LOCATION CODE (KODFOR - 17 OPTIONS)
C----------
           DO I=1, 17 
             IF(KODFOR .EQ. AKMERCHCDS(1,I)) THEN 
                AKMERCHCAT= AKMERCHCDS(2,I)
             EXIT
C  DEFAULT TO TONGASS IF LOCATION CODE IS NOT FOUND IN
C  AKMERCHCDS 
             ELSE 
               AKMERCHCAT= 3
             ENDIF
           END DO
C----------
C  DETERMINE DBHMIN, TOPD, SCFMIND AND SCFTOPD BASED ON AKMERCHCAT
C----------
C  SET DBHMIN DEFAULTS
           DO ISPC=1,MAXSP

             STMP(ISPC) = 1
             SCFSTMP(ISPC) = 1

             SELECT CASE(AKMERCHCAT)
               CASE(1)
                 DBHMIN(ISPC) = 6.
                 TOPD(ISPC)   = 4.
                 SCFMIND(ISPC)= 9.
                 SCFTOPD(ISPC)= 6.
               CASE(2)
                 DBHMIN(ISPC) = 5.
                 TOPD(ISPC)   = 4.
                 SCFMIND(ISPC)= 9.
                 SCFTOPD(ISPC)= 6.
               CASE(3)
                 DBHMIN(ISPC) = 9.
                 TOPD(ISPC)   = 7.
                 SCFMIND(ISPC)= 9.
                 SCFTOPD(ISPC)= 7.
               CASE (4)
                 DBHMIN(ISPC) = 9.
                 TOPD(ISPC)   = 6.
                 SCFMIND(ISPC)= 9.
                 SCFTOPD(ISPC)= 6.
               CASE DEFAULT
                 DBHMIN(ISPC) = 9.
                 TOPD(ISPC)   = 7.
                 SCFMIND(ISPC)= 9.
                 SCFTOPD(ISPC)= 7.
             END SELECT
           END DO

         CASE ("BM")
           DO ISPC=1,MAXSP
             STMP(ISPC)    = 1.
             DBHMIN(ISPC)  = 7.0
             TOPD(ISPC)    = 4.5
             SCFSTMP(ISPC) = 1.
             SCFMIND(ISPC) = 7.0
             SCFTOPD(ISPC) = 4.5
           END DO
           DBHMIN(7)       = 6.0
           SCFMIND(7)      = 6.0

         CASE ("CA")
           DO ISPC=1,MAXSP
             STMP(ISPC)    = 1.
             DBHMIN(ISPC)  = 7.0
             SCFSTMP(ISPC) = 1.
             SCFMIND(ISPC) = 7.0
             SELECT CASE (IFOR)
               CASE(6:10)
                 TOPD(ISPC)    = 4.5
                 SCFTOPD(ISPC) = 4.5
               CASE DEFAULT
                 TOPD(ISPC)    = 6
                 SCFTOPD(ISPC) = 6
             END SELECT
           END DO
           DBHMIN(11)       = 6.0
           SCFMIND(11)      = 6.0

         CASE ("CI")
           DO ISPC=1,MAXSP
             STMP(ISPC)    = 1.
             DBHMIN(ISPC)  = 8.0
             TOPD(ISPC)    = 6.0
             SCFSTMP(ISPC) = 1.
             SCFMIND(ISPC) = 8.0
             SCFTOPD(ISPC) = 6.0
           END DO
             DBHMIN(7)  = 7.0
             SCFMIND(7) = 7.0
          
         CASE ("CR")
          IF (IMODTY .LE. 0 .OR. IMODTY .GT. 5) IMODTY=CRDEFMT(IFOR)
           DO ISPC=1,MAXSP
             STMP(ISPC)    = 1.
             SCFSTMP(ISPC) = 1.
             SELECT CASE (IMODTY)
               CASE (1,2,4,5)
                 DBHMIN(ISPC)  = 5.0
                 TOPD(ISPC)    = 4.0
                 SCFTOPD(ISPC) = 6.0
                 SCFMIND(ISPC) = 7.0
                 IF(IFOR .GE. IGFOR) SCFMIND(ISPC) = 9.0
               CASE (3)
                 DBHMIN(ISPC)  = 9.0
                 TOPD(ISPC)    = 6.0
                 SCFMIND(ISPC) = 9.0
                 SCFTOPD(ISPC) = 6.0
               END SELECT
           END DO

         CASE ("CS")
           DO ISPC=1,MAXSP
             STMP(ISPC)    = 0.5
             SCFSTMP(ISPC) = 1.
             DBHMIN(ISPC)  = 6.
             IF(ISPC .LE. 7 .OR. IFOR .EQ. 1) DBHMIN(ISPC) = 5.
             TOPD(ISPC)    = 4.
             IF(ISPC .GT. 7 .AND. IFOR .EQ. 2) TOPD(ISPC) = 5.
             SCFMIND(ISPC) = 9.
             IF(ISPC .EQ. 1 .AND. IFOR .EQ. 1) SCFMIND(ISPC) = 6.
             IF(ISPC .GT. 7 .AND. IFOR .NE. 1) SCFMIND(ISPC) = 11.
             SCFTOPD(ISPC) = 7.6
             IF(ISPC .EQ. 1 .AND. IFOR .EQ. 1) SCFTOPD(ISPC) = 5.
             IF(ISPC .GT. 7 .AND. IFOR .NE. 1) SCFTOPD(ISPC) = 9.6
           END DO

         CASE ("EC")
           DO ISPC=1,MAXSP
             STMP(ISPC)    = 1.
             SCFSTMP(ISPC) = 1.
             DBHMIN(ISPC)  = 7.
             TOPD(ISPC)    = 4.5
             SCFMIND(ISPC) = 7.0
             SCFTOPD(ISPC) = 4.5
           END DO
             DBHMIN(7)  = 6.
             SCFMIND(7) = 6. 

         CASE ("EM")
           DO ISPC=1,MAXSP
             STMP(ISPC)    = 1.
             SCFSTMP(ISPC) = 1.
             DBHMIN(ISPC)  = 7.0
             TOPD(ISPC)    = 4.5
             SCFMIND(ISPC) = 7.0
             SCFTOPD(ISPC) = 4.5
           END DO
             DBHMIN(7)  = 6.
             SCFMIND(7) = 6. 

         CASE ("IE")
           DO ISPC=1,MAXSP
             STMP(ISPC)    = 1.
             SCFSTMP(ISPC) = 1.
             DBHMIN(ISPC)  = 7.0
             TOPD(ISPC)    = 4.5
             SCFMIND(ISPC) = 7.0
             SCFTOPD(ISPC) = 4.5
           END DO
             DBHMIN(7)  = 6.
             SCFMIND(7) = 6. 

         CASE ("KT")
           DO ISPC=1,MAXSP
             STMP(ISPC)    = 1.
             SCFSTMP(ISPC) = 1.
             DBHMIN(ISPC)  = 7.0
             TOPD(ISPC)    = 4.5
             SCFMIND(ISPC) = 7.0
             SCFTOPD(ISPC) = 4.5
           END DO
             DBHMIN(7)  = 6.
             SCFMIND(7) = 6.  

         CASE ("LS")
           DO ISPC=1,MAXSP
             STMP(ISPC)    = 0.5
             SCFSTMP(ISPC) = 1.
             DBHMIN(ISPC)  = 5.
             TOPD(ISPC)    = 4.
             IF((ISPC.GE.40.AND.ISPC.LE.42.AND.IFOR.EQ.2)
     >        .OR.(IFOR.EQ.6.AND.ISPC.GT.14)) DBHMIN(ISPC) = 6.
             IF(ISPC .LE. 14) THEN
               SCFMIND(ISPC) = 9.
               SCFTOPD(ISPC) = 7.6
             ELSE
               SELECT CASE(IFOR)
                 CASE (2)
                   SCFMIND(ISPC) = 9.0
                   SCFTOPD(ISPC) = 7.6
                   IF(ISPC.GE.40.AND.ISPC.LE.42) THEN 
                     SCFMIND(ISPC) = 11.
                     SCFTOPD(ISPC) = 9.6
                   END IF
                 CASE (5) 
                   SCFMIND(ISPC) = 11.
                   SCFTOPD(ISPC) = 7.6
                   IF(ISPC.GE.40.AND.ISPC.LE.42) SCFMIND(ISPC)=9.
                 CASE DEFAULT
                   SCFMIND(ISPC) = 11.
                   SCFTOPD(ISPC) = 9.6
               END SELECT 
             END IF
           END DO

           CASE ("NC")
             DO ISPC=1,MAXSP
               STMP(ISPC)     = 1.
               SCFSTMP(ISPC)  = 1.
               DBHMIN(ISPC)   = 9.0
               SCFMIND(ISPC)  = 9.0
               SELECT CASE (IFOR)
                 CASE (4)
                   TOPD(ISPC)    = 4.5
                   SCFTOPD(ISPC) = 4.5
                 CASE (5,7)
                   TOPD(ISPC)    = 5.
                   SCFTOPD(ISPC) = 5.
                 CASE DEFAULT
                   TOPD(ISPC)    = 6.
                   SCFTOPD(ISPC) = 6.
               END SELECT
             END DO

           CASE ("NE")
             DO ISPC=1,MAXSP
               STMP(ISPC)    = 0.5
               SCFSTMP(ISPC) = 1.   
               DBHMIN(ISPC)  = 5.
               IF((IFOR .EQ. 1 .OR. IFOR .EQ. 3) .AND. ISPC .GT. 25)
     >             DBHMIN(ISPC) = 6.
               IF(IFOR .EQ. 4 .AND. ISPC .GT. 25) DBHMIN(ISPC) = 8.
               TOPD(ISPC)    = 4.
               IF(IFOR .EQ. 3 .AND. ISPC .GT. 25) TOPD(ISPC)   = 5.
               SCFMIND(ISPC) = 9.
               IF(ISPC .GT. 25) SCFMIND(ISPC) = 11.
               SCFTOPD(ISPC) = 7.6
               IF(ISPC .GT. 25) SCFTOPD(ISPC) = 9.6
             END DO 

          CASE ("OC")
            DO ISPC=1,MAXSP
              STMP(ISPC)    = 1.
              SCFSTMP(ISPC) = 1.
              DBHMIN(ISPC)  = 7.
              SCFMIND(ISPC) = 7.
              SELECT CASE (IFOR)
                CASE (6:10)
                  TOPD(ISPC)    = 4.5
                  SCFTOPD(ISPC) = 4.5
                CASE DEFAULT
                  TOPD(ISPC)    = 6.
                  SCFTOPD(ISPC) = 6.
              END SELECT
            END DO
            DBHMIN(11)  = 6.
            SCFMIND(11) = 6.

          CASE ("OP")
            DO ISPC=1,MAXSP
              STMP(ISPC)    = 1.
              SCFSTMP(ISPC) = 1.
              DBHMIN(ISPC)  = 7.
              SCFMIND(ISPC) = 7.
              SELECT CASE (IFOR)
                CASE (4,5,6)
                  TOPD(ISPC)    = 5.
                  SCFTOPD(ISPC) = 5.
                CASE DEFAULT
                  IF(ISPC.EQ.11) THEN
                    DBHMIN(ISPC)  = 6.
                    SCFMIND(ISPC) = 6.
                  END IF 
                  TOPD(ISPC)    = 4.5
                  SCFTOPD(ISPC) = 4.5
              END SELECT
            END DO

          CASE ("PN")
            DO ISPC=1,MAXSP
              STMP(ISPC)    = 1.
              SCFSTMP(ISPC) = 1.
              DBHMIN(ISPC)  = 7.
              SCFMIND(ISPC) = 7.
              SELECT CASE (IFOR)
                CASE (4,5,6)
                  TOPD(ISPC)    = 5.
                  SCFTOPD(ISPC) = 5.
                CASE DEFAULT
                  IF(ISPC.EQ.11) THEN
                    DBHMIN(ISPC)  = 6.
                    SCFMIND(ISPC) = 6.
                  END IF 
                  TOPD(ISPC)    = 4.5
                  SCFTOPD(ISPC) = 4.5
              END SELECT
            END DO

          CASE ("SN")
            IF(ISEFOR.NE.0)THEN
              IREGN = KODFOR/10000
            ELSE
              IREGN=9
            ENDIF

            IF(IREGN .EQ. 8) THEN
              DO ISPC=1,MAXSP
                STMP(ISPC)    = 0.5
                SCFSTMP(ISPC) = 1.0
                ! SET DBHMIN AND TOPD DEFAULTS
                TOPD(ISPC)    = 4.0
                SELECT CASE (ISPC)
                  CASE (7,13,39,43,44,52,53,55,63)
                    DBHMIN(ISPC) = 6.0
                  CASE DEFAULT
                    DBHMIN(ISPC) = 4.0
                END SELECT
                ! SET NORTH CAROLINA DBHMIN AND TOPD DEFAULTS
                IF (IFOR .EQ. 11) THEN
                  TOPD(ISPC) = 3.5
                  SELECT CASE (KODIST)
                    CASE (3,10)
                      IF(ISPC .LE. 17 .OR. ISPC .EQ. 88) THEN
                        DBHMIN(ISPC) = 5.6
                      ELSE
                        DBHMIN(ISPC) = 6.0
                      END IF
                    CASE DEFAULT
                      DBHMIN(ISPC) = 8.0
                  END SELECT
                END IF
                ! SET SCFMIND AND SCFTOD DEFAULTS
                IF(ISPC .LE. 17 .OR. ISPC .EQ. 88) THEN ! SOFTWOODS
                  SCFMIND(ISPC) = 10.0
                  SCFTOPD(ISPC) = 7.0
                  IF(IFOR .EQ. 10 .AND. ISPC .EQ. 2) THEN
                    SCFMIND(ISPC) = 9.0
                    SCFTOPD(ISPC) = 7.0
                  END IF
                  ! SET NORTH CAROLINA SOFTWOOD SPECS
                  IF(IFOR .EQ. 11) THEN
                    IF(KODIST.EQ.3 .OR. KODIST.EQ.10) THEN 
                      SCFMIND(ISPC)  = 11.0
                      SCFTOPD(ISPC)  = 6.3
                    ELSE
                      SELECT CASE (ISPC)
                        CASE (2,12,15,16,17)
                          SCFMIND(ISPC) = 12.0
                          SCFTOPD(ISPC) =  9.0
                        CASE DEFAULT
                          SCFMIND(ISPC) = 10.0
                          SCFTOPD(ISPC) =  6.3
                      END SELECT
                    END IF
                  END IF
                ELSE ! HARDWOODS
                  SCFMIND(ISPC) = 12.0
                  SCFTOPD(ISPC) = 9.0
                  ! SET NORTH CAROLINA HARDWOOD SPECS
                  IF(IFOR .EQ. 11) THEN
                    SCFMIND(ISPC) = 15.0
                    SCFTOPD(ISPC) = 11.0
                    IF (KODIST .EQ. 3 .OR. KODIST .EQ. 10) THEN
                      SCFMIND(ISPC) = 13.0
                      SCFTOPD(ISPC) = 8.0
                    END IF
                  END IF
                END IF
              END DO
C END OF REGION 8 LOGIC
            ELSE
              DO ISPC=1,MAXSP
                STMP(ISPC)    = 0.5
                SCFSTMP(ISPC) = 1.0
                ! SET DBHMIN AND TOPD SPECS
                IF(IFOR.EQ.15 .AND. (ISPC.GT.17 .AND. ISPC.NE.88)) THEN
                  DBHMIN(ISPC) = 6.0
                  TOPD(ISPC)   = 5.0
                ELSE
                  DBHMIN(ISPC) = 5.0
                  TOPD(ISPC)   = 4.0
                END IF
                ! SCFMIND AND SCFTOPD SPECS
                SCFMIND(ISPC) = 9.0
                SCFTOPD(ISPC) = 7.6
                IF(IFOR.EQ.15 .AND. (ISPC.GT.17 .AND. ISPC.NE.88)) THEN
                  SCFMIND(ISPC) = 11.0
                  SCFTOPD(ISPC) = 9.6
                END IF
              END DO
            END IF

          CASE ("SO")
            DO ISPC=1,MAXSP
              STMP(ISPC)    = 1.0
              SCFSTMP(ISPC) = 1.0
              DBHMIN(ISPC)  = 9.0
              SCFMIND(ISPC) = 9.0
              SELECT CASE (IFOR)
                CASE (1:3,10)
                  TOPD(ISPC)    = 4.5
                  SCFTOPD(ISPC) = 4.5
                CASE DEFAULT
                  TOPD(ISPC)    = 6.0
                  SCFTOPD(ISPC) = 6.0
              END SELECT
            END DO

          CASE ("TT")
            DO ISPC=1,MAXSP
              STMP(ISPC)    = 1.0
              SCFSTMP(ISPC) = 1.0
              DBHMIN(ISPC)  = 8.0
              TOPD(ISPC)    = 6.0
              SCFMIND(ISPC) = 8.0
              SCFTOPD(ISPC) = 6.0
            END DO
            DBHMIN(7)       = 7.0
            SCFMIND(7)      = 7.0

          CASE ("UT")
            DO ISPC=1,MAXSP
              STMP(ISPC)    = 1.0
              SCFSTMP(ISPC) = 1.0
              DBHMIN(ISPC)  = 8.0
              TOPD(ISPC)    = 6.0
              SCFMIND(ISPC) = 8.0
              SCFTOPD(ISPC) = 6.0
            END DO
            DBHMIN(7)       = 7.0
            SCFMIND(7)      = 7.0

          CASE ("WC")
            DO ISPC=1,MAXSP
              STMP(ISPC)    = 1.0
              SCFSTMP(ISPC) = 1.0
              SELECT CASE (IFOR)
                CASE (7:10)
                  DBHMIN(ISPC)  = 7.0
                  TOPD(ISPC)    = 5.0
                  SCFMIND(ISPC) = 7.0
                  SCFTOPD(ISPC) = 5.0
                CASE DEFAULT
                  DBHMIN(ISPC)  = 7.0
                  IF(ISPC .EQ. 11) DBHMIN(ISPC) = 6.0
                  TOPD(ISPC)    = 4.5
                  SCFMIND(ISPC) = 7.0
                  IF(ISPC .EQ. 11) SCFMIND(ISPC) = 6.0
                  SCFTOPD(ISPC) = 4.5
              END SELECT
            END DO

          CASE ("WS")
            DO ISPC=1,MAXSP
              STMP(ISPC)    =  1.0
              SCFSTMP(ISPC) =  1.0
              DBHMIN(ISPC)  =  7.0
              TOPD(ISPC)    =  4.5
              SCFMIND(ISPC) = 10.0
              SCFTOPD(ISPC) =  6.0
            END DO

       END SELECT

       RETURN 
       END

