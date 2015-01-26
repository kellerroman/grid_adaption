SUBROUTINE INPUT_RANDBED()
USE MOD_GLOBAL
USE CONST
IMPLICIT NONE

CHARACTER(LEN= 1000) :: LINE
CHARACTER(LEN= 50) :: VARNAME,VARVALUE,BLOCK_NUM,BLOCK_PHASE
INTEGER :: ISTAT
INTEGER :: POS
INTEGER, PARAMETER :: IO_CF = 20
LOGICAL :: FEXISTS

INTEGER :: ZUSTAND
!< 0: UNdefiniert: Es muss eine
!< 1: WALL DEFINITION

INTEGER :: iBLOCK,iPhase
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                         CONFIGURATIONSDATEI AUSLESEN
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

INQUIRE(FILE=TRIM(GLOBAL % RANDBED_IN),EXIST=FEXISTS)
IF(FEXISTS .EQV. .FALSE.) THEN
   WRITE(*,*) "CONTROL FILE konnte nicht gefunden werden: " // TRIM(GLOBAL % RANDBED_IN)
   STOP
END IF
OPEN(IO_CF,FILE=TRIM(GLOBAL % RANDBED_IN),STATUS="OLD")
ZUSTAND = 0
DO
   READ(IO_CF,'(A1000)',IOSTAT = ISTAT) LINE
   IF (ISTAT < 0 ) THEN
!      WRITE(*,'(A)') "ENDE DER CONTROL-DATEI ERREICHT"
      EXIT
   END IF

   !! KOMMENTARE UND LEERZEILEN ENTFERNEN
   LINE = TRIM(ADJUSTL(LINE))
   POS = INDEX(LINE,"!")
   IF (POS > 0 ) THEN
      LINE = LINE(1:POS-1)
   END IF
   IF (LEN_TRIM(LINE) == 0) CYCLE

   !! RESTLICHEN LINIEN DURCHSUCHEN
   line = lower_case(line)

   SELECT CASE(TRIM(ADJUSTL(LINE)))
   CASE ("wall")
      zustand = 1
   CASE ("inflow")
      zustand = 2
   CASE ("sym")
      zustand = 3
   CASE ("outflow")
      zustand = 4
   CASE ("slipwall")
      zustand = 5
   CASE DEFAULT
      POS = INDEX(LINE,"=")
      IF (POS > 0) THEN   !  VARIABLEN DEFINITION
         VARNAME = lower_case(TRIM(LINE(1:POS-1)))
         VARVALUE = TRIM(ADJUSTL(LINE(POS+1:)))
         SELECT CASE ( TRIM(VARNAME) )
         CASE ("wall_dist")

         CASE DEFAULT
            WRITE(*,*) "VARIABLE NICHT ERkANNT:",TRIM(VARNAME)
            STOP
         END SELECT
      ELSE
         DO
            POS = INDEX(LINE,",")
            IF (POS > 0) THEN
               VARNAME = lower_case(TRIM(LINE(1:POS-1)))
               LINE = TRIM(ADJUSTL(LINE(POS+1:)))
            ELSE
               IF (LEN_TRIM(LINE) == 0) EXIT
               VARNAME = TRIM(LINE)
               LINE = ""
            END IF
            POS = 1
            DO
               IF ( IS_INTEGER(TRIM(VARNAME(1:POS)))) THEN
                  POS = POS + 1
                  IF (LEN_TRIM(VARNAME) < POS) THEN
                     WRITE(*,*) "BLOCK FACE IS NOT DEFINED: ",TRIM(VARNAME)
                     STOP
                  END IF
!                  WRITE(*,*) "IS_INT:",TRIM(VARNAME(1:POS-1))
               ELSE
                  POS = POS - 1
                  BLOCK_NUM = TRIM(ADJUSTL(VARNAME(1:POS)))
                  BLOCK_PHASE = lower_case(TRIM(ADJUSTL(VARNAME(POS+1:))))
                  READ(BLOCK_NUM,*) iBlock
                  SELECT CASE (TRIM(BLOCK_PHASE))
                  CASE("n")
                     BLOCK_PHASE = "NORTH"
                     IF (zustand == 1) THEN
                        BLOCKS(iBLOCK) % BLOCK_CONNECTION(4,1) = 0
                     END IF
                  CASE("s")
                     BLOCK_PHASE = "SOUTH"
                     IF (zustand == 1) THEN
                        BLOCKS(iBLOCK) % BLOCK_CONNECTION(3,1) = 0
                     END IF
                  CASE("o")
                     BLOCK_PHASE = "EAST"
                     IF (zustand == 1) THEN
                        BLOCKS(iBLOCK) % BLOCK_CONNECTION(2,1) = 0
                     END IF
                  CASE("w")
                     BLOCK_PHASE = "WEST"
                     IF (zustand == 1) THEN
                        BLOCKS(iBLOCK) % BLOCK_CONNECTION(1,1) = 0
                     END IF
                  CASE("b")
                     BLOCK_PHASE = "BACK"
                     IF (zustand == 1) THEN
                        BLOCKS(iBLOCK) % BLOCK_CONNECTION(5,1) = 0
                     END IF
                  CASE("f")
                     BLOCK_PHASE = "FRONT"
                     IF (zustand == 1) THEN
                        BLOCKS(iBLOCK) % BLOCK_CONNECTION(6,1) = 0
                     END IF
                  CASE DEFAULT
                     WRITE(*,*) "FACE NICHT ERKANNT:",TRIM(BLOCK_PHASE)
                     STOP
                  END SELECT
!                  WRITE(*,'(X,I4.4,2A)') iBLOCK ," : ", TRIM(BLOCK_PHASE)
                  EXIT
               END IF
            END DO
         END DO

!         WRITE(*,'(A)') "LINIE NICHT ERKANNT: "//TRIM(LINE)
      END IF
   END SELECT

END DO
CLOSE(IO_CF)

CONTAINS
   LOGICAL FUNCTION IS_INTEGER(STRING)
   IMPLICIT NONE
      integer :: IPOS
      CHARACTER (LEN = *) :: STRING
      IS_INTEGER =  .TRUE.
      DO IPOS = 1,LEN_TRIM(STRING)
         IF (ichar(STRING(IPOS:IPOS)) >57 .OR. ICHAR(STRING(IPOS:IPOS)) <48) THEN
!            WRITE(*,*) TRIM(STRING) , "IST KEINE INTEGER-ZAHL"
            IS_INTEGER =  .FALSE.
            RETURN
         END IF
      end DO

   END FUNCTION

   LOGICAL FUNCTION IS_REAL(STRING)
   IMPLICIT NONE
      integer :: IPOS
      INTEGER :: ELEM_POS
      CHARACTER (LEN = *) :: STRING
      IS_REAL =  .TRUE.
      ELEM_POS = 1
      !!!! AUFTEILUNG EINES REALEN ZAHL   - 0 . 0 D + 1
      ! VORZEICHEN                  ELEM_POS = 1
      ! ZAHL VOR DEM KOMMA          ELEM_POS = 2
      ! TRENNZEICHEN                ELEM_POS = 3
      ! ZAHL NACH DEM KOMMA         ELEM_POS = 4
      ! EXPONENT                    ELEM_POS = 5
      ! EXPONENTEN-VORZEICHEN       ELEM_POS = 6
      ! exponent                    ELEM_POS = 7
      DO IPOS = 1,LEN_TRIM(STRING)
         IF (ichar(STRING(IPOS:IPOS)) <=57 .AND. ICHAR(STRING(IPOS:IPOS)) >=48) THEN
!            WRITE(*,*) TRIM(STRING) , "IST KEINE INTEGER-ZAHL"
            SELECT CASE(ELEM_POS)
               CASE (2)
               CASE (4)
               CASE (5)
                  ELEM_POS = 7
               CASE (7)
               CASE DEFAULT
                  ELEM_POS = ELEM_POS + 1
            END SELECT
         ELSE IF (STRING(IPOS:IPOS) == "-" .OR. STRING(IPOS:IPOS) == "+") THEN
            SELECT CASE(ELEM_POS)
               CASE (2:4)
                  IS_REAL = .FALSE.
                  RETURN
               CASE (6:7)
                  IS_REAL = .FALSE.
                  RETURN
               CASE DEFAULT
                  ELEM_POS = ELEM_POS + 1
            END SELECT
         ELSE IF (STRING(IPOS:IPOS) == ".") THEN
            SELECT CASE(ELEM_POS)
               CASE (1:2)
                  ELEM_POS = ELEM_POS + 1
               CASE DEFAULT
                  IS_REAL = .FALSE.
                  RETURN
            END SELECT
         ELSE IF (STRING(IPOS:IPOS) == "D" .OR. STRING(IPOS:IPOS) == "E" .OR.  &
                  STRING(IPOS:IPOS) == "d" .OR. STRING(IPOS:IPOS) == "e") THEN
            SELECT CASE(ELEM_POS)
               CASE (2:4)
                  ELEM_POS = 5
               CASE DEFAULT
                  IS_REAL = .FALSE.
                  RETURN
            END SELECT
         END IF
      end DO
      IF (ELEM_POS <= 2 .OR. &
          ELEM_POS == 5 .OR. &
          ELEM_POS == 6 ) THEN
         IS_REAL = .FALSE.
         RETURN
      END IF
   END FUNCTION
   FUNCTION lower_case( Input_String ) RESULT ( Output_String )
      ! -- Argument and result
      implicit none
      CHARACTER( * ), INTENT( IN )       :: Input_String
      CHARACTER( LEN( Input_String ) )   :: Output_String
      integer                            :: ii,ic,nlen
      nlen = len(Input_String)
      do ii=1,nlen
         ic = ichar(Input_String(ii:ii))
         if (ic >= 65 .and. ic < 90) THEN
            Output_String(ii:ii) = char(ic+32)
         ELSE
            Output_String(ii:ii) = char(ic)
         END IF
      end do
   end function lower_case
END SUBROUTINE
