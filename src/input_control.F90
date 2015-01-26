SUBROUTINE INPUT_CONTROL()
USE MOD_GLOBAL
USE CONST
IMPLICIT NONE

CHARACTER(LEN= 1000) :: LINE
CHARACTER(LEN= 50) :: VARNAME,VARVALUE
INTEGER :: ISTAT
INTEGER :: POS
INTEGER :: nArg,i
CHARACTER(LEN = 100) :: PARAM
CHARACTER(LEN=100) :: CONTROL_FILE
INTEGER, PARAMETER :: IO_CF = 20
LOGICAL :: FEXISTS

CONTROL_FILE = "input.cfg"

GLOBAL % GIT_IN             = "git.dat"
GLOBAL % SOL_IN             = "avarout.ufo"
GLOBAL % GIT_OUT            = "git_out.dat"
GLOBAL % SOL_OUT            = "avarout_out.ufo"
GLOBAL % DBG                = 0
GLOBAL % NITER              = 0
GLOBAL % FAKTOR             = 0.5
GLOBAL % FILE_TYPE          = 1
GLOBAL % OUTPUT_TYPE        = 0
GLOBAL % OUTPUT_ANIMATION   = 0
GLOBAL % CHECK_FOR_DOUBLE_POINTS = 1
GLOBAL % CHECK_FOR_DOUBLE_KANTEN = 1
GLOBAL % DO_WALL_REFINEMENT = 0
GLOBAL % RANDBED_IN         = "randbed.dat"
GLOBAL % WALL_DIST          = 1D-6
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                            KOMMANDOZEILE AUSLESEN
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

nArg=COMMAND_ARGUMENT_COUNT()

DO i = 1,nArg
   CALL GET_COMMAND_ARGUMENT(I,PARAM)
   IF (lower_case(PARAM) == "-debug") THEN
      Global%DBG = 1
   ELSE IF (lower_case(PARAM(1:5)) == "-cfg=") THEN
      CONTROL_FILE = PARAM(6:)
   ELSE
      WRITE(*,*) "KOMMANDOZEILEN PARAMTER: ",TRIM(PARAM)
      STOP "KOMMANDO NICHT VERSTANDEN"
   END IF
END DO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                         CONFIGURATIONSDATEI AUSLESEN
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

INQUIRE(FILE=TRIM(CONTROL_FILE),EXIST=FEXISTS)
IF(FEXISTS .EQV. .FALSE.) THEN
   WRITE(*,*) "CONTROL FILE konnte nicht gefunden werden: " // TRIM(CONTROL_FILE)
   STOP
END IF
OPEN(IO_CF,FILE=CONTROL_FILE,STATUS="OLD")

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

   POS = INDEX(LINE,"=")

   IF (POS < 1) THEN
      WRITE(*,'("KONNTE LINE NICHT VERARBEITEN, KEIN = GEFUNDEN:",A)') TRIM(LINE)
      STOP
   END IF
!   WRITE(*,*) TRIM(LINE(1:POS-1)),TRIM(LINE(POS+1:))
   VARNAME = lower_case(TRIM(LINE(1:POS-1)))
   VARVALUE = TRIM(ADJUSTL(LINE(POS+1:)))
   SELECT CASE(TRIM(VARNAME))
   CASE ("debug")
      IF (IS_INTEGER(VARVALUE)) THEN
         IF (Global%DBG == 0) THEN
            READ(VARVALUE,*) Global%DBG
         END IF
      ELSE
         WRITE(*,*) "WERT FÜR DEBUG IS KEIN INTEGER"
         STOP
      END IF
   CASE ("niter")
      IF (IS_INTEGER(VARVALUE)) THEN
         READ(VARVALUE,*) Global%NITER
      ELSE
         WRITE(*,*) "WERT FÜR NITER IS KEIN INTEGER"
         STOP
      END IF
   CASE ("move_faktor")
      IF (IS_REAL(VARVALUE)) THEN
         READ(VARVALUE,*) Global%FAKTOR
      ELSE
         WRITE(*,*) "WERT FÜR MOVE_FAKTOR IS KEIN REAL",VARVALUE
         STOP
      END IF
   CASE ("git_in")
      Global%GIT_IN = VARVALUE
   CASE ("sol_in")
      Global%SOL_IN = VARVALUE
   CASE ("git_out")
      Global%GIT_OUT = VARVALUE
   CASE ("sol_out")
      Global%SOL_OUT = VARVALUE
   CASE ("file_type")
      IF (IS_INTEGER(VARVALUE)) THEN
         READ(VARVALUE,*) Global % FILE_TYPE
      ELSE
         WRITE(*,*) "WERT FÜR FILE_TYPE IS KEIN INTEGER"
         STOP
      END IF
   CASE ("output_type")
      IF (IS_INTEGER(VARVALUE)) THEN
         READ(VARVALUE,*) Global % OUTPUT_TYPE
      ELSE
         WRITE(*,*) "WERT FÜR OUTPUT_TYPE IS KEIN INTEGER"
         STOP
      END IF
   CASE ("output_animation")
      IF (IS_INTEGER(VARVALUE)) THEN
         READ(VARVALUE,*) Global % OUTPUT_ANIMATION
      ELSE
         WRITE(*,*) "WERT FÜR OUTPUT_ANIMATION IS KEIN INTEGER"
         STOP
      END IF
   CASE ("check_for_double_points")
      IF (IS_INTEGER(VARVALUE)) THEN
         READ(VARVALUE,*) Global % CHECK_FOR_DOUBLE_POINTS
      ELSE
         WRITE(*,*) "WERT FÜR CHECK_FOR_DOUBLE_POINTS IS KEIN INTEGER"
         STOP
      END IF
   CASE ("check_for_double_kanten")
      IF (IS_INTEGER(VARVALUE)) THEN
         READ(VARVALUE,*) Global % CHECK_FOR_DOUBLE_KANTEN
      ELSE
         WRITE(*,*) "WERT FÜR CHECK_FOR_DOUBLE_KANTEN IS KEIN INTEGER"
         STOP
      END IF
   CASE ("do_wall_refinement")
      IF (IS_INTEGER(VARVALUE)) THEN
         READ(VARVALUE,*) Global % DO_WALL_REFINEMENT
      ELSE
         WRITE(*,*) "WERT FÜR DO_WALL_REFINEMENT IS KEIN INTEGER"
         STOP
      END IF
   CASE ("randbed_in")
      GLOBAL % RANDBED_IN = VARVALUE
   CASE ("wall_dist")
      IF (IS_REAL(VARVALUE)) THEN
         READ(VARVALUE,*) Global%WALL_DIST
      ELSE
         WRITE(*,*) "WERT FÜR WALL_DIST IS KEIN REAL",VARVALUE
         STOP
      END IF
   CASE DEFAULT
      WRITE(*,'(A)') "LINIE NICHT ERKANNT: "//TRIM(LINE)
      STOP
   END SELECT



END DO
CLOSE(IO_CF)


IF (Global%DBG == 1) THEN
   WRITE(*,"(A)") " ================= DEBUG MODE ==================="
END IF

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
