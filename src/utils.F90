module utils
use const, only: dp
implicit none
contains
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
end module utils
