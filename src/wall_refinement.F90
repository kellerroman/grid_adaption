module wall_refinement
use const, only: dp
implicit none
private

INTEGER, ALLOCATABLE              :: KNT(:)
INTEGER                           :: NKNT
REAL(KIND = DP)                   :: WALL_DIST
REAL(KIND = DP), ALLOCATABLE      :: KNT_FORCE(:)


public input_wall_refinement
public init_wall_refinement
public calc_wall_refinement
public check_wall_refinement
contains

   SUBROUTINE init_wall_refinement()
   ! Diese Routine erstell eine Liste der Knoten die von der Wandverfeinerung betroffen sind
      use MOD_GLOBAL
      implicit none
      integer, parameter :: KAW_MAX = 100000
      integer :: B , F, i, j, k, i2, j2, k2
      integer :: pkt1
      integer :: pkt2
      integer :: kn
      INTEGER :: NKAW
      INTEGER :: KAW(KAW_MAX)
      !< Number Kanten At Wall
      NKAW = 0
      block_loop: do B = 1, global % NBLOCK
         !! WEST BOUNDARY
         F = 1
         if (BLOCKS(B) % BLOCK_CONNECTION(F,1) == 0) then
            i  = 1
            i2 = 2
            k  = 1
            do j = 1, BLOCKS(B) % NPJ
               do k = 1, BLOCKS(B) % NPK

                  j2 = j
                  k2 = k
                  ! wandpunkt
                  pkt1 = BLOCKS(b) % assoc(i ,j ,k)
                  ! wandnaechster punkt
                  pkt2 = BLOCKS(b) % assoc(i2,j2,k2)
                  ! schleife über alle Kanten die mit PKT1 verbunden sind
                  do kn = 1, UNSTR % PKT_NKNT(pkt1)
                     !WENN KANTE kn von PKT1 mit PKT2 verbunden ist
                     if (pkt2 == UNSTR % PKT_NEIGH(pkt1,kn)) then
                        ! KANTE KN von PKT in LISTE AUFNEHMEN
                        NKAW = NKAW + 1
                        if (NKAW > KAW_MAX) then
                           write(*,'(A)') "ERROR in INIT_BOUNDARY"
                           write(*,'(A,X,I0)') "Number of Edges is larger than the temporary array size:",KAW_MAX
                           stop
                        end if
                        KAW(NKAW) = UNSTR % PKT_KNT(pkt1,kn)
                     end if
                  end do
               end do
            end do
         end if
      end do block_loop

      !ABSPEICHERN DER KANTEN IN DEM ENDGÜLTIGEN ARRAY
      NKNT = nkaw

      allocate(KNT(NKNT))

      allocate(KNT_FORCE(NKNT))

      KNT_FORCE = 1.0D0
      do i = 1,NKNT
         KNT(i) = KAW(i)
      end do
!      write(*,*) nkaw

   END SUBROUTINE init_wall_refinement

   SUBROUTINE input_wall_refinement()
   USE MOD_GLOBAL
   !use boundary, only: wall_dist
   use utils, only: is_integer,lower_case
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
   ZUSTAND = -1000
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
         zustand = 0
      CASE ("sym")
         zustand = -1
      CASE ("inflow")
         zustand = -2
      CASE ("outflow")
         zustand = -3
      CASE ("slipwall")
         zustand = -4
      CASE DEFAULT
         POS = INDEX(LINE,"=")
         IF (POS > 0) THEN   !  VARIABLEN DEFINITION
            VARNAME = lower_case(TRIM(LINE(1:POS-1)))
            VARVALUE = TRIM(ADJUSTL(LINE(POS+1:)))
            SELECT CASE ( TRIM(VARNAME) )
            CASE ("wall_dist")
               READ(VARVALUE,*) WALL_DIST
               WRITE(*,*) wall_dist
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
                        BLOCKS(iBLOCK) % BLOCK_CONNECTION(4,1) = zustand
                        IF (zustand == 0) THEN
                        END IF
                     CASE("s")
                        BLOCK_PHASE = "SOUTH"
                        BLOCKS(iBLOCK) % BLOCK_CONNECTION(3,1) = zustand
                        IF (zustand == 0) THEN
                        END IF
                     CASE("o")
                        BLOCK_PHASE = "EAST"
                        BLOCKS(iBLOCK) % BLOCK_CONNECTION(2,1) = zustand
                        IF (zustand == 0) THEN
                        END IF
                     CASE("w")
                        BLOCK_PHASE = "WEST"
                        BLOCKS(iBLOCK) % BLOCK_CONNECTION(1,1) = zustand
                        IF (zustand == 0) THEN
                        END IF
                     CASE("b")
                        BLOCK_PHASE = "BACK"
                        BLOCKS(iBLOCK) % BLOCK_CONNECTION(5,1) = zustand
                        IF (zustand == 0) THEN
                        END IF
                     CASE("f")
                        BLOCK_PHASE = "FRONT"
                        BLOCKS(iBLOCK) % BLOCK_CONNECTION(6,1) = zustand
                        IF (zustand == 0) THEN
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
   END SUBROUTINE input_wall_refinement


   SUBROUTINE calc_wall_refinement()
      USE MOD_GLOBAL
      IMPLICIT NONE

      integer :: k, i

      REAL(KIND=8) :: TEMP

#ifdef DEBUG
      IF (GLOBAL%DBG >= 1)  THEN
         WRITE(*,'(A)') "WANDVERFEINERUNG"
      END IF
#endif

      IF (GLOBAL % AXSYM == 2) THEN
         WRITE(*,*) "3D NOCH NICHT UNTERSTÜTZT","WANDVERFEINERUNG"
         STOP
      END IF
      do i = 1,NKNT
         k = KNT(i)

         TEMP = UNSTR % KNT_DN(k,1) / wall_dist

         TEMP = MIN(1.1D+0, TEMP)

         TEMP = MAX(5.0D-1, TEMP)

         KNT_FORCE(i) = KNT_FORCE(i) * TEMP

         UNSTR % KNT_SPANNUNG(k,1) = UNSTR % KNT_SPANNUNG(k,1) + KNT_FORCE(i)
      end do
   END SUBROUTINE calc_wall_refinement

   subroutine check_wall_refinement()
   use mod_global, only: unstr
   implicit none
   integer :: i,k
   do i = 1, NKNT
      k = KNT(i)
      write(*,*) UNSTR % KNT_DN(k,1), KNT_FORCE(i)
   end do

   end subroutine check_wall_refinement
end module wall_refinement
