module init_mod
use const, only: dp
implicit none
private
save
public init
contains
   subroutine init()
      use MOD_GLOBAL
      use const, only: dp
      use grid, only: calc_schwerpunkte, str2unstr
      implicit none
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !
      !                             LOCALE VARIABLEN
      !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      INTEGER :: NUMBER_OF_FACES,B,F
      CHARACTER(LEN = 1) , PARAMETER :: FACES(6) = (/"W","E","S","N","B","F"/)




      IF (GLOBAL % AXSYM == 2) THEN
         NUMBER_OF_FACES = 6
      ELSE
         NUMBER_OF_FACES = 4
      END IF

      WRITE(*,'(4(A5,X),6(A2,X))') "BLOCK","NCI","NCJ","NCK",FACES(1:NUMBER_OF_FACES)
      DO B = 1,GLOBAL % NBLOCK  ! LOOP OVER ALL BLOCKS
         WRITE(*,'(4(I5,X))',ADVANCE="NO") B, BLOCKS(B) % NCI, BLOCKS(B) % NCJ, BLOCKS(B) % NCK
         DO F = 1, NUMBER_OF_FACES
            IF (BLOCKS(B) % BLOCK_CONNECTION(F,1) > 0) THEN
               WRITE(*,'(I2,X)',ADVANCE="NO") BLOCKS(B) % BLOCK_CONNECTION(F,1)
            ELSE IF (BLOCKS(B) % BLOCK_CONNECTION(F,1) == 0) THEN
               WRITE(*,'(A2,X)',ADVANCE="NO") "W"
            ELSE IF (BLOCKS(B) % BLOCK_CONNECTION(F,1) == -1) THEN
               WRITE(*,'(A2,X)',ADVANCE="NO") "S"
            ELSE IF (BLOCKS(B) % BLOCK_CONNECTION(F,1) == -2) THEN
               WRITE(*,'(A2,X)',ADVANCE="NO") "I"
            ELSE IF (BLOCKS(B) % BLOCK_CONNECTION(F,1) == -3) THEN
               WRITE(*,'(A2,X)',ADVANCE="NO") "O"
            ELSE IF (BLOCKS(B) % BLOCK_CONNECTION(F,1) == -4) THEN
               WRITE(*,'(A2,X)',ADVANCE="NO") "NS"
            ELSE
               WRITE(*,'(A2,X)',ADVANCE="NO") "-"
            END IF
         END DO
         WRITE(*,*)
      END DO


      CALL CALC_SCHWERPUNKTE()

      CALL STR2UNSTR()

      IF (GLOBAL % DO_WALL_REFINEMENT == 1) THEN
         CALL INIT_BOUNDARY() !INITIALISIERUNG DER BOUNDARY KNOTEN
      END IF

   end subroutine init

   SUBROUTINE INIT_BOUNDARY()
   ! Diese Routine erstell eine Liste der Knoten die von der Wandverfeinerung betroffen sind
      use MOD_GLOBAL
      use BOUNDARY, only: KNT_BOUNDARY, NKNT_BOUNDARY
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
      NKNT_BOUNDARY = nkaw

      allocate(KNT_BOUNDARY(NKNT_BOUNDARY))
      do i = 1,NKNT_BOUNDARY
         KNT_BOUNDARY(i) = KAW(i)
      end do
!      write(*,*) nkaw

   END SUBROUTINE INIT_BOUNDARY
end module init_mod
