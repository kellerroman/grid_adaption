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
      use wall_refinement, only: init_wall_refinement
      use edge_stress, only: init_edge_stretch
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

      CALL INIT_WALL_REFINEMENT() !INITIALISIERUNG DER BOUNDARY KNOTEN

      CALL INIT_EDGE_STRETCH

   end subroutine init

end module init_mod
