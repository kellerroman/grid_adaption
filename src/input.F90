SUBROUTINE INPUT(IS_PARALLEL)
USE MOD_GLOBAL
USE CONST
IMPLICIT NONE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                             PARAMETER
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
LOGICAL :: IS_PARALLEL                                                  ! DEFINES IF THE INPUT IS PARALLEL (NUMBERS APPENDED TO FILES)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                             LOCALE VARIABLEN
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INTEGER :: NUMBER_OF_FACES,B,F
CHARACTER(LEN = 1) , PARAMETER :: FACES(6) = (/"W","E","S","N","B","F"/)
#ifdef DEBUG
   IF (GLOBAL%DBG == 1)  THEN
      WRITE(*,*) "======================================================" &
                ,"INPUT START"  &
                ,"======================================================"

   END IF
#endif

   IF (GLOBAL % FILE_TYPE == 0 ) THEN
      CALL INPUT_GRID_OLD(IS_PARALLEL)
   ELSE
      CALL INPUT_GRID()
      CALL INPUT_SOL()
   END IF

   IF (GLOBAL % DO_WALL_REFINEMENT == 1) THEN
      CALL INPUT_RANDBED()
   END IF

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
         ELSE
            WRITE(*,'(A2,X)',ADVANCE="NO") "-"
         END IF
      END DO
      WRITE(*,*)
   END DO


   CALL CALC_SCHWERPUNKTE()

   CALL STR2UNSTR()

#ifdef DEBUG
   IF (GLOBAL%DBG == 1)  THEN
      WRITE(*,*) "======================================================" &
                ,"INPUT ENDE"  &
                ,"======================================================"

   END IF
#endif

END SUBROUTINE
