module types
   TYPE :: TBLOCKS
      INTEGER :: NI
      INTEGER :: NJ
      INTEGER :: NPK
      REAL(KIND=8),ALLOCATABLE :: XYZ(:,:,:,:)
      REAL(KIND=8) ::xstart,xend,ystart,yend
!< GITTERPUNKTE -POSITION (I,J,K,COORD)
   END TYPE
end module types

PROGRAM GridGen
use types
IMPLICIT NONE




INTEGER, PARAMETER :: FU_GIT_OUT = 34
INTEGER, PARAMETER :: IOOUT = 10
INTEGER, PARAMETER :: AXSYM = 1
INTEGER, PARAMETER :: NDIM = 2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!                                                                                        !!!!!
!!!!!!                              GITTERABMESSUNGEN                                         !!!!!
!!!!!!                                                                                        !!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INTEGER, PARAMETER :: NUM_OF_BLOCKS = 3


!!!!!!!!!!!!!!!! NJ VALUES 
INTEGER, PARAMETER :: N1 = 10
INTEGER, PARAMETER :: N2 = 20
!!!!!!!!!!!!!!!! NI VALUES 
INTEGER, PARAMETER :: N3 = 10
INTEGER, PARAMETER :: N4 = 10

REAL(KIND=8), PARAMETER :: xm1 = -1.0D0
REAL(KIND=8), PARAMETER :: x0  =  0.0D0
REAL(KIND=8), PARAMETER :: x1  =  1.0D0

REAL(KIND=8), PARAMETER :: y0 = 2.0D0
REAL(KIND=8), PARAMETER :: y1 = 2.5D0
REAL(KIND=8), PARAMETER :: y2 = 5.0D0

TYPE(TBLOCKS) :: BLOCKS(NUM_OF_BLOCKS)

INTEGER :: B,I,J,N,KB,gj

REAL(KIND=8) :: y_max,dy
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!                                                                                        !!!!!
!!!!!!                    BERECHUNG DER KLEINSTEN ZELLGRÖßE IN RADIALER RICHTUNG              !!!!!
!!!!!!                                                                                        !!!!!
!!!!!!       Annahme: konstante Vergrößerung für bestimmte Anzahl an Zellen, dann konst.      !!!!!
!!!!!!                                                                                        !!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
WRITE(*,*) "================================================" &
          ,"GRIDGEN FOR SIMPLE INJECTOR"  &
          ,"================================================"

BLOCKS(1) % NI = N3
BLOCKS(1) % NJ = N1
BLOCKS(1) % xstart = xm1
BLOCKS(1) % xend   = x0
BLOCKS(1) % ystart = y0
BLOCKS(1) % yend   = y1

BLOCKS(2) % NI = N4
BLOCKS(2) % NJ = N1
BLOCKS(2) % xstart = x0
BLOCKS(2) % xend   = x1
BLOCKS(2) % ystart = y0
BLOCKS(2) % yend   = y1

BLOCKS(3) % NI = N4
BLOCKS(3) % NJ = N2
BLOCKS(3) % xstart = x0
BLOCKS(3) % xend   = x1
BLOCKS(3) % ystart = y1
BLOCKS(3) % yend   = y2

N = 0

DO B = 1, NUM_OF_BLOCKS
   ALLOCATE(BLOCKS(B) % XYZ(BLOCKS(B) % NI + 1, BLOCKS(B) % NJ + 1, 1, NDIM))
   N = N + BLOCKS(B) % NI *BLOCKS(B) % NJ
   call make_block_grid(blocks(B))
END DO




WRITE(*,*) "TOTAL NUMBER OF CELLS:",      N
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! X !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


OPEN(FU_GIT_OUT,FILE="git.bin",FORM="UNFORMATTED",access="STREAM",STATUS="REPLACE")  !

!OPEN(IOOUT,FILE="git.dat",STATUS="REPLACE")

WRITE(FU_GIT_OUT) AXSYM
WRITE(FU_GIT_OUT) NUM_OF_BLOCKS

DO B = 1,NUM_OF_BLOCKS
   WRITE(FU_GIT_OUT) BLOCKS(B) % NI+1, BLOCKS(B) % NJ+1
END DO

DO B = 1,NUM_OF_BLOCKS
!   WRITE(IOOUT,'(2I5)') BLOCKS(B) % NI+1, BLOCKS(B) % NJ+1
   DO j = 1,BLOCKS(B) % NJ+1
      DO i = 1,BLOCKS(B) % NI+1
         WRITE(FU_GIT_OUT) (BLOCKS(B) % XYZ(I,J,1,N),N=1,NDIM)
!         WRITE(IOOUT,'(2(X,D20.13))') (BLOCKS(B) % XYZ(I,J,1,N),N=1,NDIM)
      END DO
   END DO
END DO

!CLOSE(IOOUT)
 CLOSE(FU_GIT_OUT)


WRITE(*,*) "======================================================" &
          ,"FINISHED"  &
          ,"======================================================"

END PROGRAM GridGen

subroutine make_block_grid(block)
use types
implicit none
TYPE(TBLOCKS) :: block

integer :: i,j
real(kind=8) :: dx,dy

dx = ( block % xend - block % xstart ) / block % ni
dy = ( block % yend - block % ystart ) / block % nj
do j = 1, block%nj+1
   do i = 1, block%ni+1
      block % xyz(i,j,1,1) = block % xstart + dx * (i-1)
      block % xyz(i,j,1,2) = block % ystart + dy * (j-1)
   end do
end do
end subroutine make_block_grid
