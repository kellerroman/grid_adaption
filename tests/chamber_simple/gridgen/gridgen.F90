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

INTEGER, PARAMETER :: B2S(4) = (/2,3,5,6/)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!                                                                                        !!!!!
!!!!!!                              GITTERABMESSUNGEN                                         !!!!!
!!!!!!                                                                                        !!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INTEGER, PARAMETER :: NUM_OF_BLOCKS = 6


!!!!!!!!!!!!!!!! NJ VALUES 
INTEGER, PARAMETER :: N1 = 10
INTEGER, PARAMETER :: N2 = 10
INTEGER, PARAMETER :: N3 = 10
INTEGER, PARAMETER :: N4 = 20
!!!!!!!!!!!!!!!! NI VALUES 
INTEGER, PARAMETER :: N5 = 10
INTEGER, PARAMETER :: N6 = 50
INTEGER, PARAMETER :: N7 = 10
INTEGER, PARAMETER :: N8 = 2
INTEGER, PARAMETER :: N9 = 10

REAL(KIND=8), PARAMETER :: xm1 = -1.0D0
REAL(KIND=8), PARAMETER :: x0  =  0.0D0
REAL(KIND=8), PARAMETER :: x2  =  5.0D0
REAL(KIND=8), PARAMETER :: x3  =  5.4D0
REAL(KIND=8), PARAMETER :: x4  =  5.5D0
REAL(KIND=8), PARAMETER :: x5  =  6.0D0

REAL(KIND=8), PARAMETER :: y0 = 0.0D0
REAL(KIND=8), PARAMETER :: y1 = 1.0D0
REAL(KIND=8), PARAMETER :: y2 = 2.0D0
REAL(KIND=8), PARAMETER :: y3 = 2.5D0
REAL(KIND=8), PARAMETER :: y4 = 5.0D0
REAL(KIND=8), PARAMETER :: y5 = 2.0D0
REAL(KIND=8), PARAMETER :: y6 = 3.0D0

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
WRITE(*,*) "======================================================" &
          ,"GRIDGEN"  &
          ,"======================================================"

BLOCKS(1) % NI = N5
BLOCKS(1) % NJ = N1
BLOCKS(1) % xstart = xm1
BLOCKS(1) % xend   = x0
BLOCKS(1) % ystart = y0
BLOCKS(1) % yend   = y1

BLOCKS(2) % NI = N6+N7+N8+N9
BLOCKS(2) % NJ = N1
BLOCKS(2) % xstart = x0
BLOCKS(2) % xend   = x5
BLOCKS(2) % ystart = y0
BLOCKS(2) % yend   = y1

BLOCKS(3) % NI = N6+N7+N8+N9
BLOCKS(3) % NJ = N2
BLOCKS(3) % xstart = x0
BLOCKS(3) % xend   = x5
BLOCKS(3) % ystart = y1
BLOCKS(3) % yend   = y2

BLOCKS(4) % NI = N5
BLOCKS(4) % NJ = N3
BLOCKS(4) % xstart = xm1
BLOCKS(4) % xend   = x0
BLOCKS(4) % ystart = y2
BLOCKS(4) % yend   = y3

BLOCKS(5) % NI = N6+N7+N8+N9
BLOCKS(5) % NJ = N3
BLOCKS(5) % xstart = x0
BLOCKS(5) % xend   = x5
BLOCKS(5) % ystart = y2
BLOCKS(5) % yend   = y3

BLOCKS(6) % NI = N6+N7+N8+N9
BLOCKS(6) % NJ = N4
BLOCKS(6) % xstart = x0
BLOCKS(6) % xend   = x5
BLOCKS(6) % ystart = y3
BLOCKS(6) % yend   = y4

N = 0

DO B = 1, NUM_OF_BLOCKS
   ALLOCATE(BLOCKS(B) % XYZ(BLOCKS(B) % NI + 1, BLOCKS(B) % NJ + 1, 1, NDIM))
   N = N + BLOCKS(B) % NI *BLOCKS(B) % NJ
   call make_block_grid(blocks(B))
END DO

do i = n6+1, n6+n7+n8+n9+1
   y_max = y4 + (y5-y4) * dble(i-n6-1) / dble(n7)
   if (i < n6+n7+1) then
   else if (i< n6+n7+n8+1) then
   y_max = y5
   else
   write(*,*) i,i-n6-n7-n8-1
   y_max = y5 + (y6-y5) * dble(i-n6-n7-n8-1) / dble(n9)

   end if

   write(*,*) i,y_max
   dy = y_max / (N1+N2+N3+N4)
   gj = 0
   do n = 1,4
      b = B2S(n)
      do j = 1, BLOCKS(b) % nj+1
         gj = gj + 1
         blocks(b) % xyz(i,j,1,2) = dy * (gj-1)
      end do
      gj = gj -1
   end do
end do


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
