PROGRAM GridGen
IMPLICIT NONE

   TYPE :: TBLOCKS
      INTEGER :: NI
      INTEGER :: NJ
      INTEGER :: NPK
      REAL(KIND=8),ALLOCATABLE :: XYZ(:,:,:,:)
      REAL(KIND=8) ::xstart,xend,ystart,yend
!< GITTERPUNKTE -POSITION (I,J,K,COORD)
   END TYPE


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
INTEGER, PARAMETER :: N1 = 32
INTEGER, PARAMETER :: N2 = 32
INTEGER, PARAMETER :: N3 = 32
INTEGER, PARAMETER :: N4 = 64
!!!!!!!!!!!!!!!! NI VALUES 
INTEGER, PARAMETER :: N5 = 32
INTEGER, PARAMETER :: N6 = 143 + 11 + 4!228 !196
!!!!!!!!!!!!!!!! NI VALUES (Zwischenwerte) 
INTEGER, PARAMETER :: NT = 30
INTEGER, PARAMETER :: N7 = 21-11
INTEGER, PARAMETER :: N8 = 2
INTEGER, PARAMETER :: N9 = 6

REAL(KIND=8), PARAMETER :: xm1 = -8.0D-3
REAL(KIND=8), PARAMETER :: x0  = 0.0D-3
REAL(KIND=8), PARAMETER :: x1  = 20.0D-3
REAL(KIND=8), PARAMETER :: x2  = 290.0D-3
REAL(KIND=8), PARAMETER :: x3  = 303.44D-3
REAL(KIND=8), PARAMETER :: x4  = 304.44D-3
!REAL(KIND=8), PARAMETER :: x5  = 310.0D-3
REAL(KIND=8), PARAMETER :: x5  = x4 +  (x4-x3) / N8 * N9*1.3D0!308.0D-3


REAL(KIND=8), PARAMETER :: y0 = 0.0D-3
REAL(KIND=8), PARAMETER :: y1 = 2.0D-3
REAL(KIND=8), PARAMETER :: y2 = 2.5D-3
REAL(KIND=8), PARAMETER :: y3 = 3.0D-3
REAL(KIND=8), PARAMETER :: y4 = 6.0D-3
REAL(KIND=8), PARAMETER :: y5 = 4.2819D-3
!REAL(KIND=8), PARAMETER :: y6 = 5.77D-3
!REAL(KIND=8), PARAMETER :: y6 = 5.23471D-3
REAL(KIND=8), PARAMETER :: y6 = 5.0D-3


REAL(KIND = 8) , PARAMETER :: ds_WAND = 1D-6
REAL(KIND = 8) , PARAMETER :: ds_WAND_X = 5D-6
REAL(KIND = 8) , PARAMETER :: CELL_INK = 1.2D0
REAL(KIND = 8) , PARAMETER :: CELL_INK2 = 1.25D0
REAL(KIND = 8) , PARAMETER :: CELL_INK_X = 1.2D0

REAL(KIND = 8) , PARAMETER :: X_STRETCH_O2 = 1.4D0
REAL(KIND = 8) , PARAMETER :: X_STRETCH_CH4 = 1.4D0

TYPE(TBLOCKS) :: BLOCKS(NUM_OF_BLOCKS)

INTEGER :: B,I,J,N,KB

REAL(KIND=8) :: CI, d1(N1+1),d2(N2+1),d3(N3+1),d4(N4+1),D5( n1+n2+n3+n4+1)
REAL(KIND=8) :: D6(n5+1) , D7(n6+n7+n8+n9+1), D5_1( n1+n2+n3+n4+1),D5_2( n1+n2+n3+n4+1)
REAL(KIND = 8) :: D6_uni(n5+1)  !UNIFORME VERTEILUNG AN DER SYMMETRIEACHSE INFLOW
REAL(KIND = 8) :: D6_uni_CH4(n5+1)  !VERGROEBERTE VERTEILUNG FUER DEN CH4 EINLASS

REAL(KIND=8)   :: D7_UNI(n6+n7+n8+n9+1) !UNIFORME VERTEILUNG AN DER SYMMETRIEACHSE BRENNKAMMER
REAL(KIND=8)   :: D7_UNI_CH4(n6+n7+n8+n9+1) !UNIFORME VERTEILUNG AN DER SYMMETRIEACHSE BRENNKAMMER
REAL(KIND=8) :: temp


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!                                                                                        !!!!!
!!!!!!                    BERECHUNG DER KLEINSTEN ZELLGRÖßE IN RADIALER RICHTUNG              !!!!!
!!!!!!                                                                                        !!!!!
!!!!!!       Annahme: konstante Vergrößerung für bestimmte Anzahl an Zellen, dann konst.      !!!!!
!!!!!!                                                                                        !!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!! DÜSEN VERKÜRZEN
!write(*,*) (y6-y5) / (x5-x4) ,(y6-y5) / (x5-x4)  * (308D-3-x4) + y5

!DO I = -50,50
!   WRITE(*,*)I, aSINH(DBLE(I))
!END DO
!STOP

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
END DO

WRITE(*,*) "======================================================" &
          ,"GRIDGEN"  &
          ,"======================================================"

WRITE(*,*) "TOTAL NUMBER OF CELLS:",      N
WRITE(*,*) "WANDZELLGRÖßE:",              ds_WAND
WRITE(*,*) "ZELL VERGRÖSSERUNG:",          CELL_INK
WRITE(*,*) "ZELL VERGRÖSSERUNG2:",         CELL_INK2
WRITE(*,*) "WANDZELLGRÖßE X-DIR:",              ds_WAND_X
WRITE(*,*) "ZELL VERGRÖSSERUNG X-DIR:",         CELL_INK_X
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! X !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



! X VERTEILUNG FÜR DIE EINSTRÖMUNGEN
 CALL ENDE_VORGEGEBEN2  (xm1, x0, ds_WAND_X, CELL_INK_X, N5, CI, D6)
TEMP = (x0 - xm1) / N5

DO I = 1,N5+1
   D6_UNI(I) = xm1 + DBLE(I-1) * TEMP
END DO

 CALL ENDE_VERGROEBERN(D6,D6_UNI    ,N5,N5-5,X_STRETCH_O2,1.3D0)

 CALL ENDE_VERGROEBERN(D6,D6_UNI_CH4,N5,N5-5, X_STRETCH_CH4,1.3D0)

!CALL ANFANG_VORGEGEBEN2  (xm1, x0, ds_WAND, CELL_INK, N5, CI, D6)
!WRITE(*,*) D6
!STOP
! X VERTEILUNG FÜR DEN REST DER BRENNKAMMER
 CALL ANFANG_VORGEGEBEN2  (x0, x2, ds_WAND_X, CELL_INK_X, N6, CI, D7(1:n6+1))


temp = x3-x2
temp = temp / N7 

DO i = 1, N7
   D7(n6+1+i) = D7(n6+i) + temp
END DO

temp = x4-x3
temp = temp / N8 

DO i = 1, N8
   D7(n6+n7+1+i) = D7(n6+n7+i) + temp
END DO

CALL ANFANG_VORGEGEBEN2  (x4, x5, TEMP, CELL_INK, N9, CI, D7 ( n6+n7+n8+1 : n6+n7+n8+n9+1 ) )
CALL ENDE_VORGEGEBEN2  (x2, x3, TEMP*1.2, CELL_INK, N7, CI, D7 ( n6+1 : n6+n7+1 ) )


! CALL ENDE_VORGEGEBEN2  (x2, x3, TEMP, CELL_INK, N7, CI, D7 ( n6+1 : n6+n7+1 ) )


 CALL ANFANG_VERGROEBERN(D7,D7_UNI,n6+n7+n8+n9,n6+n7+n8+n9-5,X_STRETCH_O2,1.3D0)
 CALL ANFANG_VERGROEBERN(D7,D7_UNI_CH4,n6+n7+n8+n9,n6+n7+n8+n9-5,X_STRETCH_CH4,1.3D0)
!temp = x5-x4
!temp = temp / N9
!
!DO i = 1, N9
!   D7(n6+n7+n8+1+i) = D7(n6+n7+n8+i) + temp
!END DO

write(*,*)
write(*,'(4(A12,X))') "POS","DN1","DN2","DN1/DN2"
write(*,'(3(ES12.5,X))') d7(1) &
                       , 0.0D0 &
                       , d7(2)-d7(1)

write(*,'(4(ES12.5,X))') d7(n6+1) &
                       , d7(n6+1)-d7(n6) &
                       , d7(n6+2)-d7(n6+1) &
                       ,(d7(n6+1)-d7(n6)) / (d7(n6+2)-d7(n6+1))

write(*,'(4(ES12.5,X))') d7(n6+n7+1) &
                       , d7(n6+n7+2)-d7(n6+n7+1) &
                       , d7(n6+n7+1)-d7(n6+n7)   &
                       ,(d7(n6+n7+2)-d7(n6+n7+1)) / (d7(n6+n7+1)-d7(n6+n7))

write(*,'(4(ES12.5,X))') d7(n6+n7+n8+1) &
                       , d7(n6+n7+n8+2)-d7(n6+n7+n8+1) &
                       , d7(n6+n7+n8+1)-d7(n6+n7+n8)   &
                       ,(d7(n6+n7+n8+2)-d7(n6+n7+n8+1)) / (d7(n6+n7+n8+1)-d7(n6+n7+n8))

write(*,'(2(ES12.5,X))') d7(n6+n7+n8+n9+1) &
                       , d7(n6+n7+n8+n9+1)-d7(n6+n7+n8+n9)
!do i = 2, n6+n7+n8+n9+1
!   write(*,*) i-1,d7(i), d7(i) - d7(i-1)  
!end do

DO B = 1, NUM_OF_BLOCKS
   IF (B == 1 .OR. B == 4) THEN
      DO I = 1,BLOCKS(B) % NJ+1
         BLOCKS(B) % XYZ(:, I, 1, 1) = D6
      END DO
   ELSE
      DO I = 1,BLOCKS(B) % NJ+1
         BLOCKS(B) % XYZ(:, I, 1, 1) = D7
      END DO
   END IF
END DO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Y !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! CALL ENDE_VORGEGEBEN2 (y0, y1, ds_WAND, CELL_INK, N1, CI, D1)
 CALL ENDE_VORGEGEBEN2 (y0, y1, ds_WAND, CELL_INK2, N1, CI, D1)
 CALL BEIDE_VORGEGEBEN (y1, y2, ds_WAND, CELL_INK, N2, CI, D2)
 CALL BEIDE_VORGEGEBEN (y2, y3, ds_WAND, CELL_INK, N3, CI, D3)
 CALL BEIDE_VORGEGEBEN (y3, y4, ds_WAND, CELL_INK, N4, CI, D4)
 CALL ENDE_VORGEGEBEN2 (y0, y4, ds_WAND, CELL_INK, n1+n2+n3+n4 , CI, D5)

 CALL ENDE_VORGEGEBEN2 (y0, y4, ds_WAND, CELL_INK, n1+n2+n3+n4 , CI, D5_1)

 CALL ENDE_VORGEGEBEN2 (y0, y4, ds_WAND, CELL_INK, n1+n2+n3+n4 , CI, D5_2)

B = 1
BLOCKS(B) % XYZ(1, :, 1, 2) = D1
BLOCKS(B) % XYZ(1, :, 1, 1) = BLOCKS(B) % xstart
BLOCKS(B) % XYZ(BLOCKS(B) % NI+1, :, 1, 2) = D1
BLOCKS(B) % XYZ(BLOCKS(B) % NI+1, :, 1, 1) = BLOCKS(B) % xend
B = 2
BLOCKS(B) % XYZ(1, :, 1, 2) = D1
BLOCKS(B) % XYZ(1, :, 1, 1) = BLOCKS(B) % xstart
BLOCKS(B) % XYZ(BLOCKS(B) % NI+1, :, 1, 2) = D1
BLOCKS(B) % XYZ(BLOCKS(B) % NI+1, :, 1, 1) = BLOCKS(B) % xend
B = 3
BLOCKS(B) % XYZ(1, :, 1, 2) = D2
BLOCKS(B) % XYZ(1, :, 1, 1) = BLOCKS(B) % xstart
BLOCKS(B) % XYZ(BLOCKS(B) % NI+1, :, 1, 2) = D2
BLOCKS(B) % XYZ(BLOCKS(B) % NI+1, :, 1, 1) = BLOCKS(B) % xend
B = 4
BLOCKS(B) % XYZ(1, :, 1, 2) = D3
BLOCKS(B) % XYZ(1, :, 1, 1) = BLOCKS(B) % xstart
BLOCKS(B) % XYZ(BLOCKS(B) % NI+1, :, 1, 2) = D3
BLOCKS(B) % XYZ(BLOCKS(B) % NI+1, :, 1, 1) = BLOCKS(B) % xend
B = 5
BLOCKS(B) % XYZ(1, :, 1, 2) = D3
BLOCKS(B) % XYZ(1, :, 1, 1) = BLOCKS(B) % xstart
BLOCKS(B) % XYZ(BLOCKS(B) % NI+1, :, 1, 2) = D3
BLOCKS(B) % XYZ(BLOCKS(B) % NI+1, :, 1, 1) = BLOCKS(B) % xend
B = 6
BLOCKS(B) % XYZ(1, :, 1, 2) = D4
BLOCKS(B) % XYZ(1, :, 1, 1) = BLOCKS(B) % xstart
BLOCKS(B) % XYZ(BLOCKS(B) % NI+1, :, 1, 2) = D4
BLOCKS(B) % XYZ(BLOCKS(B) % NI+1, :, 1, 1) = BLOCKS(B) % xend

DO B =1, NUM_OF_BLOCKS
   DO I = 1, BLOCKS(B) % NJ+1
      CALL INTERPOLATE(BLOCKS(B) % XYZ(:, I, 1, 1),BLOCKS(B) % XYZ(:, I, 1, 2),BLOCKS(B) % NI)
   END DO
END DO

B = 1
BLOCKS(1) % XYZ(:, 1, 1, 1) = D6_uni
DO I = 1, BLOCKS(B) % NI+1
      CALL INTERPOLATE(BLOCKS(B) % XYZ(I, :, 1, 2),BLOCKS(B) % XYZ(I, :, 1, 1),BLOCKS(B) % NJ)
END DO

B = 2
BLOCKS(B) % XYZ(:, 1, 1, 1) = D7_uni
DO I = 1, BLOCKS(B) % NI+1
      CALL INTERPOLATE(BLOCKS(B) % XYZ(I, :, 1, 2),BLOCKS(B) % XYZ(I, :, 1, 1),BLOCKS(B) % NJ)
END DO


B = 4
BLOCKS(B) % XYZ( :, BLOCKS(B) % NJ / 2 + 1, 1, 1) = D6_UNI_CH4
DO I = 1, BLOCKS(B) % NI+1
      CALL INTERPOLATE(BLOCKS(B) % XYZ(I, 1:BLOCKS(B) % NJ / 2 + 1, 1, 2) &
                      ,BLOCKS(B) % XYZ(I, 1:BLOCKS(B) % NJ / 2 + 1, 1, 1),BLOCKS(B) % NJ/2)

      CALL INTERPOLATE(BLOCKS(B) % XYZ(I, BLOCKS(B) % NJ / 2 +1 : BLOCKS(B) % NJ+1, 1, 2) &
                      ,BLOCKS(B) % XYZ(I, BLOCKS(B) % NJ / 2 +1 : BLOCKS(B) % NJ+1, 1, 1),BLOCKS(B) % NJ/2)
END DO

B = 5
BLOCKS(B) % XYZ( :, BLOCKS(B) % NJ / 2 + 1, 1, 1) = D7_UNI_CH4
DO I = 1, BLOCKS(B) % NI+1
      CALL INTERPOLATE(BLOCKS(B) % XYZ(I, 1:BLOCKS(B) % NJ / 2 + 1, 1, 2) &
                      ,BLOCKS(B) % XYZ(I, 1:BLOCKS(B) % NJ / 2 + 1, 1, 1),BLOCKS(B) % NJ/2)

      CALL INTERPOLATE(BLOCKS(B) % XYZ(I, BLOCKS(B) % NJ / 2 +1 : BLOCKS(B) % NJ+1, 1, 2) &
                      ,BLOCKS(B) % XYZ(I, BLOCKS(B) % NJ / 2 +1 : BLOCKS(B) % NJ+1, 1, 1),BLOCKS(B) % NJ/2)
END DO

N = 1
DO KB = 1, 4
   B = B2S(KB)
   
   DO I = NT+1,N6+1
      BLOCKS(B) % XYZ(I, :, 1, 2) = D5(N : BLOCKS(B) % NJ+N)
   END DO
   
   I = N6+N7+1
   BLOCKS(B) % XYZ(I, :, 1, 2) = D5_1(N : BLOCKS(B) % NJ+N)
   I = N6+N7+N8+1
   BLOCKS(B) % XYZ(I, :, 1, 2) = D5_1(N : BLOCKS(B) % NJ+N)
   I = N6+N7+N8+N9+1
   BLOCKS(B) % XYZ(I, :, 1, 2) = D5_2(N : BLOCKS(B) % NJ+N)

   N = N + BLOCKS(B) % NJ

END DO

DO N = 1, 4
   B = B2S(N)
   DO J = 1, BLOCKS(B) % NJ+1
      CALL INTERPOLATE(BLOCKS(B) % XYZ(1:NT+1, J, 1, 1),BLOCKS(B) % XYZ(1:NT+1, J, 1, 2),NT)
      CALL INTERPOLATE(BLOCKS(B) % XYZ(N6+1:N6+N7+1, J, 1, 1),BLOCKS(B) % XYZ(N6+1:N6+N7+1, J, 1, 2),N7)
      CALL INTERPOLATE(BLOCKS(B) % XYZ(N6+N7+1:N6+N7+N8+1, J, 1, 1),BLOCKS(B) % XYZ(N6+N7+1:N6+N7+N8+1, J, 1, 2),N8)
      CALL INTERPOLATE(BLOCKS(B) % XYZ(N6+N7+N8+1:N6+N7+N8+N9+1, J, 1, 1),BLOCKS(B) % XYZ(N6+N7+N8+1:N6+N7+N8+N9+1, J, 1, 2),N9)
   END DO

END DO

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

SUBROUTINE INTERPOLATE(X,Y,N)

IMPLICIT NONE
REAL( KIND = 8), INTENT(IN) :: X(N+1)
REAL( KIND = 8), INTENT(INOUT) :: Y(N+1)
INTEGER , INTENT(IN) :: N

REAL( KIND = 8) :: L

INTEGER :: I

L = (Y(N+1) - Y(1)) / (DBLE(X(n+1)) - DBLE(X(1)) )

DO I = 2, N
   Y(I) = Y(1) + L * (DBLE(X(I)) - DBLE(X(1)))
ENDDO

END SUBROUTINE

SUBROUTINE ANFANG_VORGEGEBEN(X_Start,X_End,DN,CI_max,N,CI,X)
IMPLICIT NONE
INTEGER, INTENT(IN) :: N
REAL( KIND = 8), INTENT(IN) :: X_Start,X_End,DN,CI_max
REAL( KIND = 8), INTENT(OUT) :: CI,X(N+1)

INTEGER :: NUM_ITER , I
REAL( KIND = 8) :: F, FN,L
L = X_End - X_Start
NUM_ITER = 0
 CI = CI_max
DO
   NUM_ITER = NUM_ITER + 1
   FN = DN*((CI*(N-1.0D0)-DBLE(N))*CI**N + CI)/(CI*(CI-1.0D0)**2)
   F = DN*(1.0D0-CI**N)/(1.0D0-CI) - L
   CI = CI- F/FN

   IF (NUM_ITER >= 100) THEN
      EXIT
   END IF
END DO
X(1) = X_Start

DO I = 1,N
   X(I+1) = X(I) + DN * CI ** (I-1)
END DO
END SUBROUTINE


SUBROUTINE ENDE_VORGEGEBEN(X_Start,X_End,DN,CI_max,N,CI,X)
IMPLICIT NONE
INTEGER, INTENT(IN) :: N 
REAL( KIND = 8), INTENT(IN) :: X_Start,X_End,DN,CI_max
REAL( KIND = 8), INTENT(OUT) :: CI,X(N+1)

INTEGER :: NUM_ITER , I 
REAL( KIND = 8) :: F, FN,L
L = X_End - X_Start
NUM_ITER = 0
 CI = CI_max
DO
   NUM_ITER = NUM_ITER + 1
   FN = DN*((CI*(N-1.0D0)-DBLE(N))*CI**N + CI)/(CI*(CI-1.0D0)**2)
   F = DN*(1.0D0-CI**N)/(1.0D0-CI) - L
   CI = CI- F/FN
   
   IF (NUM_ITER >= 100) THEN
      EXIT
   END IF
END DO
X(1) = X_Start

DO I = 1,N
   X(I+1) = X(I) + DN * CI ** (N-I)
END DO
END SUBROUTINE


SUBROUTINE BEIDE_VORGEGEBEN(X_Start,X_End,DN,CI_max,Ni,CI,X)
IMPLICIT NONE
INTEGER, INTENT(IN) :: Ni
REAL( KIND = 8), INTENT(IN) :: X_Start,X_End,DN,CI_max
REAL( KIND = 8), INTENT(OUT) :: CI, X(Ni+1)

INTEGER :: NUM_ITER , I 
REAL( KIND = 8) :: F, FN,L
INTEGER :: N


IF (MOD(Ni,2) /= 0) THEN
   STOP "ZELLANZAHL MUSS GERADE SEIN"
END IF

N = Ni * 0.5

L = (X_End - X_Start) / 2
NUM_ITER = 0
CI = CI_max
DO
   NUM_ITER = NUM_ITER + 1
   FN = DN*((CI*(N-1.0D0)-DBLE(N))*CI**N + CI)/(CI*(CI-1.0D0)**2)
   F = DN*(1.0D0-CI**N)/(1.0D0-CI) - L
   CI = CI- F/FN
   IF (NUM_ITER >= 100) THEN
      EXIT
   END IF
END DO

IF (CI > CI_max) THEN
   WRITE(*,*) "CELL STRECH RATIO IST ZU GROß",CI,CI_max
END IF
X(1) = X_Start

DO I = 1,N
   X(I+1) = X(I) + DN * CI ** (I - 1)
END DO
DO I = N+1,Ni
   X(I+1) = X(I) + DN * CI ** (Ni - I  )
END DO

END SUBROUTINE

SUBROUTINE ENDE_VORGEGEBEN2(X_Start,X_End,DN,CI_max,N,CI,X)
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: N
   REAL( KIND = 8), INTENT(IN) :: X_Start,X_End,DN,CI_max
   REAL( KIND = 8), INTENT(OUT) :: CI,X(N+1)

   INTEGER :: NUM_ITER , I, n1,n2
   REAL( KIND = 8) :: F,L,DN2,L1,L2
   
   L = X_End - X_Start
   NUM_ITER = 0
   CI = CI_max
   N2 = N
   L2 = L
   IF (DN*(1.0D0-CI**N)/(1.0D0-CI) < L) THEN
      WRITE(*,*) "LAENGE:",L, "IST MIT DN:",DN,"CI:",ci,"N:",N,"NICHT DARSTELLBAR"
      WRITE(*,*) "MAXIMALE LÄNGE:",DN*(1.0D0-CI**N)/(1.0D0-CI)
      STOP "ENDE_VORGEGEBEN2"
   END IF
!   WRITE(*,*) "==========",L,N,DN,CI_max

   DO
      NUM_ITER = NUM_ITER + 1
      DN2 = L2 / N2
      N1 = LOG(DN2/DN)/LOG(CI)
      N2 = N - N1
      L1 = DN * (1.0D0-CI**N1)/(1.0D0-CI)
      L2 = L- L1
      F = N2 * DN2 + L1
!      WRITE(*,*) NUM_ITER,N1,N2,L1,L2,DN2,F
      
      IF (ABS(F-L) <= 1E-8) THEN
         EXIT
      END IF

      IF (NUM_ITER >= 100) THEN
         STOP "KONVERGENZ IN ENDE_VORGEGEBEN2 NICHT ERREICHT"
      END IF
   END DO
   X(1) = X_Start

   DO I = 1,N2
      X(I+1) = X(I) + DN2
   END DO
   DO I = N2+1,N
      X(I+1) = X(I) + DN * CI ** (N-I)
   END DO
   X(N+1) = X_End
END SUBROUTINE

SUBROUTINE ANFANG_VORGEGEBEN2(X_Start,X_End,DN,CI_max,N,CI,X)
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: N
   REAL( KIND = 8), INTENT(IN) :: X_Start,X_End,DN,CI_max
   REAL( KIND = 8), INTENT(OUT) :: CI,X(N+1)

   INTEGER :: NUM_ITER , I, n1,n2
   REAL( KIND = 8) :: F,L,DN2,L1,L2

   L = X_End - X_Start
   NUM_ITER = 0
   CI = CI_max
   N2 = N
   L2 = L

   IF (DN*(1.0D0-CI**N)/(1.0D0-CI) < L) THEN
      WRITE(*,*) "LAENGE:",L, "IST MIT DN:",DN,"CI:",ci,"N:",N,"NICHT DARSTELLBAR"
      WRITE(*,*) "MAXIMALE LÄNGE:",DN*(1.0D0-CI**N)/(1.0D0-CI)
      STOP "ANFANG_VORGEGEBEN2"
   END IF

   !WRITE(*,*) "==========",L,N,DN,CI_max
   DO
      NUM_ITER = NUM_ITER + 1
      DN2 = L2 / N2
      N1 = LOG(DN2/DN)/LOG(CI)
      N2 = N - N1
      L1 = DN * (1.0D0-CI**N1)/(1.0D0-CI)
      L2 = L- L1
      F = N2*DN2+L1
   !   WRITE(*,*) N1,N2,L1,L2,DN2,F
      
      IF (ABS(F-L) <= 1E-8) THEN
         EXIT
      END IF

      IF (NUM_ITER >= 100) THEN
         STOP "KONVERGENZ IN ANFANG_VORGEGEBEN2 NICHT ERREICHT"
      END IF
   END DO
   X(1) = X_Start

   DO I = 1,N1
      X(I+1) = X(I) + DN * CI ** (I-1)
   END DO
   DO I = N1+1,N
      X(I+1) = X(I) + DN2
   END DO
   X(N+1) = X_End
END SUBROUTINE


SUBROUTINE ENDE_VERGROEBERN(X_ALT,X_NEU,NI,NI_DIFF_MAX,WAND_DELTA,CELL_INK_MAX)
! DIESE ROUTINE NIMMT EINE GEGEBEN VERTEILUNG (X_ALT) UND VERGROESSERT (X_NEU) DAS ENDE
! UM DEN FAKTOR WAND_DELTA, wobei versucht wird moeglichst früh wieder das urspruengliche GITTER ABZUBILDEN (spätestens nach NI_DIFF_MAX)
! X_ALT(1:NI-NI_DIFF+1) = X_NEU(1:NI-NI_DIFF+1)
   IMPLICIT NONE
   INTEGER, INTENT(IN)           :: NI                    ! ANZAHL DER ZELLEN, ARRAYGRÖ?E: NI +1
   INTEGER, INTENT(IN)           :: NI_DIFF_MAX           ! ANZAHL DER ZELLEN WELCHE UNTERSCHIEDLICHE ZELLGROESSEN HABEN DUERFEN
   REAL(KIND = 8), INTENT(IN)    :: WAND_DELTA            ! VERGROESSERUNG IN X_NEU GEGENÜBER X_ALT
   REAL(KIND = 8), INTENT(IN)    :: CELL_INK_MAX          ! MAXIMALE ZELLVERGROESSERUNG
   REAL(KIND = 8), INTENT(IN)    :: X_ALT(NI+1)           ! VERFEINERTES GITTER  (EINGABEWERT)
   REAL(KIND = 8), INTENT(OUT)   :: X_NEU(NI+1)           ! VERGROEBERTES GITTER (AUSGABEWERT)

   INTEGER :: NZ
   INTEGER :: ITER,I
   REAL :: WERT
   REAL :: WANDWERT
   REAL :: CI, F , FS
   WANDWERT = ABS( X_ALT(NI) - X_ALT(NI+1) ) * WAND_DELTA
   X_NEU = X_ALT
!   WRITE(*,*) "WANDWERT:",WANDWERT
   NZ = 1
   DO
      NZ = NZ + 1

      WERT = ABS( X_ALT(NI + 1) - X_ALT(NI + 1 - NZ) )
      CI = CELL_INK_MAX
      ITER = 0

!      WRITE(*,*) NZ, WERT
      IF (WERT < WANDWERT*NZ) CYCLE
      DO
         ITER = ITER +1
         F  = WERT - WANDWERT * (CI**NZ-1.0D0)/(CI-1.0D0)
         FS = - WANDWERT * ((CI*DBLE(NZ-1)-NZ)* CI ** NZ + CI) / (CI * (CI-1.0D0)*(CI-1.0D0))
         CI = CI - F / FS
!         WRITE(*,*) ITER,CI,F,FS
         IF (ABS(F) < 1D-8) THEN
            EXIT
         END IF
      END DO
      WERT = ABS(X_NEU(NI-NZ) - X_NEU(NI+1-NZ))
      WERT = ABS(WERT / ( WANDWERT * CI ** (NZ-1)))
      WRITE(*,*) WERT, CI
      IF (CI <= CELL_INK_MAX .AND. 1/CELL_INK_MAX <= WERT .AND. WERT <= CELL_INK_MAX ) THEN
         WRITE(*,*) "END VERGROEBERUNG INNERHALB VON ",NZ, "ZELLEN", "UM FAKTOR",WAND_DELTA
         EXIT
      END IF
!      IF (NZ == NI_DIFF_MAX) THEN
!         STOP
!      END IF
   END DO

   DO I = 1, NZ
      X_NEU(NI+1-I) = X_NEU(NI+2-I) - WANDWERT * CI ** (I-1)
!      WRITE(*,*) X_NEU(NI+1-i), WANDWERT * CI ** (I-1)
   END DO


END SUBROUTINE ENDE_VERGROEBERN

SUBROUTINE ANFANG_VERGROEBERN(X_ALT,X_NEU,NI,NI_DIFF_MAX,WAND_DELTA,CELL_INK_MAX)
! DIESE ROUTINE NIMMT EINE GEGEBEN VERTEILUNG (X_ALT) UND VERGROESSERT (X_NEU) DEN ANFANG
! UM DEN FAKTOR WAND_DELTA, wobei versucht wird moeglichst früh wieder das urspruengliche GITTER ABZUBILDEN (spätestens nach NI_DIFF_MAX)
! X_ALT(1:NI_DIFF+1) = X_NEU(1:NI_DIFF+1)
   IMPLICIT NONE
   INTEGER, INTENT(IN)           :: NI                    ! ANZAHL DER ZELLEN, ARRAYGRÖ?E: NI +1
   INTEGER, INTENT(IN)           :: NI_DIFF_MAX           ! ANZAHL DER ZELLEN WELCHE UNTERSCHIEDLICHE ZELLGROESSEN HABEN DUERFEN
   REAL(KIND = 8), INTENT(IN)    :: WAND_DELTA            ! VERGROESSERUNG IN X_NEU GEGENÜBER X_ALT
   REAL(KIND = 8), INTENT(IN)    :: CELL_INK_MAX          ! MAXIMALE ZELLVERGROESSERUNG
   REAL(KIND = 8), INTENT(IN)    :: X_ALT(NI+1)           ! VERFEINERTES GITTER  (EINGABEWERT)
   REAL(KIND = 8), INTENT(OUT)   :: X_NEU(NI+1)           ! VERGROEBERTES GITTER (AUSGABEWERT)

   INTEGER :: NZ
   INTEGER :: ITER,I
   REAL :: WERT
   REAL :: WANDWERT
   REAL :: CI, F , FS
   WANDWERT = ABS( X_ALT(2) - X_ALT(1) ) * WAND_DELTA
   X_NEU = X_ALT
!   WRITE(*,*) "WANDWERT:",WANDWERT
   NZ = 1
   DO
      NZ = NZ + 1

      WERT = ABS( X_ALT(1) - X_ALT(NZ+1) )
      CI = CELL_INK_MAX
      ITER = 0

!      WRITE(*,*) NZ, WERT
      IF (WERT < WANDWERT*NZ) CYCLE
      DO
         ITER = ITER +1
         F  = WERT - WANDWERT * (CI**NZ-1.0D0)/(CI-1.0D0)
         FS = - WANDWERT * ((CI*DBLE(NZ-1)-NZ)* CI ** NZ + CI) / (CI * (CI-1.0D0)*(CI-1.0D0))
         CI = CI - F / FS
!         WRITE(*,*) ITER,CI,F,FS
         IF (ABS(F) < 1D-6) THEN
            EXIT
         END IF
         IF (ITER >= 1000) THEN
            WRITE(*,*) ITER,CI,F,FS
            STOP "NO CONVERGENCE WAS ACHIEVED IN NEWTON ITERATION"
         END IF
      END DO
      WERT = ABS(X_NEU(NZ+1) - X_NEU(2+NZ))
      WERT = ABS(WERT / ( WANDWERT * CI ** (NZ-1)))
!      WRITE(*,*) WERT
      IF (CI <= CELL_INK_MAX .AND. 1/CELL_INK_MAX <= WERT .AND. WERT <= CELL_INK_MAX ) THEN
         WRITE(*,*) "ANFANG VERGROEBERUNG INNERHALB VON ",NZ, "ZELLEN", "UM FAKTOR",WAND_DELTA
         EXIT
      END IF
      IF (NZ == NI_DIFF_MAX) THEN
         STOP
      END IF
   END DO

   DO I = 1, NZ
      X_NEU(I+1) = X_NEU(I) + WANDWERT * CI ** (I-1)
!      WRITE(*,*) X_NEU(NI+1-i), WANDWERT * CI ** (I-1)
   END DO


END SUBROUTINE ANFANG_VERGROEBERN
