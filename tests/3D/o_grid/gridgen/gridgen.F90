program gridgen_o_grid
implicit none

INTEGER, PARAMETER :: NI1 = 10
INTEGER, PARAMETER :: NK1 = 16
INTEGER, PARAMETER :: NY1 = 32
INTEGER, PARAMETER :: FU = 666
INTEGER, PARAMETER :: NB = 3
INTEGER, PARAMETER :: AXSYM = 2
REAL(KIND = 8), PARAMETER :: Pi = 3.1415927/180.D0
REAL(KIND = 8), PARAMETER :: RADIUS = 2.0000000D-03
INTEGER :: NIJ(NB,2)
INTEGER :: NI, NJ, NK

INTEGER :: I ,J, K
REAL(KIND = 8) :: lz,ly, winkel
REAL(KIND = 8) :: ystart, yend, zstart, zend
WRITE(*,*) "GRIDGEN FÜR EIN EINFACHES O_GRID 28.01.2015"

OPEN(FU,FILE="git.bin",FORM="UNFORMATTED",access="STREAM",STATUS="replace")

WRITE(FU) AXSYM,NB

NIJ(1,1) = NK1 / 2
NIJ(1,2) = NK1 / 2
NIJ(2,1) = NY1 - NK1 / 2
NIJ(2,2) = NK1 / 2
NIJ(3,1) = NY1 - NK1 / 2
NIJ(3,2) = NK1 / 2
!!! BLOCK 1.
WRITE(FU) (NI1+1,(NIJ(I,J)+1,J=1,2),I=1,NB)

write(*,'(3(I4,X))') (NI1,(NIJ(I,J),J=1,2),I=1,NB)
NI = NI1
NJ = NIJ(1,1)
NK = NIJ(1,2)
DO K = 0, NK
   DO J = 0,NJ
      DO I = 0,NI1
         WRITE(FU) RADIUS * DBLE(I)/DBLE(NI),DBLE(J)*RADIUS*0.3D0/DBLE(NJ) &
                  ,DBLE(K)*RADIUS*0.3D0/DBLE(NK)
      END DO
   END DO
END DO


NJ = NIJ(2,1)
NK = NIJ(2,2)
DO K = 0, NK
   DO J = 0,NJ
      winkel =  (45.0D0 * DBLE(K) / DBLE(NK)) * pi

      zstart = DBLE(K) / DBLE(NK) * RADIUS * 0.3D0
      ystart = RADIUS * 0.3D0

      zend   = RADIUS * SIN(WINKEL)
      yend   = RADIUS * COS(WINKEL)

      ly = yend - ystart
      lz = zend - zstart

      DO I = 0,NI1
         WRITE(FU) RADIUS * DBLE(I)/DBLE(NI)         &
                  ,ystart  + DBLE(J)/DBLE(NJ) * ly      &
                  ,zstart  + DBLE(J)/DBLE(NJ) * lz
      END DO
   END DO
END DO

NJ = NIJ(3,1)
NK = NIJ(3,2)
DO K = 0, NK
   DO J = 0,NJ
      winkel =  (45.0D0 + 45.0D0 * DBLE(K) / DBLE(NK)) * pi

      zstart = RADIUS * 0.3D0
      ystart = DBLE(NK-K) / DBLE(NK) * RADIUS * 0.3D0

      zend   = RADIUS * SIN(WINKEL)
      yend   = RADIUS * COS(WINKEL)

      ly = yend - ystart
      lz = zend - zstart

      DO I = 0,NI
         WRITE(FU) RADIUS * DBLE(I)/DBLE(NI)         &
                  ,ystart  + DBLE(J)/DBLE(NJ) * ly      &
                  ,zstart  + DBLE(J)/DBLE(NJ) * lz
      END DO
   END DO
END DO


CLOSE(FU)
WRITE(*,*) "DONE"


end program
