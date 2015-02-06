program gridgen_o_grid
implicit none
INTEGER, PARAMETER :: NK = 16
INTEGER, PARAMETER :: NY = 32
INTEGER, PARAMETER :: FU = 666
INTEGER, PARAMETER :: NB = 3
INTEGER, PARAMETER :: AXSYM = 0
REAL(KIND = 8), PARAMETER :: Pi = 3.1415927/180.D0
REAL(KIND = 8), PARAMETER :: RADIUS = 2.0000000D-03
INTEGER :: NIJ(NB,2)
INTEGER :: NI, NJ

INTEGER :: I ,J
REAL(KIND = 8) :: spktx,spkty,epkty,epktx,lx,ly, winkel
WRITE(*,*) "GRIDGEN FÜR EIN EINFACHES O_GRID 28.01.2015"

OPEN(FU,FILE="git.bin",FORM="UNFORMATTED",access="STREAM",STATUS="replace")

WRITE(FU) AXSYM,NB

NIJ(1,1) = NK / 2
NIJ(1,2) = NK / 2
NIJ(2,1) = NK / 2
NIJ(2,2) = NY - NK / 2
NIJ(3,1) = NK / 2
NIJ(3,2) = NY - NK / 2
!!! BLOCK 1.
WRITE(FU) ((NIJ(I,J)+1,J=1,2),I=1,NB)

write(*,'(2(I4,X))') ((NIJ(I,J),J=1,2),I=1,NB)

NI = NIJ(1,1)
NJ = NIJ(1,2)
DO J = 0, NJ
   DO I = 0,NI
      WRITE(FU) DBLE(I)*RADIUS*0.3D0/DBLE(NI) &
                 ,DBLE(J)*RADIUS*0.3D0/DBLE(NJ)
   END DO
END DO
NI = NIJ(2,1)
NJ = NIJ(2,2)
DO J = 0, NJ
   DO I = 0,NI
      winkel =  45.0D0 * DBLE(I) /DBLE(NI) * pi
      spktx = DBLE(I)*RADIUS*0.3D0/DBLE(NI)
      spkty = RADIUS*0.3D0
      epktx = RADIUS * SIN(WINKEL)
      epkty = RADIUS * COS(WINKEL)
      lx = epktx-spktx
      ly = epkty-spkty
!      IF (J == 0) THEN
!         write(*,*) winkel
!         write(*,*) spktx,spkty
!         write(*,*) epktx,epkty
!         write(*,*) lx,ly,sqrt(lx*lx+ly*ly)
!      END IF
      WRITE(FU) spktx + DBLE(J)/DBLE(NJ) * lx &
                 ,spkty + DBLE(J)/DBLE(NJ) * ly
!      WRITE(FU,*) DBLE(I)*RADIUS*0.5D0/DBLE(NI) &
!                 ,DBLE(J)*RADIUS*0.5D0/DBLE(NJ) + RADIUS*0.5D0
   END DO
END DO

NI = NIJ(3,1)
NJ = NIJ(3,2)
DO J = 0, NJ
   DO I = 0,NI
      winkel =  (45.0D0 + 45.0D0 * DBLE(I) / DBLE(NI)) * pi
      spkty = DBLE(NI-I) * RADIUS * 0.3D0 / DBLE(NI)
      spktx = RADIUS * 0.3D0
      epktx = RADIUS * SIN(WINKEL)
      epkty = RADIUS * COS(WINKEL)
      IF (I == NI) THEN
         epktx = RADIUS
         epkty = 0.0D0
      END IF
      lx = epktx-spktx
      ly = epkty-spkty

!      IF (J == 0) THEN
!         write(*,*) winkel
!         write(*,*) spktx,spkty
!         write(*,*) epktx,epkty
!         write(*,*) lx,ly,sqrt(lx*lx+ly*ly)
!      END IF

      WRITE(FU) spktx + DBLE(J)/DBLE(NJ) * lx &
                 ,spkty + DBLE(J)/DBLE(NJ) * ly
!      WRITE(FU,*) DBLE(J)*RADIUS*0.5D0/DBLE(NJ) + RADIUS*0.5D0&
!                 ,DBLE(NI-I)*RADIUS*0.5D0/DBLE(NI)
   END DO
END DO

CLOSE(FU)
WRITE(*,*) "DONE"


end program
