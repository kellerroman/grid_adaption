program gridgen_nozzle_simple
implicit none
INTEGER, PARAMETER :: NI = 10
INTEGER, PARAMETER :: NJ = 10
INTEGER, PARAMETER :: FU = 666
REAL(KIND = 8), PARAMETER :: Pi = 3.1415927/180.D0
REAL(KIND = 8), PARAMETER :: RADIUS = 10.0D0

INTEGER :: I ,J
REAL(KIND = 8) :: spktx,spkty,epkty,epktx,lx,ly, winkel
WRITE(*,*) "GRIDGEN FÜR EIN EINFACHES O_GRID 08.10.2014"

OPEN(FU,FILE="git.dat")

!!! BLOCK 1.
WRITE(FU,*) NI+1,NJ+1

DO J = 0, NJ
   DO I = 0,NI
      WRITE(FU,*) DBLE(I)*RADIUS*0.3D0/DBLE(NI) &
                 ,DBLE(J)*RADIUS*0.3D0/DBLE(NJ)
   END DO
END DO
!!! BLOCK 2.
WRITE(FU,*) NI+1,NJ+1

DO J = 0, NJ
   DO I = 0,NI
      winkel =  45.0D0 * DBLE(I) /DBLE(NI) * pi
      spktx = DBLE(I)*RADIUS*0.3D0/DBLE(NI)
      spkty = RADIUS*0.3D0
      epktx = RADIUS * SIN(WINKEL)
      epkty = RADIUS * COS(WINKEL)
      lx = epktx-spktx
      ly = epkty-spkty
      IF (J == 0) THEN
         write(*,*) winkel
         write(*,*) spktx,spkty
         write(*,*) epktx,epkty
         write(*,*) lx,ly,sqrt(lx*lx+ly*ly)
      END IF
      WRITE(FU,*) spktx + DBLE(J)/DBLE(NJ) * lx &
                 ,spkty + DBLE(J)/DBLE(NJ) * ly
!      WRITE(FU,*) DBLE(I)*RADIUS*0.5D0/DBLE(NI) &
!                 ,DBLE(J)*RADIUS*0.5D0/DBLE(NJ) + RADIUS*0.5D0
   END DO
END DO
!!! BLOCK 3.
WRITE(FU,*) NI+1,NJ+1

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

      IF (J == 0) THEN
         write(*,*) winkel
         write(*,*) spktx,spkty
         write(*,*) epktx,epkty
         write(*,*) lx,ly,sqrt(lx*lx+ly*ly)
      END IF

      WRITE(FU,*) spktx + DBLE(J)/DBLE(NJ) * lx &
                 ,spkty + DBLE(J)/DBLE(NJ) * ly
!      WRITE(FU,*) DBLE(J)*RADIUS*0.5D0/DBLE(NJ) + RADIUS*0.5D0&
!                 ,DBLE(NI-I)*RADIUS*0.5D0/DBLE(NI)
   END DO
END DO

CLOSE(FU)
WRITE(*,*) "DONE"


end program gridgen_nozzle_simple
