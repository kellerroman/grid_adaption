program gridgen_o_grid
implicit none
INTEGER, PARAMETER :: NI = 10
INTEGER, PARAMETER :: NJ = 10
INTEGER, PARAMETER :: NJ2 = 10
INTEGER, PARAMETER :: FU = 666
INTEGER, PARAMETER :: NB = 6
INTEGER, PARAMETER :: AXSYM = 0
REAL(KIND = 8), PARAMETER :: Pi = 3.1415927/180.D0
REAL(KIND = 8), PARAMETER :: RADIUS = 10.0D0
REAL(KIND = 8), PARAMETER :: KANTENLAENGE = 15.0D0
REAL(KIND = 8), PARAMETER :: EB1 = 5.0D-1
REAL(KIND = 8), PARAMETER :: EB2 = 1.0D0 - EB1

INTEGER :: I ,J
REAL(KIND = 8) :: spktx,spkty,epkty,epktx,lx,ly, winkel
WRITE(*,*) "GRIDGEN FÜR EIN EINFACHES O_GRID_RECTANGLE 28.01.2015"

OPEN(FU,FILE="git.bin",FORM="UNFORMATTED",access="STREAM",STATUS="replace")

WRITE(FU) AXSYM,NB

!!! BLOCK 1.
WRITE(FU) NI+1,NJ+1
!!! BLOCK 2.
WRITE(FU) NI+1,NJ+1
!!! BLOCK 3.
WRITE(FU) NI+1,NJ+1
!!! BLOCK 4.
WRITE(FU) NI+1,NJ2+1
!!! BLOCK 5.
WRITE(FU) NJ2+1,NJ2+1
!!! BLOCK 6.
WRITE(FU) NI+1,NJ2+1

DO J = 0, NJ
   DO I = 0,NI
      WRITE(FU) DBLE(I)*RADIUS*EB1/DBLE(NI) &
                 ,DBLE(J)*RADIUS*EB1/DBLE(NJ)
   END DO
END DO

DO J = 0, NJ
   DO I = 0,NI
      spktx  = 0.0D0
      spkty  = RADIUS  * EB1
      epktx  = RADIUS  * ( EB1 + EB2 * DBLE(j) / DBLE(Nj))
      epkty  = RADIUS
      lx     = epktx
      ly     = epkty   - spkty

      WRITE(FU)  spktx + DBLE(I)/DBLE(NI) * lx &
                ,spkty + DBLE(J)/DBLE(NJ) * ly
   END DO
END DO

DO J = 0, NJ
   DO I = 0,NI
      winkel =  (45.0D0 + 45.0D0 * DBLE(I) / DBLE(NI)) * pi
      spkty = RADIUS  * (EB1 + EB2 * DBLE(j) / DBLE(Nj))! DBLE(NI-I) * RADIUS * 0.3D0 / DBLE(NI)
      spktx = RADIUS * EB1
      epktx = RADIUS !* SIN(WINKEL)
      epkty = 0.0D0!RADIUS * COS(WINKEL)
      IF (I == NI) THEN
         epktx = RADIUS
         epkty = 0.0D0
      END IF
      lx = epktx-spktx
      ly = epkty-spkty

      WRITE(FU) spktx + DBLE(J)/DBLE(NJ) * lx &
               ,spkty + DBLE(I)/DBLE(NI) * ly
   END DO
END DO

DO J = 0, NJ2
   DO I = 0,NI
      WRITE(FU)          DBLE(I) * RADIUS                / DBLE(NI) &
               ,RADIUS + DBLE(J) * (KANTENLAENGE-RADIUS) / DBLE(NJ2)
   END DO
END DO

DO J = 0, NJ2
   DO I = 0,NJ2
      WRITE(FU) RADIUS + DBLE(I) * (KANTENLAENGE-RADIUS) / DBLE(NJ2) &
               ,RADIUS + DBLE(J) * (KANTENLAENGE-RADIUS) / DBLE(NJ2)
   END DO
END DO

DO J = 0, NJ2
   DO I = 0,NI
      WRITE(FU) RADIUS + DBLE(I) * (KANTENLAENGE-RADIUS) / DBLE(NI) &
               ,         DBLE(J) * RADIUS                / DBLE(NJ2)
   END DO
END DO

CLOSE(FU)
WRITE(*,*) "DONE"


end program
