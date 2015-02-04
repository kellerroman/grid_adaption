SUBROUTINE CALC_SCHIEBESPANNUNG()
   USE MOD_GLOBAL
   USE CONST
   use wall_refinement, only: calc_wall_refinement
   implicit none

   UNSTR%KNT_SPANNUNG = 1.0D-5

!   CALL CALC_1_ORDER_GRADIENT(1)
   CALL ELLIPTIC_GRID_SMOOTHNING()
   ! WANDVERFEINERUNG
   call calc_wall_refinement()
   call cell_inc()

END SUBROUTINE


SUBROUTINE ELLIPTIC_GRID_SMOOTHNING()
   USE MOD_GLOBAL
   IMPLICIT NONE

   INTEGER :: U,P1,P2

#ifdef DEBUG
   IF (GLOBAL%DBG >= 1)  THEN
      WRITE(*,'(A)') "GRID_SMOOTHING"
   END IF
#endif

   IF (GLOBAL % AXSYM == 2) THEN
      WRITE(*,*) "3D NOCH NICHT UNTERSTÜTZT","ELLIPTIC_GRID_SMOOTHNING"
      STOP
   END IF

   DO U = 1, UNSTR % NKNT
      P1 = UNSTR % KNT(U,1)
      P2 = UNSTR % KNT(U,2)
      UNSTR % KNT_SPANNUNG(U,1) = 1.0D0 * ABS( UNSTR % KNT_DN(U,1) )
   END DO

END SUBROUTINE

SUBROUTINE CELL_INC()
!< DIESE ROUTINE SORGT FÜR NICHT ZU GROßE ASPECT RATIOS ZWEITER KANTEN
USE MOD_GLOBAL
IMPLICIT NONE

INTEGER :: P,K,KN,K2

REAL(KIND = 8),allocatable :: winkel(:),lenge(:)
REAL(KIND = 8)             :: MIN_KN
REAL(KIND = 8), PARAMETER :: Pi = 180.D0/3.1415927D0
#ifdef DEBUG
IF (GLOBAL%DBG >= 1)  THEN
   WRITE(*,'(A)') "CELL_INC"
END IF
#endif

IF (GLOBAL % AXSYM == 2) THEN
   WRITE(*,*) "3D NOCH NICHT UNTERSTÜTZT","WANDVERFEINERUNG"
   STOP
END IF

DO P = 1, UNSTR % NPKT
   ALLOCATE ( winkel(UNSTR % PKT_NKNT(P)),lenge(UNSTR % PKT_NKNT(P)))
   DO K = 1, UNSTR % PKT_NKNT(P)
      KN = UNSTR % PKT_KNT(P,K)
      winkel(K) = ATAN(UNSTR % KNT_DN(KN,3)/ UNSTR % KNT_DN(KN,2))+ 360.0D0
      winkel(K) = mod(winkel(K),180.0D0)
      lenge(K)  = UNSTR % KNT_DN(KN,1)
   END DO
   DO K = 1, UNSTR % PKT_NKNT(P)-1s
      DO K1 = K+1,UNSTR % PKT_NKNT(P)
         if (abs(winkel(k) - winkel(k1)) < 45.0D0) then
         !!!! ZEIGEN IN GLEICHE RICHTUNG

            if ( lenge(K) / lenge(K1) < 0.5 ) then

            end if
         end if
      END DO
   END DO
   DEALLOCATE(winkel,lenge)
END DO
END SUBROUTINE

