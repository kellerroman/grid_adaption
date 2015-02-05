module edge_stress
use const, only: dp
implicit none
   private
   real(kind = dp), allocatable              :: CELL_INC_STRESS(:)
   !< Spannungsbetrag für das Edge Stretch Ratio
   integer, allocatable, save                :: KNT_NNEIGH(:)
   !< Anzahl der Relevanten Edges für den Größenvergleich
   integer, allocatable, save                :: KNT_NEIGH(:,:)
   !< IDs der Edges mit deren Größe die Aktuelle Edge verglichen werdne soll
   public CALC_EDGE_STRESSES
   public INIT_EDGE_STRETCH
contains
   SUBROUTINE CALC_EDGE_STRESSES()
      USE MOD_GLOBAL
      USE CONST
      use wall_refinement, only: calc_wall_refinement
      implicit none
      integer :: i
      logical :: conv_prob
      integer :: conv_prob_count

      UNSTR%KNT_SPANNUNG(:,2) = UNSTR%KNT_SPANNUNG(:,1)

      CALL ELLIPTIC_GRID_SMOOTHNING()

      CALL CALC_EDGE_STRETCH_STRESSES()

      call calc_wall_refinement()
      conv_prob = .FALSE.
      conv_prob_count = 0
      do i = 1, UNSTR % NKNT
         if (UNSTR % KNT_SPANNUNG(i,1) > 1.0D5) then
   !         write(*,*) i,UNSTR % KNT_SPANNUNG(i,1)
            UNSTR % KNT_SPANNUNG(i,1) = 1.0D5
            conv_prob = .TRUE.
            conv_prob_count = conv_prob_count + 1
         end if
      end do
      if (conv_prob) then
         write(*,*) "WARNING in calc_edge_stresses"
         write(*,*) "Convergence Problems: Edge Stress too high in",conv_prob_count,"Cells"
         write(*,*) "Limited to EDGE_STRESS_MAX = 1.0D5"
      end if
   END SUBROUTINE CALC_EDGE_STRESSES

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

   SUBROUTINE CALC_EDGE_STRETCH_STRESSES()
   !< DIESE ROUTINE SORGT FÜR NICHT ZU GROßE STRETCH RATIOS ZWEITER KANTEN
   USE MOD_GLOBAL
   IMPLICIT NONE

   INTEGER :: P,K,K1,KN,KN1

   REAL(KIND = 8),allocatable :: winkel(:),lenge(:)
   REAL(KIND = 8), PARAMETER :: Pi = 180.D0/3.1415927D0
   REAL(KIND = 8), PARAMETER :: seitenver = 1.2D0
#ifdef DEBUG
   IF (GLOBAL%DBG >= 1)  THEN
      WRITE(*,'(A)') "CELL_INC"
   END IF
#endif

   IF (GLOBAL % AXSYM == 2) THEN
      WRITE(*,*) "3D NOCH NICHT UNTERSTÜTZT","CALC_EDGE_STRETCH_STRESSES"
      STOP
   END IF

   DO P = 1, UNSTR % NPKT
      ALLOCATE ( winkel(UNSTR % PKT_NKNT(P)),lenge(UNSTR % PKT_NKNT(P)))
      DO K = 1, UNSTR % PKT_NKNT(P)
         KN = UNSTR % PKT_KNT(P,K)
         winkel(K) = ATAN(UNSTR % KNT_DN(KN,3)/ UNSTR % KNT_DN(KN,2)) * PI + 360.0D0
         winkel(K) = mod(winkel(K),180.0D0)
         lenge(K)  = UNSTR % KNT_DN(KN,1)
      END DO
      DO K = 1, UNSTR % PKT_NKNT(P)-1
         DO K1 = K+1,UNSTR % PKT_NKNT(P)
            KN                           = UNSTR % PKT_KNT(P,K)
            KN1                          = UNSTR % PKT_KNT(P,K1)
            if (abs(winkel(k) - winkel(k1)) < 25.0D0) then
            !!!! ZEIGEN IN GLEICHE RICHTUNG

               if ( lenge(K1) / lenge(K) > seitenver ) then
   !                write(*,*) P,K,K1,lenge(K),lenge(K1)
   !                write(*,*) winkel(k),winkel(k1)
   !               UNSTR%CELL_INC_STRESS(KN1)   = UNSTR%KNT_SPANNUNG(KN,2) * 1.0D0/seitenver
                  CELL_INC_STRESS(KN1)   = MAX(1D-5,CELL_INC_STRESS(KN1)) * 1.01D0
               else if ( lenge(K1) / lenge(K) > seitenver*0.9D0 ) then
               else if (lenge(K1) >= lenge(K) ) then
   !               UNSTR%CELL_INC_STRESS(KN1)   = UNSTR%CELL_INC_STRESS(KN1) * 0.95D0
               else
   !               UNSTR%CELL_INC_STRESS(KN1)   = 0.0D0
               end if

               if ( lenge(K) / lenge(K1) > seitenver ) then
   !                write(*,*) P,K,K1,lenge(K),lenge(K1)
   !                write(*,*) winkel(k),winkel(k1)
   !               UNSTR%CELL_INC_STRESS(KN1)   = UNSTR%KNT_SPANNUNG(KN,2) * 1.0D0/seitenver
                  CELL_INC_STRESS(KN)   = MAX(1D-5,CELL_INC_STRESS(KN)) * 1.01D0
               else if ( lenge(K) / lenge(K1) > seitenver*0.9D0 ) then
               else if (lenge(K) >= lenge(K1) ) then
   !               UNSTR%CELL_INC_STRESS(KN)   = UNSTR%CELL_INC_STRESS(KN) * 0.95D0
               else
   !               UNSTR%CELL_INC_STRESS(KN)   = 0.0D0
               end if


            end if
            UNSTR % KNT_SPANNUNG(KN1,1)  = UNSTR % KNT_SPANNUNG(KN1,1) + CELL_INC_STRESS(KN1)
            UNSTR % KNT_SPANNUNG(KN,1)  = UNSTR % KNT_SPANNUNG(KN,1) + CELL_INC_STRESS(KN)
         END DO
      END DO

      DEALLOCATE(winkel,lenge)
   END DO
   END SUBROUTINE CALC_EDGE_STRETCH_STRESSES
   SUBROUTINE INIT_EDGE_STRETCH
   ! INITIALISATION FOR THE EDGE STRETCH CONTROL ROUTINE
   ! ALLOCATION OF ARRAYS CELL_INC_STRESS and KNT_NEIGH
   !
   use mod_global, only: unstr
   implicit none
   ALLOCATE( CELL_INC_STRESS(UNSTR%NKNT))
   CELL_INC_STRESS = 1.0D-10

   ALLOCATE( KNT_NNEIGH ( UNSTR % NKNT   ) )
   ALLOCATE( KNT_NEIGH  ( UNSTR % NKNT ,2) )
   ! INITIALISERUNG DER ANZAHL DER RELEVANTEN KNOTEN MIT 0
   KNT_NNEIGH = 0






   END SUBROUTINE INIT_EDGE_STRETCH
end module edge_stress
