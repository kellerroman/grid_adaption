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
   real(kind = dp), parameter                :: stress_max = 1E8_DP
   real(kind = dp), parameter                :: seitenver  = 1.3E0_DP
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
         if (UNSTR % KNT_SPANNUNG(i,1) > stress_max) then
   !         write(*,*) i,UNSTR % KNT_SPANNUNG(i,1)
            UNSTR % KNT_SPANNUNG(i,1) = stress_max
            conv_prob = .TRUE.
            conv_prob_count = conv_prob_count + 1
         end if
      end do
      if (conv_prob) then
         write(*,'(A)') "WARNING in calc_edge_stresses"
         write(*,'(A,X,I0,X,A)') "Convergence Problems: Edge Stress too high in",conv_prob_count,"Cells"
         write(*,'(A,X,ES7.1)') "Limited to EDGE_STRESS_MAX =",stress_max
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
   use mod_global, only: unstr, global, blocks
   implicit none

   integer :: dir,ep
   integer :: b,i,j,k,p,nk
   integer :: b2,i2,j2,k2,p2
   integer :: b3,i3,j3,k3,p3
   integer :: e,e1,e2
   ALLOCATE( CELL_INC_STRESS(UNSTR%NKNT))
   CELL_INC_STRESS = 1.0D-10

   allocate( knt_nneigh ( UNSTR % NKNT   ) )
   allocate( knt_neigh  ( UNSTR % NKNT ,2) )
   ! INITIALISERUNG DER ANZAHL DER RELEVANTEN KNOTEN MIT 0
   KNT_NNEIGH = 0
!   goto 666
   block_loop: do b = 1, GLOBAL % NBLOCK
      kdir_loop: do k = 1, BLOCKS(B) % NPK
         jdir_loop: do j = 1, BLOCKS(B) % NPJ
            idir_loop: do i = 1, BLOCKS(B) % NCI
               dir_loop: do dir = 1,2
                  if (dir == 1) then !!!!!! I-DIR
                     if (i == 1) then
                        if (BLOCKS(B) % BLOCK_CONNECTION(1,1) > 0) then
                           b2 = BLOCKS(B) % BLOCK_CONNECTION(1,1)
                           if  (BLOCKS(B) % BLOCK_CONNECTION(1,2) == 1 &
                           .AND.BLOCKS(B) % BLOCK_CONNECTION(1,3) == 1) then
                              i2 = BLOCKS(B2) % NPI
                              j2 = j
                              k2 = k
                           else
                              write(*,'(4(A,X,I0,X))') "Block",B &
                                        ,"to",B2   &
                                        ,"@FACE:",BLOCKS(B) % BLOCK_CONNECTION(1,2) &
                                        ,"MUTATION",BLOCKS(B) % BLOCK_CONNECTION(1,3)

                              STOP "ERROR in INIT_EDGE_STRETCH: Phase-Con not impl"
                           end if
                        else
                           cycle dir_loop
                        end if
                     else
                        b2 = b
                        i2 = i - 1
                        j2 = j
                        k2 = k
                     end if
                     b3 = b
                     i3 = i + 1
                     j3 = j
                     k3 = k
                  else if (dir == 2) then
                     if (j == 1) cycle dir_loop
                     b2 = b
                     i2 = i
                     j2 = j - 1
                     k2 = k

                     b3 = b
                     i3 = i
                     j3 = j + 1
                     k3 = k
                  else
                     STOP "INIT_EDGE_STRETCH 3D not implemented yet"
                  end if
                  p  = BLOCKS(b ) % ASSOC(i ,j ,k )
                  p2 = BLOCKS(b2) % ASSOC(i2,j2,k2)
                  p3 = BLOCKS(b3) % ASSOC(i3,j3,k3)
!                  write(*,*) dir,p,p2,p3
                  nk = UNSTR % PKT_NKNT(p)
                  e1 = -1
                  e2 = -1
                  do ep = 1, nk
                     e = UNSTR % PKT_KNT(p,ep)
                     if (UNSTR % KNT(e,1) == p2 .or. UNSTR % KNT(e,2) == p2) then
                        e1 = e
                     end if
                     if (UNSTR % KNT(e,1) == p3 .or. UNSTR % KNT(e,2) == p3) then
                        e2 = e
                     end if
                  end do
                  if (e1 /= -1 .and. e2 /= -1) then
                     knt_nneigh(e1) = knt_nneigh(e1) + 1
                     knt_nneigh(e2) = knt_nneigh(e2) + 1

                     knt_neigh(e1, knt_nneigh(e1)) = e2
                     knt_neigh(e2, knt_nneigh(e2)) = e1
                  else
                     STOP "ERROR in INIT_EDGE_STRETCH: could not find egdes"
                  end if
               end do dir_loop
            end do idir_loop
         end do jdir_loop
      end do kdir_loop
   end do block_loop
   write(*,'(5(A5,X))') "KNT", "P1","P2","NEI1","NEI2"
   do e = 1, UNSTR % NKNT
      write(*,'(5(I5,X))') e,UNSTR%KNT(e,:),knt_neigh(e,:knt_nneigh(e))
   end do
   stop
666  continue
   END SUBROUTINE INIT_EDGE_STRETCH
end module edge_stress
