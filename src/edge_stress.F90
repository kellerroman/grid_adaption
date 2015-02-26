module edge_stress
use const, only: dp
implicit none
   private
   real(kind = dp), allocatable              :: CELL_INC_STRESS(:)
   !< Spannungsbetrag für das Edge Stretch Ratio
   real(kind = dp), allocatable              :: CELL_EDGE_LENGTH_STRESS(:)
   !< Spannungsbetrag für den Ausgleich der Zellkantenlängen
   integer, allocatable, save                :: KNT_NNEIGH(:)
   !< Anzahl der Relevanten Edges für den Größenvergleich
   integer, allocatable, save                :: KNT_NEIGH(:,:)
   !< IDs der Edges mit deren Größe die Aktuelle Edge verglichen werdne soll
   integer, allocatable, save                :: cell_edges(:,:)
   !< ID der Kanten pro Zelle 1-2 und 3-4 gehören zusammen


   real(kind = dp),parameter                 :: cell_edge_force_inc = 1.01E0_dp
   real(kind = dp),parameter                 :: cell_edge_force_dec = 0.99E0_dp


   real(kind = dp), parameter                :: stress_max = 1E20_DP
   real(kind = dp), parameter                :: seitenver  = 1.4E0_DP
   public CALC_EDGE_STRESSES
   public INIT_EDGE_STRETCH
contains
   SUBROUTINE CALC_EDGE_STRESSES(STRESSES_SUm   )
      USE MOD_GLOBAL
      USE CONST
      use wall_refinement, only: calc_wall_refinement
      implicit none
      real(kind = dp),intent(out) :: STRESSES_SUM
      integer :: i
      logical :: conv_prob
      integer :: conv_prob_count

      UNSTR%KNT_SPANNUNG(:,2) = UNSTR%KNT_SPANNUNG(:,1)

      CALL ELLIPTIC_GRID_SMOOTHNING()

      CALL CALC_EDGE_STRETCH_STRESSES()

      call calc_cell_edge_length_diff

      call calc_wall_refinement()
      conv_prob = .FALSE.
      conv_prob_count = 0
      STRESSES_SUM = 0.0e0_dp
      do i = 1, UNSTR % NKNT
         STRESSES_SUM = STRESSES_SUM + UNSTR % KNT_SPANNUNG(i,1)
         if (UNSTR % KNT_SPANNUNG(i,1) > stress_max) then
   !         write(*,*) i,UNSTR % KNT_SPANNUNG(i,1)
            UNSTR % KNT_SPANNUNG(i,1) = stress_max
            conv_prob = .TRUE.
            conv_prob_count = conv_prob_count + 1
         end if
      end do
      if (conv_prob .and. iteration_output) then
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
!         UNSTR % KNT_SPANNUNG(U,1) = 1.0D-10
         UNSTR % KNT_SPANNUNG(U,1) = 1.0D0 * ABS( UNSTR % KNT_DN(U,1) )
      END DO

   END SUBROUTINE

   SUBROUTINE CALC_EDGE_STRETCH_STRESSES()
   !< DIESE ROUTINE SORGT FÜR NICHT ZU GROßE STRETCH RATIOS ZWEITER KANTEN
   USE MOD_GLOBAL
   IMPLICIT NONE

   integer :: e

   INTEGER :: P,K,K1,KN,KN1
   real(kind = dp) :: length_ne,length
!   REAL(KIND = 8),allocatable :: winkel(:),lenge(:)
!   REAL(KIND = 8), PARAMETER :: Pi = 180.D0/3.1415927D0

#ifdef DEBUG
   IF (GLOBAL%DBG >= 1)  THEN
      WRITE(*,'(A)') "CELL_INC"
   END IF
#endif

   IF (GLOBAL % AXSYM == 2) THEN
      WRITE(*,*) "3D NOCH NICHT UNTERSTÜTZT","CALC_EDGE_STRETCH_STRESSES"
      STOP
   END IF
   do e = 1, UNSTR % NKNT
      length = UNSTR % KNT_DN(e,1)
      length_ne = 1E5_dp
      do k = 1,knt_nneigh(e)
         kn = knt_neigh(e,k)
         length_ne = min(length_ne, UNSTR % KNT_DN(kn,1) )
      end do
      if (length / length_ne > seitenver) then
         CELL_INC_STRESS(e) = MAX(1E-5_dp,CELL_INC_STRESS(e)) * 1.01E0_dp
      else if ( length / length_ne > seitenver*0.9E0_dp ) then
         ! Zur stabilität des Verfahren ist ein gewisser Korridor notwendig
         ! wenn diese Zeile auskommentiert ist, fängt das Gitter an zu schwingen
      else if ( length > length_ne ) then
         CELL_INC_STRESS(e) = CELL_INC_STRESS(e) * 0.95E0_dp
!         write(*,*) "INK",e,length,length_ne
      else
         CELL_INC_STRESS(e) = 0.0E0_dp
      end if
      UNSTR % KNT_SPANNUNG(e,1)  = UNSTR % KNT_SPANNUNG(e,1) + CELL_INC_STRESS(e)
   end do
!   DO P = 1, UNSTR % NPKT
!      ALLOCATE ( winkel(UNSTR % PKT_NKNT(P)),lenge(UNSTR % PKT_NKNT(P)))
!      DO K = 1, UNSTR % PKT_NKNT(P)
!         KN = UNSTR % PKT_KNT(P,K)
!         winkel(K) = ATAN(UNSTR % KNT_DN(KN,3)/ UNSTR % KNT_DN(KN,2)) * PI + 360.0D0
!         winkel(K) = mod(winkel(K),180.0D0)
!         lenge(K)  = UNSTR % KNT_DN(KN,1)
!      END DO
!      DO K = 1, UNSTR % PKT_NKNT(P)-1
!         DO K1 = K+1,UNSTR % PKT_NKNT(P)
!            KN                           = UNSTR % PKT_KNT(P,K)
!            KN1                          = UNSTR % PKT_KNT(P,K1)
!            if (abs(winkel(k) - winkel(k1)) < 25.0D0) then
!            !!!! ZEIGEN IN GLEICHE RICHTUNG
!
!               if ( lenge(K1) / lenge(K) > seitenver ) then
!   !                write(*,*) P,K,K1,lenge(K),lenge(K1)
!   !                write(*,*) winkel(k),winkel(k1)
!   !               UNSTR%CELL_INC_STRESS(KN1)   = UNSTR%KNT_SPANNUNG(KN,2) * 1.0D0/seitenver
!                  CELL_INC_STRESS(KN1)   = MAX(1D-5,CELL_INC_STRESS(KN1)) * 1.01D0
!               else if ( lenge(K1) / lenge(K) > seitenver*0.9D0 ) then
!               else if (lenge(K1) >= lenge(K) ) then
!   !               UNSTR%CELL_INC_STRESS(KN1)   = UNSTR%CELL_INC_STRESS(KN1) * 0.95D0
!               else
!   !               UNSTR%CELL_INC_STRESS(KN1)   = 0.0D0
!               end if
!
!               if ( lenge(K) / lenge(K1) > seitenver ) then
!   !                write(*,*) P,K,K1,lenge(K),lenge(K1)
!   !                write(*,*) winkel(k),winkel(k1)
!   !               UNSTR%CELL_INC_STRESS(KN1)   = UNSTR%KNT_SPANNUNG(KN,2) * 1.0D0/seitenver
!                  CELL_INC_STRESS(KN)   = MAX(1D-5,CELL_INC_STRESS(KN)) * 1.01D0
!               else if ( lenge(K) / lenge(K1) > seitenver*0.9D0 ) then
!               else if (lenge(K) >= lenge(K1) ) then
!   !               UNSTR%CELL_INC_STRESS(KN)   = UNSTR%CELL_INC_STRESS(KN) * 0.95D0
!               else
!   !               UNSTR%CELL_INC_STRESS(KN)   = 0.0D0
!               end if
!
!
!            end if
!            UNSTR % KNT_SPANNUNG(KN1,1)  = UNSTR % KNT_SPANNUNG(KN1,1) + CELL_INC_STRESS(KN1)
!            UNSTR % KNT_SPANNUNG(KN,1)  = UNSTR % KNT_SPANNUNG(KN,1) + CELL_INC_STRESS(KN)
!         END DO
!      END DO
!
!      DEALLOCATE(winkel,lenge)
!   END DO
   END SUBROUTINE CALC_EDGE_STRETCH_STRESSES

   SUBROUTINE CALC_CELL_EDGE_LENGTH_DIFF
   use mod_global
   implicit none
   integer :: i,j,k,b,nc,es
   integer :: e1,e2
   nc = 0
   block_loop: do b = 1, global % nblock
      k_loop: do k = 1, blocks(b) % nck
         j_loop: do j = 1, blocks(b) % ncj
            i_loop: do i = 1, blocks(b) % nci
               nc = nc + 1
               do es = 1,3,2 !!! first do 1&2 then 3&4
                  e1 = cell_edges(nc,es)
                  e2 = cell_edges(nc,es+1)
                  if (unstr % knt_dn(e1,1) > 1.3E0_dp * unstr % knt_dn(e2,1) ) then
                     CELL_EDGE_LENGTH_STRESS(e1) = MAX(1E-5_dp,CELL_EDGE_LENGTH_STRESS(e1)) * cell_edge_force_inc
                     CELL_EDGE_LENGTH_STRESS(e2) = CELL_EDGE_LENGTH_STRESS(e2) * cell_edge_force_dec
                  else if (unstr % knt_dn(e2,1) > 1.3E0_dp * unstr % knt_dn(e1,1) ) then
                     CELL_EDGE_LENGTH_STRESS(e1) = CELL_EDGE_LENGTH_STRESS(e1) * cell_edge_force_dec
                     CELL_EDGE_LENGTH_STRESS(e2) = MAX(1E-5_dp,CELL_EDGE_LENGTH_STRESS(e2)) * cell_edge_force_inc
                  else
                     CELL_EDGE_LENGTH_STRESS(e1) = CELL_EDGE_LENGTH_STRESS(e1) * cell_edge_force_dec
                     CELL_EDGE_LENGTH_STRESS(e2) = CELL_EDGE_LENGTH_STRESS(e2) * cell_edge_force_dec
                  end if
               end do
            end do i_loop
         end do j_loop
      end do k_loop
   end do block_loop
   do e1 = 1, unstr % nknt
      UNSTR % KNT_SPANNUNG(e1,1)  = UNSTR % KNT_SPANNUNG(e1,1) + CELL_EDGE_LENGTH_STRESS(e1)
   end do

   END SUBROUTINE CALC_CELL_EDGE_LENGTH_DIFF



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
   logical :: allready_connected
   ALLOCATE( CELL_INC_STRESS(UNSTR%NKNT))
   CELL_INC_STRESS = 1.0D-10

   allocate( knt_nneigh ( UNSTR % NKNT   ) )
   allocate( knt_neigh  ( UNSTR % NKNT ,3) )
   ! INITIALISERUNG DER ANZAHL DER RELEVANTEN KNOTEN MIT 0
   KNT_NNEIGH = 0

   !!! ES WIRD ÜBER BLOCKGRENZEN IMMER NUR "ZURÜCK" nicht nach vorne geschaut
   !!! um doppelte referenzen zu verhindern
   !!! d.h. schlefen laufen bis NCX
   !!! I/J/K + 1 ist unkritisch
   block_loop: do b = 1, GLOBAL % NBLOCK
      kdir_loop: do k = 1, BLOCKS(B) % NPK
         jdir_loop: do j = 1, BLOCKS(B) % NPJ
            idir_loop: do i = 1, BLOCKS(B) % NPI
               dir_loop: do dir = 1,2
                  if (dir == 1) then !!!!!! I-DIR
                     if (i == BLOCKS(B) % NPI) cycle dir_loop
                     if (i == 1) then
                        if (BLOCKS(B) % BLOCK_CONNECTION(1,1) > 0) then
                           b2 = BLOCKS(B) % BLOCK_CONNECTION(1,1)
                           if  (BLOCKS(B) % BLOCK_CONNECTION(1,2) == 2 &
                           .AND.BLOCKS(B) % BLOCK_CONNECTION(1,3) == 1) then
                              i2 = BLOCKS(B2) % NCI !!NPI -1
                              j2 = j
                              k2 = k
                           else if  (BLOCKS(B) % BLOCK_CONNECTION(1,2) == 4 &
                           .AND.BLOCKS(B) % BLOCK_CONNECTION(1,3) == 2) then
                              i2 = BLOCKS(B2) % NPI - j + 1 !!NPI -1
                              j2 = BLOCKS(B2) % NCJ
                              k2 = k
                           else
                              write(*,'(5(A,X,I0,X))') "Block i-Dir",B &
                                        ,"to",B2   &
                                        ,"@FACE:",BLOCKS(B) % BLOCK_CONNECTION(1,2) &
                                        ,"MUTATION",BLOCKS(B) % BLOCK_CONNECTION(1,3)

                              stop "ERROR in INIT_EDGE_STRETCH: Phase-Con not impl"
                           end if
                        else
                           cycle dir_loop !! kein block angeschlossen
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
                     if (j == BLOCKS(B) % NPJ) cycle dir_loop
                     if (j == 1) then
                        if (BLOCKS(B) % BLOCK_CONNECTION(3,1) > 0) then
                           b2 = BLOCKS(B) % BLOCK_CONNECTION(3,1)
                           if  (BLOCKS(B) % BLOCK_CONNECTION(3,2) == 4 &
                           .AND.BLOCKS(B) % BLOCK_CONNECTION(3,3) == 1) then
                              i2 = i
                              j2 = BLOCKS(B2) % NCJ !!NPJ -1
                              k2 = k
                           else if  (BLOCKS(B) % BLOCK_CONNECTION(3,2) == 2 &
                           .AND.BLOCKS(B) % BLOCK_CONNECTION(3,3) == 2) then
                              i2 = BLOCKS(B2) % NCI
                              j2 = BLOCKS(B2) % NPJ - I + 1
                              k2 = k
                           else
                              write(*,'(4(A,X,I0,X))') "Block j-Dir",B &
                                        ,"to",B2   &
                                        ,"@FACE:",BLOCKS(B) % BLOCK_CONNECTION(3,2) &
                                        ,"MUTATION",BLOCKS(B) % BLOCK_CONNECTION(3,3)

                              STOP "ERROR in INIT_EDGE_STRETCH: Phase-Con not impl"
                           end if
                        else
                           cycle dir_loop
                        end if
                     else
                        b2 = b
                        i2 = i
                        j2 = j - 1
                        k2 = k
                     end if

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

                     allready_connected = .FALSE.
                     do ep = 1,knt_nneigh(e1)
                        e = knt_neigh(e1, ep)
                        if( e == e2 ) then
                           allready_connected = .TRUE.
                           exit
                        end if
                     end do

                     if (.NOT. allready_connected) then
                        knt_nneigh(e1) = knt_nneigh(e1) + 1
                        knt_nneigh(e2) = knt_nneigh(e2) + 1

                        if (knt_nneigh(e1) > 3) then

                           write(*,*) "@PKT",UNSTR%PKT_REF(p,1:4)
                           write(*,*) "Edge     1",e1,"from",UNSTR%PKT_REF(UNSTR%KNT(e1,1),1:4) &
                                     ,"to",UNSTR%PKT_REF(UNSTR%KNT(e1,2),1:4)

                           write(*,*) "NeighEdge1",knt_neigh(e1,1),"from",UNSTR%PKT_REF(UNSTR%KNT(knt_neigh(e1,1),1),1:4) &
                                     ,"to",UNSTR%PKT_REF(UNSTR%KNT(knt_neigh(e1,1),2),1:4)


                           write(*,*) "NeighEdge2",knt_neigh(e1,2),"from",UNSTR%PKT_REF(UNSTR%KNT(knt_neigh(e1,2),1),1:4) &
                                     ,"to",UNSTR%PKT_REF(UNSTR%KNT(knt_neigh(e1,2),2),1:4)


                           write(*,*) "NeighEdge3",knt_neigh(e1,3),"from",UNSTR%PKT_REF(UNSTR%KNT(knt_neigh(e1,3),1),1:4) &
                                     ,"to",UNSTR%PKT_REF(UNSTR%KNT(knt_neigh(e1,3),2),1:4)
                           write(*,*) "New  Edge1",e2,"from",UNSTR%PKT_REF(UNSTR%KNT(e2,1),1:4) &
                                     ,"to",UNSTR%PKT_REF(UNSTR%KNT(e2,2),1:4)
                           stop
                        end if
                        if (knt_nneigh(e2) > 3) then
                           write(*,*) "E2",knt_nneigh(e2), p,p2,p3,e2, e1
                           stop
                        end if

                        knt_neigh(e1, knt_nneigh(e1)) = e2
                        knt_neigh(e2, knt_nneigh(e2)) = e1
                     end if
                  else
                     write(*,*)
                     STOP "ERROR in INIT_EDGE_STRETCH: could not find egdes"
                  end if
               end do dir_loop
            end do idir_loop
         end do jdir_loop
      end do kdir_loop
   end do block_loop
   if (MINVAL(knt_nneigh) == 0) then
      write(*,'(14(A5,X))') "KNT", "P1","P2"&
                           ,"P1_I","P1_J","P1_K","P1_B"&
                           ,"P2_I","P2_J","P2_K","P2_B"
      do e = 1, UNSTR % NKNT
         if (knt_nneigh(e) > 0) cycle
         write(*,'(14(I5,X))') e,UNSTR%KNT(e,:),knt_neigh(e,:knt_nneigh(e))&
                              ,UNSTR%PKT_REF(UNSTR%KNT(e,1),1:4)&
                              ,UNSTR%PKT_REF(UNSTR%KNT(e,2),1:4)
      end do
   end if
   call init_cell_edge_length_diff
   END SUBROUTINE INIT_EDGE_STRETCH
   SUBROUTINE INIT_CELL_EDGE_LENGTH_DIFF
   use mod_global
   implicit none
   integer :: i,j,k,b, ncell,nei

   integer :: p1,p2,p3,p4

   ncell = 0
   do b = 1, global % nblock
      ncell = ncell + blocks(b) % nci * blocks(b) % ncj * blocks(b) % nck
   end do
   allocate (cell_edges(ncell,4))
   allocate (cell_edge_length_stress(unstr % nknt))
   CELL_EDGE_LENGTH_STRESS = 0.0E0_DP
   ncell = 0
   block_loop: do b = 1, global % nblock
      k_loop: do k = 1, blocks(b) % nck
         j_loop: do j = 1, blocks(b) % ncj
            i_loop: do i = 1, blocks(b) % nci
               ncell = ncell + 1
               p1 = blocks(b) % assoc(i  ,j  ,k)
               p2 = blocks(b) % assoc(i+1,j  ,k)
               p3 = blocks(b) % assoc(i  ,j+1,k)
               p4 = blocks(b) % assoc(i+1,j+1,k)
               do nei = 1, unstr % PKT_nknt(p1)
                  if (unstr % pkt_neigh(p1,nei) == p2) then
                     cell_edges(ncell,1) = unstr % pkt_knt(p1,nei)
                  else if (unstr % pkt_neigh(p1,nei) == p3) then
                     cell_edges(ncell,3) = unstr % pkt_knt(p1,nei)
                  end if
               end do
               do nei = 1, unstr % PKT_nknt(p4)
                  if (unstr % pkt_neigh(p4,nei) == p2) then
                     cell_edges(ncell,4) = unstr % pkt_knt(p4,nei)
                  else if (unstr % pkt_neigh(p4,nei) == p3) then
                     cell_edges(ncell,2) = unstr % pkt_knt(p4,nei)
                  end if
               end do
            end do i_loop
         end do j_loop
      end do k_loop
   end do block_loop


   END SUBROUTINE INIT_CELL_EDGE_LENGTH_DIFF
end module edge_stress
