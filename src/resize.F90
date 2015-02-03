SUBROUTINE RESIZE_GRID(DN_SUM,DN_MAX,DN_MAX_POS)
USE MOD_GLOBAL

IMPLICIT NONE

!! PARAMETER
REAL(KIND=8)         :: DN_SUM,DN_MAX
INTEGER              :: DN_MAX_POS

INTEGER :: DIM, U, K, KN, P1

INTEGER :: I, J, N, UP


REAL(KIND=8) :: teilen
!REAL(KIND=8) :: which(2,4)
#ifdef DEBUG
IF (GLOBAL%DBG == 1)  THEN
   WRITE(*,'(A)') "ABMESSUNGEN DES GITTERS WERDEN GEAENDERT"
END IF
#endif

IF (GLOBAL % AXSYM == 2) THEN
   WRITE(*,*) "3D NOCH NICHT UNTERSTÜTZT","unstrukt"
   STOP
END IF





IF ( GLOBAL % NODE_MOVEMENT_MECHNISM == 2) THEN
   UNSTR%PKT_DN = 0.0D+00
   DO DIM = 1,2
      DO U = 1,UNSTR % NPKT
         teilen = 0.0D+00
         !! ÜBERSPRINGEN WENN DER PUNKT IN DIESE RICHTUNG SOWIESO NICHT BEWEGT WIRD
         if (UNSTR % PKT_TYPE(U) == 2 &
         .OR.UNSTR % PKT_TYPE(U) == 5-dim) cycle

         DO K = 1,UNSTR % PKT_NKNT(U)
            !KANTE
            KN = UNSTR % PKT_KNT(U,K)
            !NACHBARPUNKT
            P1 = UNSTR % PKT_NEIGH(U,K)

            ! RICHTUNG DER SPANNUNG FESTSTELLEN
            teilen = unstr % xyz(p1,dim) - unstr % xyz(u,dim)
            teilen = SIGN(UNSTR % KNT_DN(KN,1),teilen)

            UNSTR%PKT_DN(U,DIM) = UNSTR%PKT_DN(U,DIM)                                                                  &
                      + ABS( UNSTR % KNT_SPANNUNG(KN,1) * UNSTR % KNT_DN(KN,DIM+1) )               &
                      / teilen
         END DO
      END DO
   END DO
ELSE ! GLOBAL % NODE_MOVEMENT
   UNSTR%PKT_DN = 0.0D+00

   DO DIM = 1,2
      DO U = 1,UNSTR % NPKT
         teilen = 0.0D+00

         !! ÜBERSPRINGEN WENN DER PUNKT IN DIESE RICHTUNG SOWIESO NICHT BEWEGT WIRD
         if (UNSTR % PKT_TYPE(U) == 2 &
         .OR.UNSTR % PKT_TYPE(U) == 5-dim) cycle

         DO K = 1,UNSTR % PKT_NKNT(U)
            !KANTE
            KN = UNSTR % PKT_KNT(U,K)
            !NACHBARPUNKT
            P1 = UNSTR % PKT_NEIGH(U,K)
   !         UNSTR%PKT_DN(U,DIM) = UNSTR%PKT_DN(U,DIM) + UNSTR%KNT_SPANNUNG(KN,DIM+1) * UNSTR % XYZ(P1,DIM)
            UNSTR%PKT_DN(U,DIM) = UNSTR%PKT_DN(U,DIM) + UNSTR%KNT_SPANNUNG(KN,1) * UNSTR % XYZ(P1,DIM)
            IF (GLOBAL%DBG == 1)  THEN
               WRITE(*,*) DIM,U,K,KN,teilen, UNSTR%KNT_SPANNUNG(KN,1), UNSTR%PKT_DN(U,DIM),&
                     UNSTR % XYZ(P1,DIM)
            END IF
            teilen = teilen + UNSTR%KNT_SPANNUNG(KN,1)
         END DO
         UNSTR%PKT_DN(U,DIM) = UNSTR%PKT_DN(U,DIM) / teilen - UNSTR % XYZ(U,DIM)
      END DO
   END DO


END IF !GLOBAL % NODE_MOVEMENT

#ifdef DEBUG
IF (GLOBAL%DBG == 1)  THEN
   N = 1
   K = 1
   DO I = 1,BLOCKS(N)%NPI
      DO J = 1,BLOCKS(N)%NPJ
         UP = BLOCKS(N) % ASSOC(I,J,K)
            WRITE(*,*) BLOCKS(N) % XYZ(I,J,K,:)
            WRITE(*,*) UNSTR%PKT_DN(UP,:)
      END DO
   END DO
END IF
#endif

DN_SUM = 0.0D+00
DN_MAX = -1.0D+00
DN_MAX_POS = -1
DO U = 1,UNSTR%NPKT
   N = UNSTR % PKT_TYPE(U)
   IF (N == 1 .OR. N == 3 ) THEN!( I > 1 .AND. I < BLOCKS(N) % NPI) THEN
      UNSTR % XYZ(U,1) = UNSTR % XYZ(U,1) + UNSTR%PKT_DN(U,1) * global%faktor
      DN_SUM = DN_SUM + ABS(UNSTR%PKT_DN(U,1))
      IF (ABS(UNSTR%PKT_DN(U,1))>DN_MAX) THEN
         DN_MAX = ABS(UNSTR%PKT_DN(U,1))
         DN_MAX_POS = U
      END IF
!   ELSE
!      UNSTR%PKT_DN(U,1) = 0.0D0
   END IF
!   IF ( J > 1 .AND. J < BLOCKS(N) % NPJ) THEN
   IF (N == 1 .OR. N == 4 ) THEN
      UNSTR % XYZ(U,2) = UNSTR % XYZ(U,2) + UNSTR%PKT_DN(U,2) * global%faktor
      DN_SUM = DN_SUM + ABS(UNSTR%PKT_DN(U,2))
      IF (ABS(UNSTR%PKT_DN(U,2))>DN_MAX) THEN
         DN_MAX = ABS(UNSTR%PKT_DN(U,2))
         DN_MAX_POS = U
      END IF
!   ELSE
!      UNSTR%PKT_DN(U,2)  = 0.0D0
   END IF
END DO



!   K = 1
!   DO N = 1,GLOBAL%NBLOCK
!
!!      WRITE(*,*) GLOBAL % NBLOCK, BLOCKS(N) % NCI, BLOCKS(N) % NCJ
!      allocate( dn(3, BLOCKS(N) % NPI, BLOCKS(N) % NPJ, BLOCKS(N) % NPK) )
!      dn = 0.0d0
!      DO I = 1,BLOCKS(N) % NPI
!         DO J = 1,BLOCKS(N) % NPJ
!            which = 0
!            DO DIM = 1,2
!
!               teilen = 0.1D-10
!               dn(DIM,I,J,K) = 0.0D0
!               IF ( DIM == 1 .AND. ( I == 1 .OR. I == BLOCKS(N)%NPI ) ) then
!!                  WRITE(*,'(A)',ADVANCE="NO") "CYCLE DIM1 "
!                  CYCLE
!               END IF
!               IF ( DIM == 2 .AND. ( J == 1 .OR. J == BLOCKS(N)%NPJ ) ) then
!!                  WRITE(*,'(A)',ADVANCE="NO") "CYCLE DIM2 "
!                  CYCLE
!               END IF
!               IF ( I < BLOCKS(N) % NPI ) THEN
!                  dn(DIM,I,J,K) = dn(DIM,I,J,K) + BLOCKS(N)%DELTA(1,DIM+1,I  ,J,K) * BLOCKS(N)%XYZ(DIM,I+1,J  ,K)
!                  teilen = teilen + BLOCKS(N)%DELTA(1,DIM+1,I  ,J  ,K)
!                  which(DIM,1) = 1
!               ELSE
!                  write(*,*) BLOCKS(N)%DELTA(1,DIM+1,I-1,J,K),BLOCKS(N)%DELTA(2,DIM+1,I,J,K),BLOCKS(N)%DELTA(2,DIM+1,I,J-1,K)
!               END IF
!               IF ( I > 1 ) THEN
!                  dn(DIM,I,J,K) = dn(DIM,I,J,K) + BLOCKS(N)%DELTA(1,DIM+1,I-1,J,K) * BLOCKS(N)%XYZ(DIM,I-1,J  ,K)
!                  teilen = teilen + BLOCKS(N)%DELTA(1,DIM+1,I-1,J  ,K)
!                  which(DIM,2) = 1
!               END IF
!               IF ( J < BLOCKS(N) % NPJ ) THEN
!                  dn(DIM,I,J,K) = dn(DIM,I,J,K) + BLOCKS(N)%DELTA(2,DIM+1,I,J  ,K) * BLOCKS(N)%XYZ(DIM,I  ,J+1,K)
!                  teilen = teilen + BLOCKS(N)%DELTA(2,DIM+1,I  ,J  ,K)
!                  which(DIM,3) = 1
!               END IF
!               IF ( J > 1 ) THEN
!                  dn(DIM,I,J,K) = dn(DIM,I,J,K)+  BLOCKS(N)%DELTA(2,DIM+1,I,J-1,K) * BLOCKS(N)%XYZ(DIM,I  ,J-1,K)
!                  teilen = teilen + BLOCKS(N)%DELTA(2,DIM+1,I  ,J-1,K)
!                  which(DIM,4) = 1
!               END IF
!               dn(DIM,I,J,K) = dn(DIM,I,J,K) / teilen - BLOCKS(N)%XYZ(DIM,I,J,K)
!            END DO
!            write(*,'(2I3,F10.5,4F8.3)') I,J, dn(1,i,j,k),which(1,:)
!            write(*,'(6X,F10.5,4F8.3)')  dn(2,i,j,k),which(2,:)
!         END DO
!      END DO
!
!      DO I = 2,BLOCKS(n) % NCI
!         DO J = 2,BLOCKS(N) % NCJ
!            DO DIM = 1,2
!               BLOCKS(N) % XYZ(DIM,I,J,K) = BLOCKS(N) % XYZ(DIM,I,J,K) + dn(DIM,I,J,K)
!            END DO
!         END DO
!      END DO
!      deallocate(dn)
!   END DO
!END IF
END SUBROUTINE
