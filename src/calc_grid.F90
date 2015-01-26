subroutine calc_grid
!! BERECHNUNG DER KANTENLÄNGE und KNOTENWERTE


   USE MOD_GLOBAL
   USE CONST
   implicit none
   INTEGER :: N,I,J,K,L,U,I1,J1,DI
   INTEGER :: TYP
   INTEGER :: P1,P2

   REAL(KIND=8) :: teilen,tlen,abstand,tabs,d1,d2,dg,y1,y2
   REAL(KIND = 8), ALLOCATABLE :: TV1(:),TV2(:)
   INTEGER,PARAMETER:: PWA (9,2) = RESHAPE((/0,1,1,0,-1,-1,-1,0,1,0,0,-1,-1,-1,0,1,1,1/),(/9,2/)) ! Punkt Wahl Array

#ifdef DEBUG
   IF (GLOBAL%DBG == 1)  THEN
      WRITE(*,*) "======================================================" &
                ,"CALC GRID START"  &
                ,"======================================================"

   END IF
#endif
   !============================================================================!
   !
   !   BERECHNUNG DER KANTENLÄNGEN
   !
   !============================================================================!
   IF (GLOBAL % AXSYM == 2) THEN
      WRITE(*,*) "3D NOCH NICHT UNTERSTÜTZT","KANTENLÄNGE"
      STOP
   END IF
   UNSTR%KNT_DN  = 0.0D0
   DO U = 1,UNSTR%NKNT
      P1 = UNSTR%KNT(U,1)
      P2 = UNSTR%KNT(U,2)
      DO N = 1,2
         tlen = UNSTR%XYZ(P2,N)-UNSTR%XYZ(P1,N)
         UNSTR%KNT_DN(U,N+1) = tlen
         UNSTR%KNT_DN(U,1) = UNSTR%KNT_DN(U,1) + tlen*tlen
      END DO
      UNSTR%KNT_DN(U,1) = SQRT(UNSTR%KNT_DN(U,1))
   END DO

#ifdef DEBUG
   IF (GLOBAL%DBG == 1)  THEN
      WRITE(*,*) "==========================="  &
                ,"KANTEN: START PUNKT (STRUKT) UND LÄNGE"              &
                ,"==========================="
      DO U = 1,UNSTR%NKNT
         P1 = UNSTR%KNT(U,1)
         WRITE(*,*) U,UNSTR%PKT_REF(P1,1:2),UNSTR%KNT_DN(U,1:3)
      END DO
   END IF
#endif
   !============================================================================!
   !
   !   FINDE NAHELIEGESTEN KNOTEN IM URSPRÜNGLICHEN GITTER, STARTEND BEI DER REFERENZ
   !   WIRD FÜR DIE INTERPOLATION DES KNOTENWERTES BENÖTIGT
   !
   !============================================================================!
#ifdef DEBUG
   IF (GLOBAL%DBG == 1)  THEN
      WRITE(*,*) "==========================="  &
                ,"NÄCHSTER NACHBAR"              &
                ,"==========================="
   END IF
#endif
   IF (GLOBAL % AXSYM == 2) THEN
      WRITE(*,*) "3D NOCH NICHT UNTERSTÜTZT","FIND NEAREST REFERENCE POINT"
      STOP
   ELSE IF (GLOBAL % AXSYM == -1) THEN
      N = 1
      J = 1
      K = 1
      DO U = 1,UNSTR % NPKT
         TYP = 2
         DO WHILE ( TYP > 1)
            TYP = 1
            I = UNSTR%PKT_REF(U,1)
            abstand = ABS(UNSTR % XYZ(U,1) - BLOCKS(N) % XYZ(I,J,K,1))
            IF (I-1 > 0) THEN
               tabs = ABS(UNSTR % XYZ(U,1) - BLOCKS(N) % XYZ(I-1,J,K,1))
               IF (tabs < abstand) THEN
                  UNSTR%PKT_REF(U,1) = I-1
                  abstand = tabs
                  TYP = 2
#ifdef DEBUG
                  IF (GLOBAL%DBG == 1) WRITE(*,*) "NEUER NÄCHSTER PUNKT GEFUNDEN", UNSTR % PKT_REF(U,1)&
                                                                                 , UNSTR % XYZ(U,1) &
                                                                                 ,BLOCKS(N) % XYZ(I,J,K,1) &
                                                                                 ,BLOCKS(N) % XYZ(UNSTR%PKT_REF(U,1),J,K,1)
#endif
               END IF
            END IF
            IF (I+1 <= BLOCKS(1) % NPI) THEN
               tabs = ABS(UNSTR % XYZ(U,1) - BLOCKS(N) % XYZ(I+1,J,K,1))
               IF (tabs < abstand) THEN
                  UNSTR%PKT_REF(U,1) = I+1
                  abstand = tabs
                  TYP = 2
#ifdef DEBUG
                  IF (GLOBAL%DBG == 1) WRITE(*,*) "NEUER NÄCHSTER PUNKT GEFUNDEN", UNSTR % PKT_REF(U,1) &
                                                                                 , UNSTR % XYZ(U,1) &
                                                                                 , BLOCKS(N) % XYZ(I,J,K,1) &
                                                                                 , BLOCKS(N) % XYZ(UNSTR%PKT_REF(U,1),J,K,1)
#endif
               END IF
            END IF
         END DO
      END DO

   ELSE
      DO U = 1,UNSTR % NPKT
         I = UNSTR%PKT_REF(U,1)
         J = UNSTR%PKT_REF(U,2)
         K = UNSTR%PKT_REF(U,3)
         N = UNSTR%PKT_REF(U,4)
         P1 = I
         P2 = J
         abstand = 1.0D+10
         TYP = 2
         DO WHILE ( TYP > 1)
            DO L = 1,9
               I1 = I + PWA(L,1)
               J1 = J + PWA(L,2)
               if (i1 < 1 .OR. J1 < 1 .OR. I1 > BLOCKS(N) % NPI .OR. J1 > BLOCKS(N) % NPJ) CYCLE
               tabs = 0.0D0
               DO DI = 1,2
                  tabs = tabs + (UNSTR % XYZ(U,DI) - BLOCKS(N) % XYZ(I1,J1,K,DI)) &
                              * (UNSTR % XYZ(U,DI) - BLOCKS(N) % XYZ(I1,J1,K,DI))
               END DO
               tabs = SQRT(tabs)

               IF (L > 1) THEN ! ÜBERPRÜFE DEN ABSTAND
                  IF (tabs < abstand) THEN
                     P1 = I1
                     P2 = J1
                     TYP = L
                  END IF
               ELSE
                  abstand = tabs
                  TYP = -1
               END IF

            END DO
            IF (TYP > 1) THEN
               I = P1
               J = P2
#ifdef DEBUG
               IF (GLOBAL%DBG == 1) THEN
                  WRITE(*,'(A,I0,A,2(I0,X),A,2(I0,X))') "NEUER NÄCHSTER PUNKT GEFUNDEN ",u &
                        ," von ",UNSTR%PKT_REF(U,1),UNSTR%PKT_REF(U,2),"nach ", I,J
                  WRITE(*,'(5F10.3)')  UNSTR % XYZ(U,1:2),BLOCKS(N) % XYZ(I,J,K,1:2)
               END IF
#endif

               UNSTR%PKT_REF(U,1) = I
               UNSTR%PKT_REF(U,2) = J

            ELSE
#ifdef DEBUG
               IF (GLOBAL%DBG == 1) WRITE(*,*) "ALTER PUNKT WIRD WEITERVERWENDET",u,I,J,K &
                                    , UNSTR % XYZ(U,1:2),BLOCKS(N) % XYZ(I,J,K,1:2)
#endif
            END IF
         END DO
      END DO
   END IF

   ! referenzen updaten. beim blockübertritt muss ANGEPASST WERDEN

   !============================================================================!
   !
   !  INTERPOlATION DES KNOTENWERTES ANHAND DER SCHWERPUNKTE UM DEN REFERENZKNOTEN HERRUM
   !
   !============================================================================!
   IF (GLOBAL % NVAR > 1) THEN
      WRITE(*,*) "MULTIPLE VARS NOCH NICHT UNTERSTÜTZT","calc_grid"
      STOP
   END IF
   IF (GLOBAL % AXSYM == 2) THEN
      WRITE(*,*) "3D NOCH NICHT UNTERSTÜTZT","calc_grid"
      STOP
   ELSE IF (GLOBAL % AXSYM == -1) THEN
      DO U = 2, UNSTR%NPKT-1
         I = UNSTR%PKT_REF(U,1)
         tlen  = 1.0D0/ABS(BLOCKS(1)%SWP(I-1,1,1,1)-UNSTR%XYZ(U,1))
         UNSTR % PKT_VAR(U,1) = BLOCKS(1) % CVAR(I-1,1,1,1) * tlen
         teilen = tlen

         tlen  = 1.0D0/ABS(BLOCKS(1)%SWP(I,1,1,1)-UNSTR%XYZ(U,1))
         teilen = teilen + tlen
         UNSTR % PKT_VAR(U,1) = UNSTR % PKT_VAR(U,1) + BLOCKS(1) % CVAR(I,1,1,1) * tlen

         UNSTR % PKT_VAR(U,1) = UNSTR % PKT_VAR(U,1) / teilen
      END DO
      UNSTR % PKT_VAR(U,1) = 0.0D0
      UNSTR % PKT_VAR(U,1) = 0.0D0

#ifdef DEBUG
      IF (GLOBAL%DBG == 1)  THEN
         WRITE(*,*) "==========================="  &
                   ,"BERECHNUNG DER KNOTENWERTE" &
                   ,"==========================="
         K = 1
         J = 1
         WRITE(*,'(A10)',ADVANCE="NO") "CELL-IND"
         DO I = 1,BLOCKS(N)%NCI
            WRITE(*,'(I10)',ADVANCE="NO") I
            WRITE(*,'(X)',ADVANCE="NO")
         END DO
         WRITE(*,*)
         WRITE(*,'(A10,5X)',ADVANCE="NO") "CELL"
         DO I = 1,BLOCKS(N)%NCI
            WRITE(*,'(F10.5)',ADVANCE="NO") BLOCKS(N)%CVAR(I,J,K,:)
            WRITE(*,'(";")',ADVANCE="NO")
         END DO
         WRITE(*,*)
         WRITE(*,'(A10)',ADVANCE="NO") "PUNKT-IND"
         DO I = 1,BLOCKS(N)%NPI
            WRITE(*,'(I5)',ADVANCE="NO") I
            WRITE(*,'(6X)',ADVANCE="NO")
         END DO
         WRITE(*,*)
         WRITE(*,'(A10)',ADVANCE="NO") "PUNKT"
         DO I = 1,BLOCKS(N)%NPI
            L = BLOCKS(N) % ASSOC(I,1,K)
            WRITE(*,'(F10.5)',ADVANCE="NO") UNSTR%PKT_VAR(L,:)
            WRITE(*,'(";")',ADVANCE="NO")
         END DO
         WRITE(*,*)
      END IF
#endif


   ELSE
      ALLOCATE( TV1(GLOBAL % NVAR), TV2(GLOBAL % NVAR))
      UNSTR % PKT_VAR = 0.0D0
      DO U = 1, UNSTR%NPKT
         I = UNSTR%PKT_REF(U,1)
         J = UNSTR%PKT_REF(U,2)
         K = UNSTR%PKT_REF(U,3)
         N = UNSTR%PKT_REF(U,4)


         d1 = ABS(BLOCKS(N)%SWP(I-1,J-1,K,1)-UNSTR%XYZ(U,1))
         d2 = ABS(BLOCKS(N)%SWP(I,J-1,K,1)-UNSTR%XYZ(U,1))
         dg = d1 + d2

         y1 = (BLOCKS(N) % SWP(I,J-1,K,2) * d1 + BLOCKS(N) % SWP(I-1,J-1,K,2) * d2) / dg

         DO L = 1,GLOBAL % NVAR
            TV1(L) = (BLOCKS(N) % CVAR(I,J-1,K,L) * d1 + BLOCKS(N) % CVAR(I-1,J-1,K,L) * d2) / dg
         END DO

         d1 = ABS(BLOCKS(N)%SWP(I-1,J,K,1)-UNSTR%XYZ(U,1))
         d2 = ABS(BLOCKS(N)%SWP(I,J,K,1)-UNSTR%XYZ(U,1))
         dg = d1 + d2

         y2 = (BLOCKS(N) % SWP(I,J,K,2) * d1 + BLOCKS(N) % SWP(I-1,J,K,2) * d2) / dg

         DO L = 1,GLOBAL % NVAR
            TV2(L) = (BLOCKS(N) % CVAR(I,J,K,L) * d1 + BLOCKS(N) % CVAR(I-1,J,K,L) * d2) / dg
         END DO

         d1 = ABS(y1-UNSTR%XYZ(U,2))
         d2 = ABS(y2-UNSTR%XYZ(U,2))
         dg = d1 + d2


         DO L = 1, GLOBAL%NVAR
            UNSTR % PKT_VAR(U,L) = (TV1(L)*d2+TV2(L)*d1)/dg
         END DO
!         teilen = 0.0D0
!         TYP = UNSTR%PKT_TYPE(U)
!         !!! RECHTS OBEN
!         tlen = 1.0D0 / SQRT(                                       &
!                 (BLOCKS(N)%SWP(I,J,K,1)-UNSTR%XYZ(U,1))    &
!               * (BLOCKS(N)%SWP(I,J,K,1)-UNSTR%XYZ(U,1))    &
!               + (BLOCKS(N)%SWP(I,J,K,2)-UNSTR%XYZ(U,2))    &
!               * (BLOCKS(N)%SWP(I,J,K,2)-UNSTR%XYZ(U,2))+1E-10)
!         DO L = 1, GLOBAL%NVAR
!            UNSTR % PKT_VAR(U,L) = UNSTR % PKT_VAR(U,L)                                                    &
!                                 + BLOCKS(N) % CVAR(I,J,K,L) * tlen
!   !         write(*,*) I,J,K,UNSTR % PKT_VAR(U,L),BLOCKS(N) % CVAR(I,J,K,L),tlen
!         END DO
!         teilen = teilen + tlen
!      !! RECHTS UNTEN
!         tlen = 1.0D0 / SQRT(                                       &
!                 (BLOCKS(N)%SWP(I,J-1,K,1)-UNSTR%XYZ(U,1))    &
!               * (BLOCKS(N)%SWP(I,J-1,K,1)-UNSTR%XYZ(U,1))    &
!               + (BLOCKS(N)%SWP(I,J-1,K,2)-UNSTR%XYZ(U,2))    &
!               * (BLOCKS(N)%SWP(I,J-1,K,2)-UNSTR%XYZ(U,2))+1E-10)
!
!         DO L = 1, GLOBAL%NVAR
!            UNSTR % PKT_VAR(U,L) = UNSTR % PKT_VAR(U,L)                                                    &
!                                 + BLOCKS(N) % CVAR(I,J-1,K,L) * tlen
!   !         write(*,*) UNSTR % PKT_VAR(U,L),BLOCKS(N) % CVAR(I,J-1,K,L),tlen
!         END DO
!         teilen = teilen + tlen
!      !! LINKS OBEN
!
!         tlen = 1.0D0 / SQRT(                                       &
!                 (BLOCKS(N)%SWP(I-1,J,K,1)-UNSTR%XYZ(U,1))    &
!               * (BLOCKS(N)%SWP(I-1,J,K,1)-UNSTR%XYZ(U,1))    &
!               + (BLOCKS(N)%SWP(I-1,J,K,2)-UNSTR%XYZ(U,2))    &
!               * (BLOCKS(N)%SWP(I-1,J,K,2)-UNSTR%XYZ(U,2))+1E-10)
!         DO L = 1, GLOBAL%NVAR
!            UNSTR % PKT_VAR(U,L) = UNSTR % PKT_VAR(U,L)                                                    &
!                                 + BLOCKS(N) % CVAR(I-1,J,K,L) * tlen
!   !         write(*,*) UNSTR % PKT_VAR(U,L),BLOCKS(N) % CVAR(I-1,J,K,L),tlen
!         END DO
!         teilen = teilen + tlen
!      !! LINKS UNTEN
!
!         tlen = 1.0D0 / SQRT(                                       &
!                 (BLOCKS(N)%SWP(I-1,J-1,K,1)-UNSTR%XYZ(U,1))    &
!               * (BLOCKS(N)%SWP(I-1,J-1,K,1)-UNSTR%XYZ(U,1))    &
!               + (BLOCKS(N)%SWP(I-1,J-1,K,2)-UNSTR%XYZ(U,2))    &
!               * (BLOCKS(N)%SWP(I-1,J-1,K,2)-UNSTR%XYZ(U,2))+1E-10)
!         DO L = 1, GLOBAL%NVAR
!            UNSTR % PKT_VAR(U,L) = UNSTR % PKT_VAR(U,L)                                                    &
!                                 + BLOCKS(N) % CVAR(I-1,J-1,K,L) * tlen
!   !         write(*,*) UNSTR % PKT_VAR(U,L),BLOCKS(N) % CVAR(I-1,J-1,K,L),tlen
!         END DO
!         teilen = teilen + tlen
!
!
!         DO L = 1, GLOBAL%NVAR
!            UNSTR % PKT_VAR(U,L) = UNSTR % PKT_VAR(U,L) / teilen
!         END DO

      END DO
#ifdef DEBUG
      IF (GLOBAL%DBG == 1)  THEN
         WRITE(*,*) "==========================="  &
                   ,"BERECHNUNG DER KNOTENWERTE" &
                   ,"==========================="

         K = 1
         DO N=1,GLOBAL%NBLOCK

            WRITE(*,'(4X)',ADVANCE="NO")
            DO I = 0,BLOCKS(N)%NPI
               WRITE(*,'(I10)',ADVANCE="NO") &
                          I
               WRITE(*,'(";")',ADVANCE="NO")
            END DO
            WRITE(*,*)

            DO J = 0,BLOCKS(N)%NPJ
            WRITE(*,'(I3,X)',ADVANCE="NO") J
               DO I = 0,BLOCKS(N)%NPI
                  WRITE(*,'(F10.5)',ADVANCE="NO") &
                             BLOCKS(N)%CVAR(I,J,K,:)
                  WRITE(*,'(";")',ADVANCE="NO")
               END DO
               WRITE(*,*)
               IF (J<BLOCKS(N)%NPJ) THEN
                  WRITE(*,'(I3,6X)',ADVANCE="NO") J+1
                  DO I = 1,BLOCKS(N)%NPI
                     L = BLOCKS(N) % ASSOC(I,J+1,K)
                     WRITE(*,'(F10.5)',ADVANCE="NO") &
                                 UNSTR%PKT_VAR(L,:)
                     WRITE(*,'(";")',ADVANCE="NO")
                  END DO
                  WRITE(*,*)
               END IF
            END DO


            WRITE(*,*)

            DO J = 0,BLOCKS(N)%NPJ
            WRITE(*,'(I3,X)',ADVANCE="NO") J
               DO I = 0,BLOCKS(N)%NPI
                  WRITE(*,'(F10.5)',ADVANCE="NO") &
                             BLOCKS(N)%SWP(I,J,K,1)
                  WRITE(*,'(";")',ADVANCE="NO")
               END DO
               WRITE(*,*)
               IF (J<BLOCKS(N)%NPJ) THEN
                  WRITE(*,'(I3,6X)',ADVANCE="NO") J+1
                  DO I = 1,BLOCKS(N)%NPI
                     L = BLOCKS(N) % ASSOC(I,J+1,K)
                     WRITE(*,'(F10.5)',ADVANCE="NO") &
                                 UNSTR%XYZ(L,1)
                     WRITE(*,'(";")',ADVANCE="NO")
                  END DO
                  WRITE(*,*)
               END IF
            END DO
         END DO
      END IF
#endif
   END IF

#ifdef DEBUG
   IF (GLOBAL%DBG == 1)  THEN
      WRITE(*,*) "======================================================" &
                ,"CALC GRID ENDE"  &
                ,"======================================================"

   END IF
#endif
end subroutine calc_grid
