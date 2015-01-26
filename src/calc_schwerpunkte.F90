SUBROUTINE CALC_SCHWERPUNKTE()
   USE MOD_GLOBAL
   USE CONST
   IMPLICIT NONE
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !
   !                             LOCALE VARIABLEN
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   INTEGER :: N,I,J,K,L,U
   !============================================================================!
   !
   !                  SCHWERPUNKTBERECHNUNG
   !
   !============================================================================!
   DO N=1,GLOBAL%NBLOCK
      ALLOCATE( BLOCKS(N) % SWP   (0:BLOCKS(N)%NPI,0:BLOCKS(N)%NPJ,0:BLOCKS(N)%NPK,3) )
      BLOCKS(N)%SWP = 0.0D0
      IF (GLOBAL % AXSYM == 2) THEN
         DO L = 1,3
            DO U = 1,8
               DO K = 1,BLOCKS(N)%NCK
                  DO J = 1,BLOCKS(N)%NCJ
                     DO I = 1,BLOCKS(N)%NCI
                        BLOCKS(N)%SWP(I,J,K,L) = BLOCKS(N)%SWP(I,J,K,L) &
                                               + BLOCKS(N) % XYZ(I+UML(U,1),J+UML(U,2),K+UML(U,3),L)
                     END DO
                  END DO
               END DO
            END DO
            DO K = 1,BLOCKS(N)%NCK
               DO J = 1,BLOCKS(N)%NCJ
                  DO I = 1,BLOCKS(N)%NCI
                     BLOCKS(N)%SWP(I,J,K,L) = BLOCKS(N)%SWP(I,J,K,L) * 0.125D0
                  END DO
               END DO
            END DO
         END DO
      ELSE IF (GLOBAL % AXSYM == -1) THEN
         IF (GLOBAL%DBG == 1)  THEN
      WRITE(*,*) "===========================","ZELL-SCHWERPUNKTE BERECHNEN"  &
                ,"==========================="
         END IF
         DO I = 1,BLOCKS(N)%NCI
            BLOCKS(N)%SWP(I,1,1,1) = (BLOCKS(N)%XYZ(I,1,1,1)+BLOCKS(N)%XYZ(I+1,1,1,1)) *0.5D0
            BLOCKS(N)%SWP(I,1,1,2) = 1.0D0
            IF (GLOBAL%DBG == 1) WRITE(*,*) I,BLOCKS(N)%SWP(I,1,1,1),BLOCKS(N)%XYZ(I,1,1,1),BLOCKS(N)%XYZ(I+1,1,1,1)
         END DO
      ELSE
         K = 1
         DO L = 1,2
            DO U = 1,4
               DO J = 1,BLOCKS(N)%NCJ
                  DO I = 1,BLOCKS(N)%NCI
                     BLOCKS(N)%SWP(I,J,K,L) = BLOCKS(N)%SWP(I,J,K,L) &
                                            + BLOCKS(N) % XYZ(I+UML(U,1),J+UML(U,2),K,L)
                  END DO
               END DO
            END DO
            DO J = 1,BLOCKS(N)%NCJ
               DO I = 1,BLOCKS(N)%NCI
                  BLOCKS(N)%SWP(I,J,K,L) = BLOCKS(N)%SWP(I,J,K,L) * 0.25D0
               END DO
            END DO
         END DO
      END IF
   END DO
   !============================================================================!
   !
   !                  randpunkte
   !
   !============================================================================!
   IF (GLOBAL % AXSYM == 2) THEN
      WRITE(*,*) "3D NOCH NICHT UNTERSTÜTZT","randpunkte"
      STOP
   ELSE IF (GLOBAL % AXSYM == -1) THEN
      DO N=1,GLOBAL%NBLOCK
         BLOCKS(N) % SWP(0,1,1,1) =2 * BLOCKS(N)%SWP(1,1,1,1) - BLOCKS(N)%SWP(2,1,1,1)
         BLOCKS(N) % SWP(0,1,1,2) = 1.0
         BLOCKS(N) % SWP(BLOCKS(N)%NPI,1,1,1) = 2 * BLOCKS(N)%SWP(BLOCKS(N)%NCI,1,1,1)&
                                              - BLOCKS(N)%SWP(BLOCKS(N)%NCI-1,1,1,1)
         BLOCKS(N) % SWP(BLOCKS(N)%NPI,1,1,2) = 1.0
      END DO
   ELSE
      k = 1
      DO N=1,GLOBAL%NBLOCK


         DO L = 1,3
            DO I = 1,BLOCKS(N)%NCI
               ! j = 0
               BLOCKS(N)%SWP(I,0,K,L) = 2 * BLOCKS(N)%SWP(I,1,K,L) - BLOCKS(N)%SWP(I,2,K,L)

               ! j = npj
               BLOCKS(N)%SWP(I,BLOCKS(N)%NPJ,K,L) = 2 * BLOCKS(N)%SWP(I,BLOCKS(N)%NCJ,K,L)&
                                  - BLOCKS(N)%SWP(I,BLOCKS(N)%NCJ-1,K,L)
            END DO
            DO J = 0,BLOCKS(N)%NPJ
               ! I = 0
               BLOCKS(N)%SWP(0,J,K,L) = 2 * BLOCKS(N)%SWP(1,J,K,L) - BLOCKS(N)%SWP(2,J,K,L)
               ! I = npj
               BLOCKS(N)%SWP(BLOCKS(N)%NPI,J,K,L) = 2 * BLOCKS(N)%SWP(BLOCKS(N)%NCI,J,K,L)&
                                  - BLOCKS(N)%SWP(BLOCKS(N)%NCI-1,J,K,L)
            END DO
            DO I = 0,BLOCKS(N)%NPI,BLOCKS(N)%NPI
               ! j = 0
               BLOCKS(N)%SWP(I,0,K,L) = 2 * BLOCKS(N)%SWP(I,1,K,L) - BLOCKS(N)%SWP(I,2,K,L)
               ! j = npj
               BLOCKS(N)%SWP(I,BLOCKS(N)%NPJ,K,L) = 2 * BLOCKS(N)%SWP(I,BLOCKS(N)%NCJ,K,L)&
                                  - BLOCKS(N)%SWP(I,BLOCKS(N)%NCJ-1,K,L)
            END DO

         END DO
         DO L = 1,GLOBAL%NVAR
            DO I = 1,BLOCKS(N)%NCI
               ! j = 0
               BLOCKS(N)%CVAR(I,0,K,L) = 2 * BLOCKS(N)%CVAR(I,1,K,L) - BLOCKS(N)%CVAR(I,2,K,L)

               ! j = npj
               BLOCKS(N)%CVAR(I,BLOCKS(N)%NPJ,K,L) = 2 * BLOCKS(N)%CVAR(I,BLOCKS(N)%NCJ,K,L)&
                                  - BLOCKS(N)%CVAR(I,BLOCKS(N)%NCJ-1,K,L)
            END DO
            DO J = 0,BLOCKS(N)%NPJ
               ! I = 0
               BLOCKS(N)%CVAR(0,J,K,L) = 2 * BLOCKS(N)%CVAR(1,J,K,L) - BLOCKS(N)%CVAR(2,J,K,L)
               ! I = npj
               BLOCKS(N)%CVAR(BLOCKS(N)%NPI,J,K,L) = 2 * BLOCKS(N)%CVAR(BLOCKS(N)%NCI,J,K,L)&
                                  - BLOCKS(N)%CVAR(BLOCKS(N)%NCI-1,J,K,L)
            END DO
            DO I = 0,BLOCKS(N)%NPI,BLOCKS(N)%NPI
               ! j = 0
               BLOCKS(N)%CVAR(I,0,K,L) = 2 * BLOCKS(N)%CVAR(I,1,K,L) - BLOCKS(N)%CVAR(I,2,K,L)
               ! j = npj
               BLOCKS(N)%CVAR(I,BLOCKS(N)%NPJ,K,L) = 2 * BLOCKS(N)%CVAR(I,BLOCKS(N)%NCJ,K,L)&
                                  - BLOCKS(N)%CVAR(I,BLOCKS(N)%NCJ-1,K,L)
            END DO

         END DO
      END DO
   END IF
END SUBROUTINE CALC_SCHWERPUNKTE
