module grid
use const, only: DP
implicit none
contains
   SUBROUTINE STR2UNSTR()
      USE MOD_GLOBAL
      USE CONST
      IMPLICIT NONE
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !
      !                             LOCALE VARIABLEN
      !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      INTEGER :: N,I,J,K,U,L
      LOGICAL :: IS_OLD_NODE,DO_CONNECT_I,DO_CONNECT_J
      INTEGER :: MYPKT, MYKNT, NPKT

      INTEGER :: NUMBER_OF_FACES,NACHBAR,F

      REAL(KIND=8), PARAMETER :: ERROR = 1.0D-8

      CHARACTER(LEN = 1) , PARAMETER :: FACES(6) = (/"W","E","S","N","B","F"/)

      INTEGER :: BLOCKS_TO_PROCESS

      INTEGER, ALLOCATABLE :: BLOCKS_LIST   (:)
   !< LISTE MIT DEN BLÖCKEN WELCHE ZUR BERECHNUNG HINZUGEFÜGT WURDEN (RECHEN-QUEUE)
      INTEGER, ALLOCATABLE :: BLOCKS_STATUS  (:)
   !< LISTE MIT DEM STATUS JEDES BLOCKS
   !< 0: NOCH NICHTS PASSIERT
   !< 1: WURDE IN DIE RECHEN-QUEUE (BLOCKS_LIST) AUFGENOMMEN
   !< 2: WURDE BEARBEITET
      !============================================================================!
      !
      !                  UNSTRUKTURIERTES GITTER
      !
      !============================================================================!

      IF (GLOBAL % AXSYM == 2) THEN
         NUMBER_OF_FACES = 6
      ELSE
         NUMBER_OF_FACES = 4
      END IF

      IF (GLOBAL % AXSYM == 2) THEN
         WRITE(*,*) "3D NOCH NICHT UNTERSTÜTZT","unstrukt"
         STOP
      ELSE IF (GLOBAL % AXSYM == -1) THEN
         UNSTR%NPKT = 0
         UNSTR%NKNT = 0
         DO N=1,GLOBAL%NBLOCK
            UNSTR%NPKT = UNSTR%NPKT + BLOCKS(N) % NPI
            UNSTR%NKNT = UNSTR%NKNT + BLOCKS(N) % NCI
         END DO
      ELSE
         UNSTR%NPKT = 0
         UNSTR%NKNT = 0
         DO N=1,GLOBAL%NBLOCK
            UNSTR%NPKT = UNSTR%NPKT + BLOCKS(N) % NPI*BLOCKS(N) % NPJ*BLOCKS(N) % NPK
            UNSTR%NKNT = UNSTR%NKNT + BLOCKS(N) % NCI*BLOCKS(N) % NPJ*BLOCKS(N) % NPK &         ! KANTEN IN I-RICHTUNG
                                    + BLOCKS(N) % NPI*BLOCKS(N) % NCJ*BLOCKS(N) % NPK           ! KANTEN IN J-RICHTUNG
         END DO
      END IF

      ALLOCATE( UNSTR%XYZ        (UNSTR%NPKT,3))
      ALLOCATE( UNSTR%PKT_VAR    (UNSTR%NPKT,GLOBAL % NVAR))
      ALLOCATE( UNSTR%PKT_TYPE   (UNSTR%NPKT))
      ALLOCATE( UNSTR%PKT_NKNT   (UNSTR%NPKT))
      ALLOCATE( UNSTR%PKT_KNT    (UNSTR%NPKT,6))
      ALLOCATE( UNSTR%PKT_NEIGH  (UNSTR%NPKT,6))
      ALLOCATE( UNSTR%PKT_REF    (UNSTR%NPKT,4))

      ALLOCATE( UNSTR%KNT        (UNSTR%NKNT,2))
      ALLOCATE( UNSTR%KNT_DN     (UNSTR%NKNT,4))
      ALLOCATE( UNSTR%KNT_SPANNUNG     (UNSTR%NKNT,1))

      UNSTR%PKT_TYPE = -1
      UNSTR%PKT_NKNT = 0
      UNSTR%PKT_KNT = -1

      IF (GLOBAL%DBG >= 1)  THEN
         WRITE(*,"(A,I0)") "ANZAHL AN GITTERPUNKTE IM UNSTRUKTURIERTEN GITTER: ",UNSTR%NPKT
         WRITE(*,"(A,I0)") "ANZAHL AN KANTEN IM UNSTRUKTURIERTEN GITTER: ",UNSTR%NKNT
      END IF
   !   STOP
      nPKT = 0
      MYPKT = 0
      myKNT = 0
      K = 1

      ALLOCATE ( BLOCKS_LIST  (GLOBAL%NBLOCK))
      ALLOCATE ( BLOCKS_STATUS (-1:GLOBAL%NBLOCK))

      BLOCKS_LIST(:) = 0
      BLOCKS_STATUS(:) = 0

      BLOCKS_TO_PROCESS = 1
      BLOCKS_LIST(BLOCKS_TO_PROCESS) = 1
      BLOCKS_STATUS(BLOCKS_LIST(BLOCKS_TO_PROCESS)) = 1

   !   DO N=1,GLOBAL%NBLOCK
      DO WHILE (BLOCKS_TO_PROCESS > 0)
         N = BLOCKS_LIST(1)
         BLOCKS_STATUS(N) = 2
         IF (GLOBAL%DBG >= 1) &
         write(*,*) "BLOCK",N,"of",GLOBAL%NBLOCK,"WIRD ABGEARBEITET",BLOCKS_TO_PROCESS
         ALLOCATE( BLOCKS(N) % ASSOC (BLOCKS(N)%NPI,BLOCKS(N)%NPJ,BLOCKS(N)%NPK) )
         DO J = 1,BLOCKS(N)%NPJ
            DO I = 1,BLOCKS(N)%NPI
               DO_CONNECT_I = .TRUE.
               DO_CONNECT_J = .TRUE.
               IS_OLD_NODE = .TRUE.
               IF ( I == 1 .AND. BLOCKS(N) % BLOCK_CONNECTION(1,1) >= 1 &
               .AND. BLOCKS_STATUS(BLOCKS(N) % BLOCK_CONNECTION(1,1)) == 2) THEN
                  DO_CONNECT_J = .FALSE.
                  IF (BLOCKS(N) % BLOCK_CONNECTION(1,2) == 2 .AND. &
                      BLOCKS(N) % BLOCK_CONNECTION(1,3) == 1) THEN
                      L = BLOCKS(BLOCKS(N) % BLOCK_CONNECTION(1,1)) % NPI
                      MYPKT = BLOCKS(BLOCKS(N) % BLOCK_CONNECTION(1,1)) % ASSOC(L,J,K)
                      IF (GLOBAL%DBG >= 2) &
                      WRITE(*,'("VERBINDUNG VON BLOCK ",I0,A," J = ",I0," AUF BLOCK ",I0,A," PUNKT: ",I0)') &
                              N,FACES(1),J,BLOCKS(N) % BLOCK_CONNECTION(1,1),FACES(BLOCKS(N) % BLOCK_CONNECTION(1,2)),MYPKT

                  ELSE
                     WRITE(*,*) "BLOCK",N,"WEST AUF" &
                           ,BLOCKS(N) % BLOCK_CONNECTION(1,1) &
                           ,BLOCKS(N) % BLOCK_CONNECTION(1,2) &
                           ,BLOCKS(N) % BLOCK_CONNECTION(1,3)
                     STOP "WEST RAND KANN NUR MIT UNVERDREHTER OST RAND"
                  END IF
               ELSE IF ( I == BLOCKS(N) % NPI .AND. BLOCKS(N) % BLOCK_CONNECTION(2,1) >= 1 &
               .AND. BLOCKS_STATUS(BLOCKS(N) % BLOCK_CONNECTION(2,1)) == 2) THEN
                  IF (J == 1 .AND. BLOCKS_STATUS(BLOCKS(N) % BLOCK_CONNECTION(3,1)) == 2) THEN
                     DO_CONNECT_I = .FALSE.
                  ELSE IF ( J == BLOCKS(N) % NPJ .AND. BLOCKS_STATUS(BLOCKS(N) % BLOCK_CONNECTION(4,1)) == 2) THEN
                     DO_CONNECT_I = .FALSE.
                  END IF
                  DO_CONNECT_J = .FALSE.
                  IF (BLOCKS(N) % BLOCK_CONNECTION(2,2) == 1 .AND. &
                      BLOCKS(N) % BLOCK_CONNECTION(2,3) == 1) THEN
                      MYPKT = BLOCKS(BLOCKS(N) % BLOCK_CONNECTION(2,1)) % ASSOC(1,J,K)
                      IF (GLOBAL%DBG >= 2) &
                      WRITE(*,'("VERBINDUNG VON BLOCK ",I0,A," J = ",I0," AUF BLOCK ",I0,A," PUNKT: ",I0)') &
                              N,FACES(2),J,BLOCKS(N) % BLOCK_CONNECTION(2,1),FACES(BLOCKS(N) % BLOCK_CONNECTION(2,2)),MYPKT
                  ELSE
                     WRITE(*,*) "BLOCK",N,"OST AUF" &
                           ,BLOCKS(N) % BLOCK_CONNECTION(2,1) &
                           ,BLOCKS(N) % BLOCK_CONNECTION(2,2) &
                           ,BLOCKS(N) % BLOCK_CONNECTION(2,3)
                     STOP "OST RAND KANN NUR MIT UNVERDREHTER WEST RAND"
                  END IF
               ELSE IF ( J == 1 .AND. BLOCKS(N) % BLOCK_CONNECTION(3,1) >= 1) THEN
                  DO_CONNECT_I = .FALSE.
                  IF (BLOCKS(N) % BLOCK_CONNECTION(3,2) == 4 .AND. &
                      BLOCKS(N) % BLOCK_CONNECTION(3,3) == 1) THEN
                      L = BLOCKS(BLOCKS(N) % BLOCK_CONNECTION(3,1)) % NPJ
                      MYPKT = BLOCKS(BLOCKS(N) % BLOCK_CONNECTION(3,1)) % ASSOC(I,L,K)

                      IF (GLOBAL%DBG >= 2) &
                      WRITE(*,'("VERBINDUNG VON BLOCK ",I0,A," I = ",I0," AUF BLOCK ",I0,A," PUNKT: ",I0)') &
                              N,FACES(3),I,BLOCKS(N) % BLOCK_CONNECTION(3,1),FACES(BLOCKS(N) % BLOCK_CONNECTION(3,2)),MYPKT
                  ELSE IF (BLOCKS(N) % BLOCK_CONNECTION(3,2) == 2 .AND. &
                      BLOCKS(N) % BLOCK_CONNECTION(3,3) == 2) THEN
                      L = BLOCKS(BLOCKS(N) % BLOCK_CONNECTION(3,1)) % NPI
                      U = BLOCKS(N)%NPI ! BLOCKS(BLOCKS(N) % BLOCK_CONNECTION(3,1)) % NPJ
                      MYPKT = BLOCKS(BLOCKS(N) % BLOCK_CONNECTION(3,1)) % ASSOC(L,U-I+1,K)
                      IF (GLOBAL%DBG >= 2) &
                      WRITE(*,'("VERBINDUNG VON BLOCK ",I0,A," I = ",I0," AUF BLOCK ",I0,A," PUNKT: ",I0)') &
                              N,FACES(3),I,BLOCKS(N) % BLOCK_CONNECTION(3,1),FACES(BLOCKS(N) % BLOCK_CONNECTION(3,2)),MYPKT
                  ELSE
                     WRITE(*,*) "BLOCK",N,"SUED AUF" &
                           ,BLOCKS(N) % BLOCK_CONNECTION(3,1) &
                           ,BLOCKS(N) % BLOCK_CONNECTION(3,2) &
                           ,BLOCKS(N) % BLOCK_CONNECTION(3,3)
                     STOP "SUED RAND KANN NUR MIT UNVERDREHTER NORD bzw.   OST RAND"
                  END IF
               ELSE
                  nPKT = nPKT + 1
                  MYPKT = nPKT
                  DO L = 1,3
                     UNSTR%XYZ(MYPKT,L) = BLOCKS(N) % XYZ(I,J,K,L)
                  END DO
                  UNSTR%PKT_REF(MYPKT,4) = N
                  UNSTR%PKT_REF(MYPKT,1) = I
                  UNSTR%PKT_REF(MYPKT,2) = J
                  UNSTR%PKT_REF(MYPKT,3) = K
                  IS_OLD_NODE = .FALSE.
               END IF
               BLOCKS(N) % ASSOC(I,J,K) = MYPKT

               IF (IS_OLD_NODE) THEN
                  IF ((I > 1.AND. I < BLOCKS(N)%NPI) .OR. (J > 1 .AND. J < BLOCKS(N)%NPJ)) THEN
                     UNSTR%PKT_TYPE(MYPKT) = 1
                  ELSE IF ( I == 1 .AND. J == 1) THEN
                     ! PUNKT MUSS VON VIER oder DREI (bei O-GRID) BLÖCKEN UMGEBEN SEIN.
                     ! TESTENDER BLOCK IST RECHT DRÜBER. D.H.

                     L = BLOCKS(N) % BLOCK_CONNECTION(3,1) ! UNTERER BLOCK
                     U = BLOCKS(N) % BLOCK_CONNECTION(1,1) ! LINKER BLOCK
                     IF (L >= 1 .AND. U >= 1) THEN
                        IF   (BLOCKS(N) % BLOCK_CONNECTION(3,2) == 4 &
                        .AND. BLOCKS(N) % BLOCK_CONNECTION(3,3) == 1 ) THEN
                           L = BLOCKS(L) % BLOCK_CONNECTION(1,1) ! BLOCK RUNTER->LINKS

                        ELSE IF (BLOCKS(N) % BLOCK_CONNECTION(3,2) == 2 &
                           .AND. BLOCKS(N) % BLOCK_CONNECTION(3,3) == 2 ) THEN
                           L = BLOCKS(L) % BLOCK_CONNECTION(4,1) ! BLOCK RUNTER->LINKS ( da block verdreht: in diesem block wieder hoch


                        ELSE
                           STOP "BLOCK CONNECTION FALL IST NOCH NICHT UNTERSTÜTZT"
                        END IF
                        U = BLOCKS(U) % BLOCK_CONNECTION(3,1) ! BLOCK LINKS -> RUNTER
                        IF (U >= 1 .AND. L >= 1 ) THEN ! .AND. U == L
                           UNSTR%PKT_TYPE(MYPKT) = 1
                        END IF
                     END IF
                  END IF
               ELSE
                  IF (I > 1 .AND. I < BLOCKS(N)%NPI .AND. J > 1 .AND. J < BLOCKS(N)%NPJ) THEN
                     UNSTR%PKT_TYPE(MYPKT) = 1
                  ELSE
                     UNSTR%PKT_TYPE(MYPKT) = 2
                  END IF
               END IF
               IF (DO_CONNECT_I .AND. I > 1) THEN ! .AND. .NOT.(J == 1 .AND. BLOCKS(N) % BLOCK_CONNECTION(3,1) >= 1)) THEN
                  MYKNT = MYKNT +1
                  L = BLOCKS(N) % ASSOC(I-1,J,K) ! KÖNNTE AUCH DIREKT MIT MYPKT-1 gesetzt werden
                                                 ! aber so werden mehrfachassoziationen von struktuierten
                                                 ! Punkte auf unstrukturierte Punkte direkt mitbeachtet
                  UNSTR%KNT(MYKNT,1) = L
                  UNSTR%KNT(MYKNT,2) = MYPKT
                  ! KANTEN DEN BEIDEN PUNKTE HINZUFÜGEN
                  !! AKTUELLER PUNKT
                  U = UNSTR%PKT_NKNT(MYPKT) + 1
                  UNSTR%PKT_NKNT(MYPKT) = U
                  UNSTR%PKT_KNT(MYPKT,U) = MYKNT
                  UNSTR%PKT_NEIGH(MYPKT,U) = L
                  !! ZWEITER KANTENPKT
                  U = UNSTR%PKT_NKNT(L) + 1
                  UNSTR%PKT_NKNT(L) = U
                  UNSTR%PKT_KNT(L,U) = MYKNT
                  UNSTR%PKT_NEIGH(L,U) = MYPKT
#ifdef DEBUG
                  IF (GLOBAL%DBG >= 2) &
                     WRITE(*,'("VERBINDE (X-DIR) ",I0," (",I0,",",I0,",",I0,") MIT ",I0," KANTE: ",I0)') MYPKT,I,J,K,L,MYKNT
#endif
               END IF

               IF (DO_CONNECT_J .AND. J > 1) THEN! .AND. .NOT.(I == 1 .AND. BLOCKS(N) % BLOCK_CONNECTION(1,1) >= 1)) THEN
                  MYKNT = MYKNT +1
                  L = BLOCKS(N) % ASSOC(I,J-1,K)
                  UNSTR%KNT(MYKNT,1) = L
                  UNSTR%KNT(MYKNT,2) = MYPKT
                  ! KANTEN DEN BEIDEN PUNKTE HINZUFÜGEN
                  !! AKTUELLER PUNKT
                  U = UNSTR%PKT_NKNT(MYPKT) + 1
                  UNSTR%PKT_NKNT(MYPKT) = U
                  UNSTR%PKT_KNT(MYPKT,U) = MYKNT
                  UNSTR%PKT_NEIGH(MYPKT,U) = L
                  !! ZWEITER KANTENPKT
                  U = UNSTR%PKT_NKNT(L) + 1
                  UNSTR%PKT_NKNT(L) = U
                  UNSTR%PKT_KNT(L,U) = MYKNT
                  UNSTR%PKT_NEIGH(L,U) = MYPKT
#ifdef DEBUG
                  IF (GLOBAL%DBG >= 2) &
                    WRITE(*,'("VERBINDE (Y-DIR) ",I0," (",I0,",",I0,",",I0,") MIT ",I0," KANTE: ",I0)') MYPKT,I,J,K,L,MYKNT
#endif
               END IF

   !            ELSE IF (I == 1 .AND. J == 1) THEN
   !               UNSTR%PKT_TYPE(MYPKT ) = 6
   !            ELSE IF (I == BLOCKS(N)%NPI .AND. J == 1) THEN
   !               UNSTR%PKT_TYPE(MYPKT) = 7
   !            ELSE IF (I == BLOCKS(N)%NPI .AND. J == BLOCKS(N)%NPJ) THEN
   !               UNSTR%PKT_TYPE(MYPKT) = 8
   !            ELSE IF (I == 1 .AND. J == BLOCKS(N)%NPJ) THEN
   !               UNSTR%PKT_TYPE(MYPKT) = 9
   !            ELSE IF (I == 1) THEN
   !               UNSTR%PKT_TYPE(MYPKT) = 2
   !            ELSE IF (J == BLOCKS(N)%NPJ) THEN
   !               UNSTR%PKT_TYPE(MYPKT) = 3
   !            ELSE IF (I == BLOCKS(N)%NPI) THEN
   !               UNSTR%PKT_TYPE(MYPKT) = 4
   !            ELSE IF (J == 1) THEN
   !               UNSTR%PKT_TYPE(MYPKT) = 5
   !            END IF

            END DO
         END DO
   !      BLOCKS_STATUS

         BLOCKS_TO_PROCESS = BLOCKS_TO_PROCESS - 1
         DO F = 1,BLOCKS_TO_PROCESS
            BLOCKS_LIST(F) = BLOCKS_LIST(F+1)
         END DO
         DO F = 1,NUMBER_OF_FACES
            NACHBAR = BLOCKS(N) % BLOCK_CONNECTION(F,1)
            IF (NACHBAR >= 1) THEN

               IF (BLOCKS_STATUS(NACHBAR) == 0 ) THEN
                  BLOCKS_TO_PROCESS = BLOCKS_TO_PROCESS + 1
                  BLOCKS_LIST(BLOCKS_TO_PROCESS) = NACHBAR
                  BLOCKS_STATUS(NACHBAR) = 1
                  IF (GLOBAL%DBG >= 1) &
                  WRITE(*,'("FÜGE ",I0,"(",A,") AN STELLE ",I0)') NACHBAR,FACES(F),BLOCKS_TO_PROCESS

               END IF
            END IF
         END DO
      END DO

      DO I = 1,GLOBAL % NBLOCK
         IF (BLOCKS_STATUS(I) /= 2) THEN
            WRITE(*,*) "BLOCK",I,"WURDE NOCH NICHT ABGEARBEITET"
            STOP
         END IF
      END DO

      DEALLOCATE ( BLOCKS_LIST )
      DEALLOCATE ( BLOCKS_STATUS )
      IF (nPKT /= UNSTR % NPKT) THEN
#ifdef DEBUG
         IF (GLOBAL%DBG >= 1)  THEN
            WRITE(*,"(A,I0,A,I0)") "ANZAHL DER REALEN GITTERPUNKTE IM UNSTR. GITTER: ",nPKT," DIFF: ",UNSTR % NPKT-nPKT

         END IF
#endif
         UNSTR % NPKT = NPKT

      END IF
      IF (MYKNT /= UNSTR % NKNT) THEN
#ifdef DEBUG
               IF (GLOBAL%DBG >= 1)  THEN
            WRITE(*,"(A,I0,A,I0)") "ANZAHL AN REALEN KANTEN IM UNSTR. GITTER: ",MYKNT," DIFF: ",UNSTR % NKNT-MYKNT

         END IF
#endif
         UNSTR % NKNT = MYKNT

      END IF


   !!!!!!!!!!!!!!!!!!!!

   !!   RANDPUNKTE AUF VERSCHIEBBARKEIT UNTERSUCHEN

   !!!!!!!!!!!!!!!!!!!!
   IF (GLOBAL % AXSYM == 2) THEN
      WRITE(*,*) "3D NOCH NICHT UNTERSTÜTZT","RANDPUNKT-VERSCHIEBUNG@STR2UNSTR"
      STOP
   END IF
   DO U = 1,UNSTR % NPKT
      IF ( UNSTR % PKT_TYPE(U) == 1) CYCLE
#ifdef DEBUG
      IF (GLOBAL%DBG >= 2)  THEN
         I = UNSTR%PKT_REF(U,1)
         J = UNSTR%PKT_REF(U,2)
         K = UNSTR%PKT_REF(U,3)
         N = UNSTR%PKT_REF(U,4)
         WRITE(*,*) U, N, I,J
         WRITE(*,*) U,UNSTR % XYZ(U,1),UNSTR % XYZ(U,2)
      END IF
#endif
      I = 0
      J = 0
      DO N = 1, UNSTR % PKT_NKNT(U)
         K = UNSTR % PKT_NEIGH(U,N)
#ifdef DEBUG
         IF (GLOBAL%DBG >= 2)  THEN
            WRITE(*,*) K,UNSTR % XYZ(K,1),UNSTR % XYZ(K,2)
         END IF
#endif
         IF (ABS(UNSTR % XYZ(U,2) - UNSTR % XYZ(K,2)) <= ERROR) THEN
            I = I +1
         END IF
         IF (ABS(UNSTR % XYZ(U,1) - UNSTR % XYZ(K,1)) <= ERROR) THEN
            J = J +1
         END IF

      END DO
      !!! IN 2D Müssen zwei Nachbaren den selben Y-WERT HABEN
      IF (I == 2) THEN
         UNSTR % PKT_TYPE(U) = 3
      ELSE IF (I > 2) THEN
         STOP "ERROR BEI DER RANDPUNKT-VERSCHIEBUNG in I RICHTUNG"
      END IF
      IF (J == 2) THEN
         IF (UNSTR % PKT_TYPE(U) == 3) THEN
            !!! DIESE PUNKTE SOLLTEN ALLE SCHON DURCH DIE OBERE ROUTINE ERKANNT WORDEN SEIN.
            !!! VERMUTLICH ECKPUNKT AN DREI BLÖCKEN
            WRITE ( *,*) "WARNUNG: RANDPUNKT KANN SOWOHL IN X ALSO AUCH Y VERSCHOBEN WERDEN: WIRD FESTGESETZT"
            WRITE(*,*) "PKT,B,I,J,K:",U,UNSTR%PKT_REF(U,4),UNSTR%PKT_REF(U,1),UNSTR%PKT_REF(U,2),UNSTR%PKT_REF(U,3)
            UNSTR % PKT_TYPE(U) = 2
         ELSE
            UNSTR % PKT_TYPE(U) = 4
         END IF
      ELSE IF (J > 2) THEN
         STOP "ERROR BEI DER RANDPUNKT-VERSCHIEBUNG in J RICHTUNG"
      END IF

   END DO

   IF (Global%CHECK_FOR_DOUBLE_POINTS==1) THEN
      WRITE(*,*) "TESTE AUF DOPPELTE PUNKTE - START"
      J = 0
      DO U = 1, UNSTR% NPKT
         DO N = 1, UNSTR % NPKT
            IF (U == N) CYCLE
            IF   (ABS(UNSTR % XYZ (U,1) - UNSTR % XYZ (N,1)) <= 1E-9 &
            .AND. ABS(UNSTR % XYZ (U,2) - UNSTR % XYZ (N,2)) <= 1E-9 &
            .AND. ABS(UNSTR % XYZ (U,3) - UNSTR % XYZ (N,3)) <= 1E-9 ) THEN

               WRITE(*,'("KONTEN ",I0," UND ",I0," SIND DOPPELT VORHANDEN. KOORD: ",3(F10.7,X))') u,n, UNSTR % XYZ (N,1:3)

               WRITE(*,'("REFERENZ(",I0,"): BLOCK,I,J,K ",I0,X,I0,X,I0,X,I0)') U,UNSTR%PKT_REF(U,4),UNSTR%PKT_REF(U,1) &
                                                                            ,UNSTR%PKT_REF(U,2),UNSTR%PKT_REF(U,3)
               WRITE(*,'("REFERENZ(",I0,"): BLOCK,I,J,K ",I0,X,I0,X,I0,X,I0)') N,UNSTR%PKT_REF(N,4),UNSTR%PKT_REF(N,1) &
                                                                            ,UNSTR%PKT_REF(N,2),UNSTR%PKT_REF(N,3)


               J = J + 1
               IF (J >= 10) THEN
                  STOP
               END IF
            END IF
         END DO
      END DO

      IF  ( J == 1) STOP

      WRITE(*,*) "TESTE AUF DOPPELTE PUNKTE - ENDE"
   END IF

   IF (Global%CHECK_FOR_DOUBLE_KANTEN==1) THEN

      WRITE(*,*) "TESTE AUF DOPPELTE VERBINDUNGEN - START"

      J = 0
      DO U = 1, UNSTR% NPKT

         DO N = 1, UNSTR % PKT_NKNT(U)
            DO I = 1, UNSTR % PKT_NKNT(U)
               IF (I == N) CYCLE
               IF (UNSTR % PKT_NEIGH(U,N) == UNSTR % PKT_NEIGH(U,I) ) THEN
                  K = UNSTR % PKT_NEIGH(U,I)
                  WRITE(*,'("PUNKT ",I0," HAT ZWEI VERDINGUNGEN ZU ",I0," KNT: ",I0,X,I0)')  U,K       &
                                                                 ,UNSTR % PKT_KNT(U,N),UNSTR % PKT_KNT(U,I)
                  WRITE(*,'("REFERENZ(",I0,"): BLOCK,I,J,K ",I0,X,I0,X,I0,X,I0)') U,UNSTR%PKT_REF(U,4),UNSTR%PKT_REF(U,1) &
                                                                            ,UNSTR%PKT_REF(U,2),UNSTR%PKT_REF(U,3)
                  WRITE(*,'("REFERENZ(",I0,"): BLOCK,I,J,K ",I0,X,I0,X,I0,X,I0)') K,UNSTR%PKT_REF(K,4),UNSTR%PKT_REF(K,1) &
                                                                            ,UNSTR%PKT_REF(K,2),UNSTR%PKT_REF(K,3)
                  J = J + 1
                  IF (J >= 10) THEN
                     STOP
                  END IF
               END IF
            END DO

         END DO

      END DO
      IF (J == 1) STOP
      WRITE(*,*) "TESTE AUF DOPPELTE VERBINDUNGEN - ENDE"
   END IF

#ifdef DEBUG
      IF (GLOBAL%DBG >= 2)  THEN
         WRITE(*,*) "===========================","UNSTR PUNKTE VERBINDUNGEN"  &
                   ,"==========================="
         K = 1
         DO N=1,GLOBAL%NBLOCK
            DO J = 1,BLOCKS(N)%NPJ
            WRITE(*,'(I3,4X)',ADVANCE="NO") J
               DO I = 1,BLOCKS(N)%NPI
                  L = BLOCKS(N) % ASSOC(I,J,K)
                  WRITE(*,'(I3,"[",I0,",",I0,",",I0,"]","(",I1,"):",6(I3,X))',ADVANCE="NO") &
                              L,UNSTR%PKT_REF(L,1:3),UNSTR%PKT_NKNT(L),UNSTR%PKT_NEIGH(L,1:4)!UNSTR%PKT_NKNT(L))
                  WRITE(*,'(";")',ADVANCE="NO")
               END DO
               WRITE(*,*)
            END DO
         END DO
         N = 1
         K = 1
         WRITE(*,*) "===========================","UNSTR PUNKTE KOORDINATEN" &
                     ,"==========================="
         DO I = 1,BLOCKS(N)%NPI
            DO J = 1,BLOCKS(N)%NPJ
               L = BLOCKS(N) % ASSOC(I,J,K)
               WRITE(*,*) L, BLOCKS(N) % XYZ(I,J,K,:)
            END DO
         END DO
      END IF
#endif
   END SUBROUTINE
   SUBROUTINE UNSTR2STR
   USE MOD_GLOBAL
   IMPLICIT NONE

   INTEGER :: N,I,J,K,UP,DIM
   IF (GLOBAL%DBG >= 1)  THEN
      WRITE(*,"(A)") "STRUKTURIERTES GITTER WIRD AUS DEN UNSTRUKTURIERTEN GITTER ERSTELLT"
   end if
   IF (GLOBAL % AXSYM == 2) THEN
      WRITE(*,*) "3D NOCH NICHT UNTERSTÜTZT","unstr2str"
      STOP
   END IF

   DO N = 1,GLOBAL%NBLOCK
      DO I = 1,BLOCKS(N)%NPI
         DO J = 1,BLOCKS(N)%NPJ
            DO K = 1,BLOCKS(N)%NPK
               UP = BLOCKS(N) % ASSOC(I,J,K)
               DO DIM = 1,2
                  BLOCKS(N) % XYZ(I,J,K,DIM) = UNSTR % XYZ(UP,DIM)
               END DO
               BLOCKS(N) % XYZ(I,J,K,3) = 0.0D0
            END DO
         END DO
      END DO
   END DO

   IF (GLOBAL%DBG >= 2)  THEN
      N = 1
      K = 1
      DO J = 1,BLOCKS(N)%NPJ
         DO I = 1,BLOCKS(N)%NPI
            WRITE(*,*) BLOCKS(N) % ASSOC(I,J,K),BLOCKS(N) % XYZ(I,J,K,:)
         END DO
      END DO
   END IF


   END SUBROUTINE
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
end module
