MODULE TYPES
   TYPE :: TGLOBAL
      INTEGER :: AXSYM
      INTEGER :: NBLOCK
      CHARACTER(LEN=100) :: GIT_IN
      CHARACTER(LEN=100) :: SOL_IN
      CHARACTER(LEN=100) :: PARA_OUT
      INTEGER :: DBG
      INTEGER :: CHECK_FOR_DOUBLE_POINTS
      INTEGER :: CHECK_FOR_DOUBLE_KANTEN
      INTEGER :: NVAR
   END TYPE

   TYPE :: TBLOCKS
      INTEGER :: NCI
      INTEGER :: NCJ
      INTEGER :: NCK
      INTEGER :: NPI
      INTEGER :: NPJ
      INTEGER :: NPK
      INTEGER :: IOUT
      INTEGER :: JOUT
      INTEGER :: KOUT

      REAL(KIND=8),ALLOCATABLE :: XYZ(:,:,:,:)
!< GITTERPUNKTE -POSITION (I,J,K,COORD)

      INTEGER, ALLOCATABLE :: ASSOC(:,:,:)
!< POINTER AUF UNSTRUTURIERTEN PUNKT; der DEN GITTERPUNKT REpresentiert

      INTEGER :: BLOCK_CONNECTION (6,3)
!< GIBT BLOCKVERBDINUNG DES BLOCKES AN: (FACE,VALUE)
!< FACE: 1 = W, 2 = E, 3 = S, 4 = N, 5 = B, 6 = F
!< VALUE: 1 = BLOCKNUMMER, 2= FACE DES NACHBAR_BLOCKS, 3 = PERMUTATION            ! SCHWERPUNKT
      REAL(KIND=8),ALLOCATABLE :: VAL(:,:,:,:)
!< GITTERPUNKTE -POSITION (I,J,K,VAR)


   END TYPE

   TYPE :: TUNSTR
      INTEGER :: NPKT
      INTEGER :: NKNT
      INTEGER :: NCELL

      REAL(KIND=8),ALLOCATABLE :: XYZ(:,:)
!< KOORDINATEN DES PUNKTES

      INTEGER, ALLOCATABLE :: PKT_TYPE(:)
!< Art eines Gitterpunkts:
!< 1 - normaler Grid-interner Punkt. Besitzt 4 "normale" umliegende Zellen
!< 2 - Randzelle: Kann nicht verschoben werden
!< 3 - Randzelle: Verschiebbar in X-Richtung
!< 4 - Randzellle Verschiebbar in Y-Richtung
      INTEGER, ALLOCATABLE :: PKT_NKNT(:)
!< ANzAHL DER KANTEN an PUNKTE
      INTEGER, ALLOCATABLE :: PKT_KNT(:,:)
!< KANTEN DIE IN VERBINDUNG MIT DEM PUNKT STEHEN
!< Entspricht auch der Anzahl von Nachbarn
      INTEGER, ALLOCATABLE :: PKT_NEIGH(:,:)
!< Nachbarpunkte anhand der Kanten, könnte auch über die Kanten ermittelt werden.
      INTEGER, ALLOCATABLE :: PKT_REF(:,:)
!< REFERENZKNOTENPKT AUF DEN GITTER ZUR WERTINTERPOLATION

      INTEGER, ALLOCATABLE :: KNT(:,:)
!< Vorhandene Kanten = Verbindung zwischen zwei Punkten

      INTEGER, ALLOCATABLE :: CELL(:,:)
!< PUNKTE DIE ZU EINER ZELLE GEHÖREN
!< ERSTER INDEX: UNSTR ZELLINDEX
!< ZWEITER INDEX: 1-4: PUNKTINDEX DER UNSTR PUNKTE
!<                5-7: CELLINDEX DER STR ZELLEN
!<                8  : BLOCKINDEX DER STR ZELLE
   END TYPE
END MODULE

MODULE MOD_GLOBAL
   USE TYPES
   TYPE(TGLOBAL) :: GLOBAL
   TYPE(TBLOCKS), ALLOCATABLE :: BLOCKS(:)
   TYPE(TUNSTR) :: UNSTR
   INTEGER, PARAMETER :: IO_SOL = 111
END MODULE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!     SOL TO UNSTRUCTURE PARAVIEW MAIN FILE          !!!!!!!
!!
!! AUTHOR:           ROMAN KELLER
!! START:            14.01.2015
!! LAST CHANGE:      14.01.2015
!!
!! WANDELT EINE BINÄRE LÖSUNGSDATEI in ein UNSTRUKTURIERTES PARASOL FILE UM.
!! DAMIT NUR EINE DATEI Pro ZEITSCHRITT IN PARAVIEW GELADEN WERDEN MUSS.
!!
!! CHANGELOG:
!!
!!
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program sol2unstrpara
   use mod_global
   implicit none


   CHARACTER(LEN=*),PARAMETER :: VERSION = "V0.1.0"
   CHARACTER(LEN=*),PARAMETER :: LAST_CHANGE = "14.01.2015"


   INTEGER :: i
   REAL(KIND=8) :: DN_SUM,DN_MAX
   INTEGER :: DN_MAX_POS

   INTEGER :: ITER
   LOGICAL :: NEW_SOL

   GLOBAL % GIT_IN             = "grid.bin"
   GLOBAL % SOL_IN             = "sol.ufo.bin"
   GLOBAL % CHECK_FOR_DOUBLE_POINTS = 1
   GLOBAL % CHECK_FOR_DOUBLE_KANTEN = 1
   GLOBAL % DBG  =1


   WRITE(*,*) "==============================================" &
             ,"SOL2UNSTRPARA by ROMAN KELLER" &
             ,"=============================================="
   
   WRITE(*,'(A)') "VERSION "//VERSION//" LAST CHANGE "//LAST_CHANGE

   WRITE(*,'(A)') "PLOTTING: "//TRIM(GLOBAL % GIT_IN)

   CALL INPUT_GRID()

   CALL READ_SOL_HEADER()

   CALL STR2UNSTR()
   
   DO
      CALL READ_SOL(ITER,NEW_SOL)
      IF (NEW_SOL) THEN
         CALL PARAVIEW_OUTPUT_UNSTR (ITER)
      ELSE
         EXIT
      END IF
   END DO
   WRITE(*,*) "======================================================" &
             ,"FINISHED"  &
             ,"======================================================"
end program
SUBROUTINE INPUT_GRID()
USE MOD_GLOBAL
IMPLICIT NONE
INTEGER, PARAMETER :: GIT_UNIT = 25

LOGICAL :: FEXISTS

INTEGER :: GITDIM
INTEGER :: NUMBER_OF_CORNER_POINTS
INTEGER :: NUMBER_OF_FACES
INTEGER :: NUMBER_OF_POINTS_PER_FACE
INTEGER :: NUMBER_OF_PERMUTATIONS

REAL(KIND=8),ALLOCATABLE :: CORNER_POINT (:,:,:)

INTEGER,ALLOCATABLE :: FID2CP (:,:)
!< UMRECHNUNG VON FACE UND nter PUNKT AUF DER FACE zu CORNER PUNKT

INTEGER,ALLOCATABLE :: PERM (:,:)
!< PERMUTATION DER CORNER PUNKTE AUF ER JEWEILIGEN FACE

REAL(KIND=8),PARAMETER :: eps = 1.0D-8

INTEGER :: BB, F , FF, FP , P

LOGICAL :: FOUND

CHARACTER(LEN = 1) , PARAMETER :: FACES(6) = (/"W","E","S","N","B","F"/)

CHARACTER(LEN = 5) , PARAMETER :: FACENAME(6) = (/"WEST ","OST  ","SUED ","NORD ","BACK ","FRONT"/)


INTEGER :: B,I,J,K,V

INTEGER :: nI,nJ,nK

IF (GLOBAL%DBG == 1) THEN
   WRITE(*,'(2A,X,A)') "OPENING ",TRIM(GLOBAL % GIT_IN)
END IF
INQUIRE(FILE=TRIM(GLOBAL% GIT_IN),EXIST=FEXISTS)
IF(FEXISTS .EQV. .FALSE.) THEN
   WRITE(*,*) "GITTER INPUT DATEI konnte nicht gefunden werden: " // TRIM(GLOBAL% GIT_IN)
   STOP
END IF
OPEN(GIT_UNIT,FILE=TRIM(GLOBAL% GIT_IN),FORM="UNFORMATTED",access="STREAM",STATUS="OLD")
READ(GIT_UNIT) GLOBAL %AXSYM,GLOBAL % NBLOCK

IF (GLOBAL%DBG == 1) WRITE(*,'(2(A,X,I0,5X))') "AXSYM:",GLOBAL % AXSYM,"NUMBER_OF_BLOCKS:", GLOBAL % NBLOCK
IF (GLOBAL % AXSYM == 3) THEN
   GITDIM = 3
ELSE
   GITDIM = 2
END IF

ALLOCATE(BLOCKS(GLOBAL%NBLOCK))

IF (GLOBAL%DBG == 1) WRITE(*,'(4(A5,X))') "BLOCK","NCI","NCJ","CNK"

DO B = 1,GLOBAL % NBLOCK
   IF (GLOBAL % AXSYM == 3) THEN
      READ(GIT_UNIT) BLOCKS(B) % NPI, BLOCKS(B) % NPJ, BLOCKS(B) % NPK
      BLOCKS(B) % NCI = BLOCKS(B) % NPI - 1
      BLOCKS(B) % NCJ = BLOCKS(B) % NPJ - 1
      BLOCKS(B) % NCK = BLOCKS(B) % NPK - 1
   ELSE
      READ(GIT_UNIT) BLOCKS(B) % NPI, BLOCKS(B) % NPJ, BLOCKS(B) % NPK
      BLOCKS(B) % NPK = 1
      BLOCKS(B) % NCI = BLOCKS(B) % NPI - 1
      BLOCKS(B) % NCJ = BLOCKS(B) % NPJ - 1
      BLOCKS(B) % NCK = 1
   END IF

   IF (GLOBAL%DBG == 1) WRITE(*,'(4(I5,X))') B, BLOCKS(B) % NCI, BLOCKS(B) % NCJ, BLOCKS(B) % NCK

   ALLOCATE (BLOCKS(B) % XYZ(BLOCKS(B) % NPI, BLOCKS(B) % NPJ, BLOCKS(B) % NPK, 3))

END DO !B= 1,NB  SCHLEIFE ÜBER BLÖCKE GIT
DO B = 1,GLOBAL % NBLOCK
   DO k = 1,BLOCKS(B) %  NPK
      DO j = 1,BLOCKS(B) % NPJ
         DO i= 1,BLOCKS(B) % NPI
            READ(GIT_UNIT) (BLOCKS(B) % XYZ(I,J,K,V),v=1,GITDIM)
         END DO
      END DO
   END DO
END DO !B= 1,NB  SCHLEIFE ÜBER BLÖCKE GIT IN

WRITE(*,*)
WRITE(*,'("========== GIT_IN =========")')
IF(GLOBAL % AXSYM == 3) THEN
   WRITE(*,'("3D SIMULATION")')
ELSE IF (GLOBAL % AXSYM == 2) THEN
   WRITE(*,'("2D SIMULATION")')
ELSE
   WRITE(*,'("1D SIMULATION")')
END IF
WRITE(*,'("BLOCKS ON FILE:         ",I0)') GLOBAL % NBLOCK

DO B = 1,GLOBAL % NBLOCK

END DO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!              INITIALISIERUNGEN FÜR DIE BLOCK CONNECTIONS        !!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IF (GLOBAL % AXSYM == 3) THEN
   NUMBER_OF_CORNER_POINTS = 8
   NUMBER_OF_FACES = 6
   NUMBER_OF_POINTS_PER_FACE = 4
   NUMBER_OF_PERMUTATIONS = 8
ELSE
   NUMBER_OF_CORNER_POINTS = 8
   NUMBER_OF_FACES = 4
   NUMBER_OF_POINTS_PER_FACE = 2
   NUMBER_OF_PERMUTATIONS = 2
END IF

ALLOCATE( CORNER_POINT (GLOBAL % NBLOCK,NUMBER_OF_CORNER_POINTS,GITDIM) )
ALLOCATE( FID2CP       (NUMBER_OF_FACES, NUMBER_OF_POINTS_PER_FACE))
ALLOCATE( PERM         (NUMBER_OF_PERMUTATIONS,NUMBER_OF_POINTS_PER_FACE))
IF (GLOBAL % AXSYM == 3) THEN

   FID2CP(1,1:NUMBER_OF_POINTS_PER_FACE)=(/1,3,5,7/)
   FID2CP(2,1:NUMBER_OF_POINTS_PER_FACE)=(/2,4,6,8/)
   FID2CP(3,1:NUMBER_OF_POINTS_PER_FACE)=(/1,2,5,6/)
   FID2CP(4,1:NUMBER_OF_POINTS_PER_FACE)=(/3,4,7,8/)
   FID2CP(5,1:NUMBER_OF_POINTS_PER_FACE)=(/1,2,3,4/)
   FID2CP(6,1:NUMBER_OF_POINTS_PER_FACE)=(/5,6,7,8/)

   PERM(1,1:NUMBER_OF_POINTS_PER_FACE)=(/1,2,3,4/)
   PERM(2,1:NUMBER_OF_POINTS_PER_FACE)=(/2,3,4,1/)
   PERM(3,1:NUMBER_OF_POINTS_PER_FACE)=(/3,4,1,2/)
   PERM(4,1:NUMBER_OF_POINTS_PER_FACE)=(/4,1,2,3/)
   PERM(5,1:NUMBER_OF_POINTS_PER_FACE)=(/4,3,2,1/)
   PERM(6,1:NUMBER_OF_POINTS_PER_FACE)=(/3,2,1,4/)
   PERM(7,1:NUMBER_OF_POINTS_PER_FACE)=(/2,1,4,3/)
   PERM(8,1:NUMBER_OF_POINTS_PER_FACE)=(/1,4,3,2/)

ELSE

   FID2CP(1,1:NUMBER_OF_POINTS_PER_FACE)=(/1,3/)
   FID2CP(2,1:NUMBER_OF_POINTS_PER_FACE)=(/2,4/)
   FID2CP(3,1:NUMBER_OF_POINTS_PER_FACE)=(/1,2/)
   FID2CP(4,1:NUMBER_OF_POINTS_PER_FACE)=(/3,4/)

   PERM(1,1:NUMBER_OF_POINTS_PER_FACE)=(/1,2/)
   PERM(2,1:NUMBER_OF_POINTS_PER_FACE)=(/2,1/)

END IF
!!!!!!!!! STORING CORNER POINTS
IF (GLOBAL % AXSYM == 3) THEN
   DO V = 1, GITDIM
      DO B = 1,GLOBAL % NBLOCK
         nI = BLOCKS(B) % NPI
         nJ = BLOCKS(B) % NPJ
         nK = BLOCKS(B) % NPK
         CORNER_POINT(B,1,V) = BLOCKS(B) % XYZ( 1, 1, 1, V)
         CORNER_POINT(B,2,V) = BLOCKS(B) % XYZ(ni, 1, 1, V)
         CORNER_POINT(B,3,V) = BLOCKS(B) % XYZ( 1,nj, 1, V)
         CORNER_POINT(B,4,V) = BLOCKS(B) % XYZ(ni,nj, 1, V)
         CORNER_POINT(B,5,V) = BLOCKS(B) % XYZ( 1, 1,nk, V)
         CORNER_POINT(B,6,V) = BLOCKS(B) % XYZ(ni, 1,nk, V)
         CORNER_POINT(B,7,V) = BLOCKS(B) % XYZ( 1,nj,nk, V)
         CORNER_POINT(B,8,V) = BLOCKS(B) % XYZ(ni,nj,nk, V)
      END DO
   END DO

ELSE
   DO V = 1, GITDIM
      DO B = 1,GLOBAL % NBLOCK
         nI = BLOCKS(B) % NPI
         nJ = BLOCKS(B) % NPJ
         CORNER_POINT(B,1,V) = BLOCKS(B) % XYZ( 1, 1, 1, V)
         CORNER_POINT(B,2,V) = BLOCKS(B) % XYZ(ni, 1, 1, V)
         CORNER_POINT(B,3,V) = BLOCKS(B) % XYZ( 1,nj, 1, V)
         CORNER_POINT(B,4,V) = BLOCKS(B) % XYZ(ni,nj, 1, V)
      END DO
   END DO
END IF

FOUND = .TRUE.
WRITE(*,'(4(A5,X),6(A2,X))') "BLOCK","NCI","NCJ","NCK",FACES(1:NUMBER_OF_FACES)
DO B = 1,GLOBAL % NBLOCK  ! LOOP OVER ALL BLOCKS
   WRITE(*,'(4(I5,X))',ADVANCE="NO") B, BLOCKS(B) % NCI, BLOCKS(B) % NCJ, BLOCKS(B) % NCK
   BLOCKS(B) % BLOCK_CONNECTION = -1
   DO F = 1, NUMBER_OF_FACES
      DO BB = 1,GLOBAL % NBLOCK! LOOP OVER ALL BLOCK AGAIN
         FOUND = .FALSE.
         IF (b == bb) CYCLE
         DO FF = 1,NUMBER_OF_FACES
            DO P = 1, NUMBER_OF_PERMUTATIONS
               FOUND = .TRUE.
               DO FP = 1, NUMBER_OF_POINTS_PER_FACE
                  DO V = 1, GITDIM
!                        WRITE(*,*) B,FID2CP(F,FP),V, BB,FID2CP(FF,PERM(P,FP)),V
!                        CORNER_POINT(B,FID2CP(F,FP),V)
                     IF (ABS(CORNER_POINT(B,FID2CP(F,FP),V) - CORNER_POINT(BB,FID2CP(FF,PERM(P,FP)),V)) > EPS) THEN
                        FOUND = .FALSE.
                        EXIT
                     END IF
                  END DO
                  IF (.NOT.FOUND) THEN
                     EXIT
                  END IF
               END DO
               IF (FOUND) THEN
                  BLOCKS(B) % BLOCK_CONNECTION(F,1) = BB
                  BLOCKS(B) % BLOCK_CONNECTION(F,2) = FF
                  BLOCKS(B) % BLOCK_CONNECTION(F,3) = P
                  EXIT
               END IF
            END DO
            IF (FOUND) THEN
               EXIT
            END IF
         END DO
         IF (FOUND) THEN

            EXIT


         END IF
      END DO
      IF (FOUND) THEN
         WRITE(*,'(I2,X)',ADVANCE="NO") BB
      ELSE
         WRITE(*,'(A2,X)',ADVANCE="NO") "--"
      END IF
   END DO
   WRITE(*,*)
END DO


CLOSE(GIT_UNIT)
DEALLOCATE( CORNER_POINT)
DEALLOCATE( FID2CP )
DEALLOCATE( PERM)

END SUBROUTINE

SUBROUTINE READ_SOL_HEADER()
USE MOD_GLOBAL
IMPLICIT NONE

INTEGER :: IO_FILE_VERSION, DIMENSION_SOL, NBLOCK_SOL

INTEGER :: TNI, TNJ, TNK,B

OPEN(IO_SOL,FILE=TRIM(GLOBAL % SOL_IN),FORM="UNFORMATTED",access="STREAM",STATUS="OLD")

READ(IO_SOL) IO_FILE_VERSION,DIMENSION_SOL,NBLOCK_SOL,GLOBAL % NVAR

   IF (GLOBAL % AXSYM /= DIMENSION_SOL) THEN
      WRITE(*,*) "ERROR: DIMENSIONEN STIMMEN NICHT ÜBEREIN"
      STOP
   END IF

   IF (GLOBAL % NBLOCK /= NBLOCK_SOL ) THEN
      WRITE(*,*) "ERROR: BLOCKANZAHL STIMMEN NICHT ÜBEREIN"
      STOP
   END IF
DO B = 1, GLOBAL % NBLOCK
   READ(IO_SOL)  TNI,  TNJ,  TNK
   IF (TNI /= BLOCKS(B) % NCI)  THEN
      STOP "NI STIMMT NICHT ÜBEREIN"
   END IF
   IF (TNJ /= BLOCKS(B) % NCJ)  THEN
      STOP "NJ STIMMT NICHT ÜBEREIN"
   END IF
   IF (TNK /= BLOCKS(B) % NCK)  THEN
      STOP "NK STIMMT NICHT ÜBEREIN"
   END IF
   ALLOCATE( BLOCKS(B) % VAL(TNI,  TNJ,  TNK,GLOBAL % NVAR))
END DO
END SUBROUTINE READ_SOL_HEADER

SUBROUTINE READ_SOL(ITER,NEW_SOL)
USE MOD_GLOBAL
IMPLICIT NONE
LOGICAL, INTENT(OUT) :: NEW_SOL
INTEGER, INTENT(OUT) :: ITER

REAL(KIND=8) :: MAX_RES, AVG_RES

INTEGER :: status, B

NEW_SOL = .FALSE.


READ(IO_SOL,IOSTAT=status) ITER, MAX_RES, AVG_RES
if(status.lt.0) then
   NEW_SOL = .FALSE.
ELSE
   WRITE(*,*) "ITERATION:", ITER
   DO B = 1,GLOBAL % NBLOCK
         READ(IO_SOL) BLOCKS(B) % VAL
   END DO
   NEW_SOL = .TRUE.
END IF

END SUBROUTINE READ_SOL

SUBROUTINE STR2UNSTR()
   USE MOD_GLOBAL
   IMPLICIT NONE
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !
   !                             LOCALE VARIABLEN
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   INTEGER :: N,I,J,K,U,L
   LOGICAL :: IS_OLD_NODE,DO_CONNECT_I,DO_CONNECT_J
   INTEGER :: MYPKT, MYKNT, NPKT

   INTEGER :: I_U,I_D,J_U,J_D

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

   IF (GLOBAL % AXSYM == 3) THEN
      NUMBER_OF_FACES = 6
   ELSE
      NUMBER_OF_FACES = 4
   END IF

   IF (GLOBAL % AXSYM ==3) THEN
      WRITE(*,*) "3D NOCH NICHT UNTERSTÜTZT","unstrukt"
      STOP
   ELSE IF (GLOBAL % AXSYM == 1) THEN
      UNSTR%NPKT = 0
      UNSTR%NKNT = 0

      UNSTR%NCELL = 0
      DO N=1,GLOBAL%NBLOCK
         UNSTR%NCELL = UNSTR%NCELL + BLOCKS(N) % NCI
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
         UNSTR%NCELL = UNSTR%NCELL + BLOCKS(N) % NCI * BLOCKS(N) % NCJ * BLOCKS(N) % NCK
      END DO
   END IF

   ALLOCATE( UNSTR%XYZ        (UNSTR%NPKT,3))
   ALLOCATE( UNSTR%PKT_TYPE   (UNSTR%NPKT))
   ALLOCATE( UNSTR%PKT_NKNT   (UNSTR%NPKT))
   ALLOCATE( UNSTR%PKT_KNT    (UNSTR%NPKT,6))
   ALLOCATE( UNSTR%PKT_NEIGH  (UNSTR%NPKT,6))
   ALLOCATE( UNSTR%PKT_REF    (UNSTR%NPKT,4))

   ALLOCATE( UNSTR%KNT        (UNSTR%NKNT,2))

   ALLOCATE ( UNSTR%CELL      (UNSTR%NCELL,8))

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
   BLOCK_LOOP: DO WHILE (BLOCKS_TO_PROCESS > 0)
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
            IF ( I == 1 .AND. BLOCKS(N) % BLOCK_CONNECTION(1,1) /= -1 &
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
            ELSE IF ( I == BLOCKS(N) % NPI .AND. BLOCKS(N) % BLOCK_CONNECTION(2,1) /= -1 &
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
            ELSE IF ( J == 1 .AND. BLOCKS(N) % BLOCK_CONNECTION(3,1) /= -1) THEN
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
                  IF (L /= -1 .AND. U /= -1) THEN
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
                     IF (U /= -1 .AND. L /= -1 ) THEN ! .AND. U == L
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
            IF (DO_CONNECT_I .AND. I > 1) THEN ! .AND. .NOT.(J == 1 .AND. BLOCKS(N) % BLOCK_CONNECTION(3,1) /= -1)) THEN
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

            IF (DO_CONNECT_J .AND. J > 1) THEN! .AND. .NOT.(I == 1 .AND. BLOCKS(N) % BLOCK_CONNECTION(1,1) /= -1)) THEN
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
         IF (NACHBAR /= -1) THEN

            IF (BLOCKS_STATUS(NACHBAR) == 0 ) THEN
               BLOCKS_TO_PROCESS = BLOCKS_TO_PROCESS + 1
               BLOCKS_LIST(BLOCKS_TO_PROCESS) = NACHBAR
               BLOCKS_STATUS(NACHBAR) = 1
               IF (GLOBAL%DBG >= 1) &
               WRITE(*,'("FÜGE ",I0,"(",A,") AN STELLE ",I0)') NACHBAR,FACES(F),BLOCKS_TO_PROCESS

            END IF
         END IF
      END DO
   END DO BLOCK_LOOP

   DO I = 1,GLOBAL % NBLOCK
      IF (BLOCKS_STATUS(I) /= 2) THEN
         WRITE(*,*) "BLOCK",I,"WURDE NOCH NICHT ABGEARBEITET"
         STOP
      END IF
   END DO

   DEALLOCATE ( BLOCKS_LIST )
   DEALLOCATE ( BLOCKS_STATUS )
   L = 0
   DO N = 1,GLOBAL % NBLOCK
      DO I = 1, BLOCKS(N) % NCI
         DO J = 1,BLOCKS(N) % NCJ
            DO K = 1,BLOCKS(N) % NCK
               L = L + 1
               UNSTR % CELL(L,1) = BLOCKS(N) % ASSOC(I  ,J  ,K)
               UNSTR % CELL(L,2) = BLOCKS(N) % ASSOC(I+1,J  ,K)
               UNSTR % CELL(L,3) = BLOCKS(N) % ASSOC(I+1,J+1,K)
               UNSTR % CELL(L,4) = BLOCKS(N) % ASSOC(I  ,J+1,K)
               UNSTR % CELL(L,5) = I
               UNSTR % CELL(L,6) = J
               UNSTR % CELL(L,7) = K
               UNSTR % CELL(L,8) = N
            END DO
         END DO
      END DO
   END DO



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
IF (GLOBAL % AXSYM == 3) THEN
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
         WRITE(*,*) "WARNUNG: RANDPUNKT KANN SOWOHL IN X ALSO AUCH Y VERSCHOBEN WERDEN: WIRD FESTGESETZT"
         WRITE(*,*) "PKT,B,I,J,K:",U,UNSTR % PKT_REF(U,4),UNSTR % PKT_REF(U,1),UNSTR % PKT_REF(U,2),UNSTR % PKT_REF(U,3)
         WRITE(*,*) "KOORDINATEN:", UNSTR % XYZ(U,:)
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
SUBROUTINE PARAVIEW_OUTPUT()
    use mod_global
    implicit none
CHARACTER(LEN=20) :: FILENAME
    INTEGER :: I,J,K,u,B
   DO B = 1,GLOBAL % NBLOCK
!    OPEN(10,FILE="paraview.vtk")
    WRITE(FILENAME,'(A,I0,A)') "paraview_",B,".vtk"
    OPEN(10,FILE=FILENAME)
    WRITE(10,"(A)") '# vtk DataFile Version 2.0'
    WRITE(10,"(A,I0)") 'GRID-ADAPTION in PARAVIEW BLOCK:',B
    WRITE(10,"(A)") 'ASCII'
    WRITE(10,"(A)") ""
    WRITE(10,"(A)") 'DATASET STRUCTURED_GRID'

       WRITE(10,"(A,3I4)") 'DIMENSIONS ',BLOCKS(B)%NPI,BLOCKS(B)%NPJ,BLOCKS(B)%NPK
       WRITE(10,"(A,I8,X,A)") 'POINTS ',BLOCKS(B)%NPI*BLOCKS(B)%NPJ*BLOCKS(B)%NPK,'double'
       DO K = 1,BLOCKS(B)%NPK
           DO j = 1,BLOCKS(B)%NPJ
              DO I = 1,BLOCKS(B)%NPI
                u = BLOCKS(B)% ASSOC(i,j,k)
                 WRITE(10,*) BLOCKS(B)%XYZ(i,j,k,1),BLOCKS(B)%XYZ(i,j,k,2),BLOCKS(B)%XYZ(i,j,k,3)
                 !,DBLE(UNSTR%PKT_NKNT(U))
                 !!UNSTR%PKT_VAR(U,1)!
              END DO
           END DO
       END DO
       write(10,*) 'POINT_DATA', BLOCKS(B)%NPI*BLOCKS(B)%NPJ*BLOCKS(B)%NPK
!       write(10,*) 'SCALARS NUMBER_KNOTEN double 1'
!       write(10,*) 'LOOKUP_TABLE default'
!       DO K = 1,BLOCKS(B)%NPK
!          DO j = 1,BLOCKS(B)%NPJ
!             DO I = 1,BLOCKS(B)%NPI
!                u = BLOCKS(B)% ASSOC(i,j,k)
!                write(10,*) DBLE(UNSTR%PKT_NKNT(U))!UNSTR%PKT_VAR(U,1)
!             END DO
!          END DO
!       END DO
!       write(10,*) 'POINT_DATA', BLOCKS(B)%NPI*BLOCKS(B)%NPJ*BLOCKS(B)%NPK
       write(10,*) 'SCALARS PKT_TYPE double 1'
       write(10,*) 'LOOKUP_TABLE default'
       DO K = 1,BLOCKS(B)%NPK
          DO j = 1,BLOCKS(B)%NPJ
             DO I = 1,BLOCKS(B)%NPI
                u = BLOCKS(B)% ASSOC(i,j,k)
                write(10,*) DBLE(UNSTR%PKT_TYPE(U))!UNSTR%PKT_VAR(U,1)
             END DO
          END DO
       END DO
   CLOSE (10)
    END DO
end subroutine

SUBROUTINE PARAVIEW_OUTPUT_UNSTR(ITER)
   use mod_global
   implicit none

   INTEGER :: ITER
   INTEGER :: I,N
   CHARACTER(LEN=20) :: FILENAME

   IF ( ITER >= 0 ) THEN
      WRITE(FILENAME,'(A,I0,A)') "paraview_",ITER,".vtk"
   ELSE
      FILENAME = "paraview.vtk"
   END IF
   OPEN(10,FILE=FILENAME)

   WRITE(10,"(A)") '# vtk DataFile Version 2.0'
   WRITE(10,"(A)") 'GRID-ADAPTION Unstructured_Grid'
   WRITE(10,"(A)") 'ASCII'
   WRITE(10,"(A)") ""
   WRITE(10,"(A)") "DATASET UNSTRUCTURED_GRID"
   WRITE(10,"(A,X,I0,X,A)") "POINTS",UNSTR % NPKT,"float"
   IF (GLOBAL % AXSYM == 3) THEN
      DO I = 1, UNSTR % NPKT
         WRITE(10,'(3(F20.13,X))') UNSTR % XYZ(I,1), UNSTR % XYZ(I,2), UNSTR % XYZ(I,3)
      END DO
   ELSE
      DO I = 1, UNSTR % NPKT
         WRITE(10,'(3(F20.13,X))') UNSTR % XYZ(I,1), UNSTR % XYZ(I,2), 0.0D0
      END DO
   END IF
   WRITE(10,*)
   WRITE(10,"(A,X,I0,X,I0)") "CELLS", UNSTR % NCELL, UNSTR % NCELL*5
   DO I = 1, UNSTR % NCELL
      WRITE(10,'(5(I0,X))') 4,(UNSTR % CELL(I,N)-1,N=1,4)
   END DO
   WRITE(10,*)
   WRITE(10,"(A,X,I0)") "CELL_TYPES", UNSTR % NCELL
   DO I = 1, UNSTR % NCELL
      WRITE(10,'(I0)') 9
   END DO
   WRITE(10,*)
   WRITE(10,"(A,X,I0)") "CELL_DATA",UNSTR % NCELL
   WRITE(10,"(A)") 'SCALARS TEMPERATUR float'
   WRITE(10,"(A)") 'LOOKUP_TABLE default'
   DO I = 1, UNSTR % NCELL
      WRITE(10,'(3(F20.13,X))') BLOCKS(UNSTR%CELL(I,8)) % VAL(UNSTR%CELL(I,5),UNSTR%CELL(I,6),UNSTR%CELL(I,7),1)
   END DO
!   WRITE(10,"(A)") 'SCALARS PKT_NKNT int'
!   WRITE(10,"(A)") 'LOOKUP_TABLE default'
!   DO I = 1, UNSTR % NPKT
!      WRITE(10,'(I0)') UNSTR%PKT_NKNT(I)
!   END DO

   CLOSE(10)
END SUBROUTINE

