module IO
use CONST, ONLY: DP
implicit none
contains
   SUBROUTINE INPUT(IS_PARALLEL)
   USE MOD_GLOBAL
   USE CONST
   use wall_refinement, only: input_wall_refinement
   IMPLICIT NONE
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !
   !                             PARAMETER
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   LOGICAL :: IS_PARALLEL                                                  ! DEFINES IF THE INPUT IS PARALLEL (NUMBERS APPENDED TO FILES)


#ifdef DEBUG
      IF (GLOBAL%DBG == 1)  THEN
         WRITE(*,*) "======================================================" &
                   ,"INPUT START"  &
                   ,"======================================================"

      END IF
#endif

      IF (GLOBAL % FILE_TYPE == 0 ) THEN
         CALL INPUT_GRID_OLD(IS_PARALLEL)
      ELSE
         CALL INPUT_GRID()
         CALL INPUT_SOL()
      END IF

      IF (GLOBAL % DO_WALL_REFINEMENT == 1) THEN
!         CALL INPUT_RANDBED() !EINLESEN DER RANDBEDINGUNGEN
         call input_wall_refinement()
      END IF

#ifdef DEBUG
      IF (GLOBAL%DBG == 1)  THEN
         WRITE(*,*) "======================================================" &
                   ,"INPUT ENDE"  &
                   ,"======================================================"

      END IF
#endif

   END SUBROUTINE
   SUBROUTINE INPUT_CONTROL()
   USE MOD_GLOBAL
   USE CONST
   use utils, only: is_integer,lower_case,is_real
   IMPLICIT NONE

   CHARACTER(LEN= 1000) :: LINE
   CHARACTER(LEN= 50) :: VARNAME,VARVALUE
   INTEGER :: ISTAT
   INTEGER :: POS
   INTEGER :: nArg,i
   CHARACTER(LEN = 100) :: PARAM
   CHARACTER(LEN=100) :: CONTROL_FILE
   INTEGER, PARAMETER :: IO_CF = 20
   LOGICAL :: FEXISTS

   CONTROL_FILE = "input.cfg"

   GLOBAL % GIT_IN             = "git.dat"
   GLOBAL % SOL_IN             = "avarout.ufo"
   GLOBAL % GIT_OUT            = "git_out.dat"
   GLOBAL % SOL_OUT            = "avarout_out.ufo"
   GLOBAL % DBG                = 0
   GLOBAL % NITER              = 0
   GLOBAL % FAKTOR             = 0.5D0
   GLOBAL % FILE_TYPE          = 1
   GLOBAL % OUTPUT_TYPE        = 0
   GLOBAL % OUTPUT_ANIMATION   = 0
   GLOBAL % CHECK_FOR_DOUBLE_POINTS = 1
   GLOBAL % CHECK_FOR_DOUBLE_KANTEN = 1
   GLOBAL % DO_WALL_REFINEMENT = 0
   GLOBAL % RANDBED_IN         = "randbed.dat"
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !
   !                            KOMMANDOZEILE AUSLESEN
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   nArg=COMMAND_ARGUMENT_COUNT()

   DO i = 1,nArg
      CALL GET_COMMAND_ARGUMENT(I,PARAM)
      IF (lower_case(PARAM) == "-debug") THEN
         Global%DBG = 1
      ELSE IF (lower_case(PARAM(1:5)) == "-cfg=") THEN
         CONTROL_FILE = PARAM(6:)
      ELSE
         WRITE(*,*) "KOMMANDOZEILEN PARAMTER: ",TRIM(PARAM)
         STOP "KOMMANDO NICHT VERSTANDEN"
      END IF
   END DO

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !
   !                         CONFIGURATIONSDATEI AUSLESEN
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   INQUIRE(FILE=TRIM(CONTROL_FILE),EXIST=FEXISTS)
   IF(FEXISTS .EQV. .FALSE.) THEN
      WRITE(*,*) "CONTROL FILE konnte nicht gefunden werden: " // TRIM(CONTROL_FILE)
      STOP
   END IF
   OPEN(IO_CF,FILE=CONTROL_FILE,STATUS="OLD")

   DO
      READ(IO_CF,'(A1000)',IOSTAT = ISTAT) LINE
      IF (ISTAT < 0 ) THEN
   !      WRITE(*,'(A)') "ENDE DER CONTROL-DATEI ERREICHT"
         EXIT
      END IF

      !! KOMMENTARE UND LEERZEILEN ENTFERNEN
      LINE = TRIM(ADJUSTL(LINE))
      POS = INDEX(LINE,"!")
      IF (POS > 0 ) THEN
         LINE = LINE(1:POS-1)
      END IF
      IF (LEN_TRIM(LINE) == 0) CYCLE

      !! RESTLICHEN LINIEN DURCHSUCHEN

      POS = INDEX(LINE,"=")

      IF (POS < 1) THEN
         WRITE(*,'("KONNTE LINE NICHT VERARBEITEN, KEIN = GEFUNDEN:",A)') TRIM(LINE)
         STOP
      END IF
   !   WRITE(*,*) TRIM(LINE(1:POS-1)),TRIM(LINE(POS+1:))
      VARNAME = lower_case(TRIM(LINE(1:POS-1)))
      VARVALUE = TRIM(ADJUSTL(LINE(POS+1:)))
      SELECT CASE(TRIM(VARNAME))
      CASE ("debug")
         IF (IS_INTEGER(VARVALUE)) THEN
            IF (Global%DBG == 0) THEN
               READ(VARVALUE,*) Global%DBG
            END IF
         ELSE
            WRITE(*,*) "WERT FÜR DEBUG IS KEIN INTEGER"
            STOP
         END IF
      CASE ("niter")
         IF (IS_INTEGER(VARVALUE)) THEN
            READ(VARVALUE,*) Global%NITER
         ELSE
            WRITE(*,*) "WERT FÜR NITER IS KEIN INTEGER"
            STOP
         END IF
      CASE ("move_faktor")
         IF (IS_REAL(VARVALUE)) THEN
            READ(VARVALUE,*) Global%FAKTOR
         ELSE
            WRITE(*,*) "WERT FÜR MOVE_FAKTOR IS KEIN REAL",VARVALUE
            STOP
         END IF
      CASE ("git_in")
         Global%GIT_IN = VARVALUE
      CASE ("sol_in")
         Global%SOL_IN = VARVALUE
      CASE ("git_out")
         Global%GIT_OUT = VARVALUE
      CASE ("sol_out")
         Global%SOL_OUT = VARVALUE
      CASE ("file_type")
         IF (IS_INTEGER(VARVALUE)) THEN
            READ(VARVALUE,*) Global % FILE_TYPE
         ELSE
            WRITE(*,*) "WERT FÜR FILE_TYPE IS KEIN INTEGER"
            STOP
         END IF
      CASE ("output_type")
         IF (IS_INTEGER(VARVALUE)) THEN
            READ(VARVALUE,*) Global % OUTPUT_TYPE
         ELSE
            WRITE(*,*) "WERT FÜR OUTPUT_TYPE IS KEIN INTEGER"
            STOP
         END IF
      CASE ("output_animation")
         IF (IS_INTEGER(VARVALUE)) THEN
            READ(VARVALUE,*) Global % OUTPUT_ANIMATION
         ELSE
            WRITE(*,*) "WERT FÜR OUTPUT_ANIMATION IS KEIN INTEGER"
            STOP
         END IF
      CASE ("check_for_double_points")
         IF (IS_INTEGER(VARVALUE)) THEN
            READ(VARVALUE,*) Global % CHECK_FOR_DOUBLE_POINTS
         ELSE
            WRITE(*,*) "WERT FÜR CHECK_FOR_DOUBLE_POINTS IS KEIN INTEGER"
            STOP
         END IF
      CASE ("check_for_double_kanten")
         IF (IS_INTEGER(VARVALUE)) THEN
            READ(VARVALUE,*) Global % CHECK_FOR_DOUBLE_KANTEN
         ELSE
            WRITE(*,*) "WERT FÜR CHECK_FOR_DOUBLE_KANTEN IS KEIN INTEGER"
            STOP
         END IF
      CASE ("do_wall_refinement")
         IF (IS_INTEGER(VARVALUE)) THEN
            READ(VARVALUE,*) Global % DO_WALL_REFINEMENT
         ELSE
            WRITE(*,*) "WERT FÜR DO_WALL_REFINEMENT IS KEIN INTEGER"
            STOP
         END IF
      CASE ("randbed_in")
         GLOBAL % RANDBED_IN = VARVALUE
      CASE DEFAULT
         WRITE(*,'(A)') "LINIE NICHT ERKANNT: "//TRIM(LINE)
         STOP
      END SELECT



   END DO
   CLOSE(IO_CF)


   IF (Global%DBG == 1) THEN
      WRITE(*,"(A)") " ================= DEBUG MODE ==================="
   END IF

   END SUBROUTINE


   SUBROUTINE INPUT_SOL()
   USE MOD_GLOBAL
   USE CONST
   IMPLICIT NONE
   INTEGER, PARAMETER :: F_SOL = 10
   INTEGER :: N
   LOGICAL :: FEXISTS

   IF (GLOBAL%DBG == 1) THEN
      WRITE(*,'(2A,X,A)') "OPENING ",TRIM(GLOBAL % SOL_IN)
   END IF
   INQUIRE(FILE=TRIM(GLOBAL% SOL_IN),EXIST=FEXISTS)
   IF(FEXISTS .EQV. .FALSE.) THEN
      WRITE(*,*) "SOLUTION DATEI konnte nicht gefunden werden: " // TRIM(GLOBAL% SOL_IN)
      GLOBAL%NVAR = 1
      ALLOCATE( GLOBAL % VNAME (GLOBAL%NVAR) )
      GLOBAL % VNAME(1) = "DUMMY"
      DO N  = 1, GLOBAL % NBLOCK

         ALLOCATE( BLOCKS(N) % CVAR  (0:BLOCKS(N)%NPI,0:BLOCKS(N)%NPJ,0:BLOCKS(N)%NPK,GLOBAL%NVAR) )
         BLOCKS(N) % CVAR = 1.0D0
      END DO
   ELSE
      STOP "SOLUTION MPPIO DATEI ÖFFNEN NOCH NCIHT IMPLEMENTIERT"
      open(F_SOL,FILE=TRIM(GLOBAL% SOL_IN),FORM="UNFORMATTED",access="STREAM",STATUS="OLD")
      close(F_SOL)
   END IF

   END SUBROUTINE
   SUBROUTINE INPUT_GRID_OLD(IS_PARALLEL)
   USE MOD_GLOBAL
   USE CONST
   IMPLICIT NONE
   INTEGER, PARAMETER :: F_GIT = 10
   INTEGER, PARAMETER :: F_VAR = 11
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !
   !                             PARAMETER
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   LOGICAL :: IS_PARALLEL   !DEFINES IF THE INPUT IS PARALLEL (NUMBERS APPENDED TO FILES)
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !
   !                             LOCALE VARIABLEN
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   INTEGER :: N,V,I,J,K

   IF (IS_PARALLEL) THEN
      WRITE(*,*) "PARALLEL INPUT NOT SUPPORTED YET"
      STOP
   ELSE
      OPEN(F_GIT,FILE=TRIM(GLOBAL % GIT_IN),STATUS="OLD")
      OPEN(F_VAR,FILE=TRIM(GLOBAL % SOL_IN),STATUS="OLD",FORM="UNFORMATTED")

   !!             AVAROUT.ufo

      READ(F_VAR) GLOBAL%AXSYM, GLOBAL%NSTEP, GLOBAL%NBLOCK, GLOBAL%NVAR, GLOBAL%NBASEVAR2
#ifdef DEBUG
      WRITE(*,*) GLOBAL%AXSYM, GLOBAL%NSTEP, GLOBAL%NBLOCK, GLOBAL%NVAR, GLOBAL%NBASEVAR2
#endif
      ALLOCATE( GLOBAL % VNAME (GLOBAL%NVAR) )
      ALLOCATE(BLOCKS(GLOBAL%NBLOCK))

      DO N = 1,GLOBAL%NBLOCK
         READ(F_VAR) BLOCKS(N)%NCI,BLOCKS(N)%NCJ,BLOCKS(N)%NCK
         READ(F_VAR) BLOCKS(N)%IOUT,BLOCKS(N)%JOUT,BLOCKS(N)%KOUT
#ifdef DEBUG
         WRITE(*,'(A,3(X,I4,","))') "NCI,NCJ,NCK = ",BLOCKS(N)%NCI,BLOCKS(N)%NCJ,BLOCKS(N)%NCK
         WRITE(*,'(A,3(X,I4,","))') "IOUT,JOUT,KOUT = ",BLOCKS(N)%IOUT,BLOCKS(N)%JOUT,BLOCKS(N)%KOUT
#endif

         IF (GLOBAL%AXSYM == 2) THEN
            READ(F_GIT,*) BLOCKS(N)%NPI,BLOCKS(N)%NPJ,BLOCKS(N)%NPK
         ELSE
            READ(F_GIT,*) BLOCKS(N)%NPI,BLOCKS(N)%NPJ
            BLOCKS(N)%NPK = 1
         END IF

         IF ( BLOCKS(N)%NPI-1 /= BLOCKS(N)%NCI &
         .OR. (BLOCKS(N)%NPJ-1 /= BLOCKS(N)%NCJ .AND. GLOBAL%AXSYM /= -1) &
         .OR. (BLOCKS(N)%NPK-1 /= BLOCKS(N)%NCK .AND. GLOBAL%AXSYM == 2) &
         .OR. (BLOCKS(N)%NPK /= BLOCKS(N)%NCK .AND. GLOBAL%AXSYM /= 2 )   &
         .OR. (BLOCKS(N)%NPK /= BLOCKS(N)%NCK .AND. GLOBAL%AXSYM == -1)) THEN
            WRITE(*,*) "UNTERSCHIEDLICHER DIMENSIONEN IN AVAROUT.UFO UND GIT.DAT"
            WRITE(*,*) "AVAROUT:",BLOCKS(N)%NCI,BLOCKS(N)%NCJ,BLOCKS(N)%NCK
            WRITE(*,*) "    GIT:",BLOCKS(N)%NPI,BLOCKS(N)%NPJ,BLOCKS(N)%NPK
            STOP
         END IF


         ALLOCATE( BLOCKS(N) % CVAR  (0:BLOCKS(N)%NPI,0:BLOCKS(N)%NPJ,0:BLOCKS(N)%NPK,GLOBAL%NVAR) )

         ALLOCATE( BLOCKS(N) % XYZ   (BLOCKS(N)%NPI,BLOCKS(N)%NPJ,BLOCKS(N)%NPK,3) )
         DO V = 1,GLOBAL%NVAR
            READ(F_VAR) GLOBAL % VNAME(V)
#ifdef DEBUG1
            WRITE(*,*) V,GLOBAL % VNAME(V)
#endif
            READ(F_VAR) (((BLOCKS(N)%CVAR(i,j,k,v),i=1,BLOCKS(N)%NCI) &
                                                  ,j=1,BLOCKS(N)%NCJ) &
                                                  ,k=1,BLOCKS(N)%NCK)
         END DO


         IF (GLOBAL%AXSYM .EQ. 2) THEN
            DO k = 1, BLOCKS(N)%NPK
               DO j = 1,BLOCKS(N)%NPJ
                  DO i= 1,BLOCKS(N)%NPI
                     READ(F_GIT,*) BLOCKS(N)%XYZ(i,j,k,1),BLOCKS(N)%XYZ(i,j,k,2),BLOCKS(N)%XYZ(i,j,k,3)

                  END DO
               END DO
            END DO
         ELSE
            k = 1
            DO j = 1,BLOCKS(N)%NPJ
               DO i= 1,BLOCKS(N)%NPI
                  READ(F_GIT,*) BLOCKS(N)%XYZ(i,j,k,1),BLOCKS(N)%XYZ(i,j,k,2)
                  BLOCKS(N)%XYZ(i,j,k,3) = 1.D+0
   !                  WRITE(*,*) I,J,BLOCKS(N)%XYZ(1,i,j,k),BLOCKS(N)%XYZ(2,i,j,k)
               END DO
            END DO
         END IF
      END DO

      CLOSE(F_VAR)
      CLOSE(F_GIT)
   END IF
   END SUBROUTINE
   SUBROUTINE INPUT_GRID()
   USE MOD_GLOBAL
   USE CONST
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
   IF (GLOBAL % AXSYM == 2) THEN
      GITDIM = 3
   ELSE
      GITDIM = 2
   END IF

   ALLOCATE(BLOCKS(GLOBAL%NBLOCK))

   IF (GLOBAL%DBG == 1) WRITE(*,'(4(A5,X))') "BLOCK","NCI","NCJ","CNK"

   DO B = 1,GLOBAL % NBLOCK
      IF (GLOBAL % AXSYM == 2) THEN
         READ(GIT_UNIT) BLOCKS(B) % NPI, BLOCKS(B) % NPJ, BLOCKS(B) % NPK
         BLOCKS(B) % NCI = BLOCKS(B) % NPI - 1
         BLOCKS(B) % NCJ = BLOCKS(B) % NPJ - 1
         BLOCKS(B) % NCK = BLOCKS(B) % NPK - 1
      ELSE
         READ(GIT_UNIT) BLOCKS(B) % NPI, BLOCKS(B) % NPJ
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
   IF(GLOBAL % AXSYM == 2) THEN
      WRITE(*,'("3D SIMULATION")')
   ELSE IF (GLOBAL % AXSYM == 1) THEN
      WRITE(*,'("2D ROTATIONSYMETRISCHE SIMULATION")')
   ELSE
      WRITE(*,'("2D SIMULATION")')
   END IF
   WRITE(*,'("BLOCKS ON FILE:         ",I0)') GLOBAL % NBLOCK

   DO B = 1,GLOBAL % NBLOCK

   END DO

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!!!!!!!!!!              INITIALISIERUNGEN FÜR DIE BLOCK CONNECTIONS        !!!!!!!!!!!!
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   IF (GLOBAL % AXSYM == 2) THEN
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
   IF (GLOBAL % AXSYM == 2) THEN

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
   IF (GLOBAL % AXSYM == 2) THEN
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
   !WRITE(*,'(4(A5,X),6(A2,X))') "BLOCK","NCI","NCJ","NCK",FACES(1:NUMBER_OF_FACES)
   DO B = 1,GLOBAL % NBLOCK  ! LOOP OVER ALL BLOCKS
   !   WRITE(*,'(4(I5,X))',ADVANCE="NO") B, BLOCKS(B) % NCI, BLOCKS(B) % NCJ, BLOCKS(B) % NCK
      BLOCKS(B) % BLOCK_CONNECTION = -1000
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
   !      IF (FOUND) THEN
   !         WRITE(*,'(I2,X)',ADVANCE="NO") BB
   !      ELSE
   !         WRITE(*,'(A2,X)',ADVANCE="NO") "--"
   !      END IF
      END DO
      WRITE(*,*)
   END DO


   CLOSE(GIT_UNIT)
   DEALLOCATE( CORNER_POINT)
   DEALLOCATE( FID2CP )
   DEALLOCATE( PERM)

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
      INTEGER :: I
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
      IF (GLOBAL % AXSYM == 2) THEN
         DO I = 1, UNSTR % NPKT
            WRITE(10,'(3(F20.13,X))') UNSTR % XYZ(I,1), UNSTR % XYZ(I,2), UNSTR % XYZ(I,3)
         END DO
      ELSE
         DO I = 1, UNSTR % NPKT
            WRITE(10,'(3(F20.13,X))') UNSTR % XYZ(I,1), UNSTR % XYZ(I,2), 0.0D0
         END DO
      END IF
      WRITE(10,*)
      WRITE(10,"(A,X,I0,X,I0)") "CELLS", UNSTR % NKNT, UNSTR % NKNT*3
      DO I = 1, UNSTR % NKNT
         WRITE(10,'(3(I0,X))') 2,UNSTR % KNT(I,1)-1,UNSTR % KNT(I,2)-1
      END DO
      WRITE(10,*)
      WRITE(10,"(A,X,I0)") "CELL_TYPES", UNSTR % NKNT
      DO I = 1, UNSTR % NKNT
         WRITE(10,'(I0)') 3
      END DO
      WRITE(10,*)
      WRITE(10,"(A,X,I0)") "POINT_DATA",UNSTR % NPKT
      WRITE(10,"(A)") 'SCALARS PKT_TYPE int'
      WRITE(10,"(A)") 'LOOKUP_TABLE default'
      DO I = 1, UNSTR % NPKT
         WRITE(10,'(I0)') UNSTR%PKT_TYPE(I)
      END DO
      WRITE(10,"(A)") 'SCALARS PKT_NKNT int'
      WRITE(10,"(A)") 'LOOKUP_TABLE default'
      DO I = 1, UNSTR % NPKT
         WRITE(10,'(I0)') UNSTR%PKT_NKNT(I)
      END DO
      WRITE(10,"(A,X,I0)") "CELL_DATA",UNSTR % NKNT
      WRITE(10,"(A)") 'SCALARS KANTENLAENGE double'
      WRITE(10,"(A)") 'LOOKUP_TABLE default'
      DO I = 1, UNSTR % NKNT
         WRITE(10,*) UNSTR%KNT_DN(I,1)
      END DO
      WRITE(10,"(A)") 'SCALARS KANTENLAENGE_X double'
      WRITE(10,"(A)") 'LOOKUP_TABLE default'
      DO I = 1, UNSTR % NKNT
         WRITE(10,*) UNSTR%KNT_DN(I,2)
      END DO
      WRITE(10,"(A)") 'SCALARS KANTENLAENGE_Y double'
      WRITE(10,"(A)") 'LOOKUP_TABLE default'
      DO I = 1, UNSTR % NKNT
         WRITE(10,*) UNSTR%KNT_DN(I,2)
      END DO
      WRITE(10,"(A)") 'SCALARS KANTENSPANNUNG double'
      WRITE(10,"(A)") 'LOOKUP_TABLE default'
      DO I = 1, UNSTR % NKNT
         WRITE(10,*) UNSTR%KNT_SPANNUNG(I,1)
      END DO
      CLOSE(10)
   END SUBROUTINE

   SUBROUTINE OUTPUT_1D(iter)
      use mod_global
      implicit none

      INTEGER :: iter

      INTEGER :: i

      CHARACTER(LEN=20) :: FILENAME

      WRITE(filename,'(A,I0,A)') "1Dout_",iter,".dat"


      OPEN(66,FILE=filename)

      DO I = 1, UNSTR % NPKT
         WRITE(66,*) UNSTR%XYZ(I,1),",",UNSTR%PKT_VAR(I,1)
      END DO



      CLOSE(66)
   END SUBROUTINE

   SUBROUTINE WRITE_TECPLOT(FILENAME)
   USE MOD_GLOBAL

   IMPLICIT NONE
   CHARACTER(LEN=*) :: FILENAME
   INTEGER, PARAMETER :: OUTUNIT = 10
   INTEGER :: N, V, I, J, K

   OPEN(OUTUNIT,FILE=TRIM(FILENAME),STATUS="REPLACE")
   WRITE(OUTUNIT,'(A)',ADVANCE="NO") 'Variables = "X", "Y", "Z"'
   DO v=1,GLOBAL%NVAR
    WRITE(OUTUNIT,'(3A)',ADVANCE="NO") ', "',TRIM(GLOBAL%VNAME(V)),'"'
   END DO
   WRITE(OUTUNIT,'(/)',ADVANCE="NO")
   DO N = 1, GLOBAL%NBLOCK

      WRITE(OUTUNIT,'(A,I4,A,I4,A,I4)') 'ZONE I=',BLOCKS(N)%NPI,', J=',BLOCKS(N)%NPJ,', K=',BLOCKS(N)%NPK
      WRITE(OUTUNIT,'(A,I0,A)') 'DATAPACKING=BLOCK, VARLOCATION=([4-',GLOBAL%NVAR+4,']=CELLCENTERED)'
      WRITE(OUTUNIT,'(5(D20.13,1X))') (((BLOCKS(N)%XYZ(i,j,k,1),i=1,BLOCKS(N)%NPI)  &
                                                                  ,j=1,BLOCKS(N)%NPJ)  &
                                                                  ,k=1,BLOCKS(N)%NPK)
      WRITE(OUTUNIT,*)
      WRITE(OUTUNIT,'(5(D20.13,1X))') (((BLOCKS(N)%XYZ(i,j,k,2),i=1,BLOCKS(N)%NPI) &
                                                                  ,j=1,BLOCKS(N)%NPJ) &
                                                                  ,k=1,BLOCKS(N)%NPK)
      WRITE(OUTUNIT,*)
      WRITE(OUTUNIT,'(5(D20.13,1X))') (((BLOCKS(N)%XYZ(i,j,k,3),i=1,BLOCKS(N)%NPI) &
                                                                  ,j=1,BLOCKS(N)%NPJ) &
                                                                  ,k=1,BLOCKS(N)%NPK)
      WRITE(OUTUNIT,*)
      DO v=1,GLOBAL%NVAR
         WRITE(OUTUNIT,'(5(D20.13,1X))') (((BLOCKS(N)%CVAR(i,j,k,v),i=1,BLOCKS(N)%NCI) &
                                                                   ,j=1,BLOCKS(N)%NCJ) &
                                                                   ,k=1,BLOCKS(N)%NCK)
         WRITE(OUTUNIT,*)
         WRITE(OUTUNIT,'(5(D20.13,1X))') (((BLOCKS(N)%CVAR(i,j,k,v),i=1,BLOCKS(N)%NCI) &
                                                                   ,j=1,BLOCKS(N)%NCJ) &
                                                                   ,k=1,BLOCKS(N)%NCK)
         WRITE(OUTUNIT,*)
      END DO
   END DO
   END SUBROUTINE

   SUBROUTINE WRITE_TECPLOT_ANI(SOLTIME)
   USE MOD_GLOBAL

   IMPLICIT NONE
   INTEGER :: SOLTIME
   integer :: coord_var
   INTEGER, PARAMETER :: OUTUNIT = 10
   INTEGER :: N, V, I, J, K
   IF (SOLTIME <= 0) THEN
   OPEN(OUTUNIT,FILE="ani.plt",STATUS="REPLACE")

   WRITE(OUTUNIT,'(A)',ADVANCE="NO") 'Variables = "X", "Y"'

   if (global % axsym == 2) then
      WRITE(OUTUNIT,'(A)',ADVANCE="NO") ', "Z"'
   end if

   DO v=1,GLOBAL%NVAR
    WRITE(OUTUNIT,'(3A)',ADVANCE="NO") ', "',TRIM(GLOBAL%VNAME(V)),'"'
   END DO
   WRITE(OUTUNIT,'(/)',ADVANCE="NO")

   ELSE
   OPEN(OUTUNIT,FILE="ani.plt",STATUS="old")
   END IF

   if (global % axsym == 2) then
      coord_var = 4
   else
      coord_var = 3
   end if

   DO N = 1, GLOBAL%NBLOCK

      WRITE(OUTUNIT,'(A,I0,A,I4,A,I4,A,I4)') 'ZONE,SOLUTIONTIME=',SOLTIME &
      ,' I=',BLOCKS(N)%NPI,', J=',BLOCKS(N)%NPJ,', K=',BLOCKS(N)%NPK
      WRITE(OUTUNIT,'(A,I0,A,I0,A)',ADVANCE="NO") 'DATAPACKING=BLOCK, VARLOCATION=([',coord_var &
               ,'-',GLOBAL%NVAR+coord_var,']=CELLCENTERED)'

      do v = 1,coord_var-1
         DO k = 1,BLOCKS(n) %  NPK
            DO j = 1,BLOCKS(n) % NPJ
               DO i= 1,BLOCKS(n) % NPI
                  WRITE(OUTUNIT,'(5(D20.13,1X))') (BLOCKS(n) % XYZ(I,J,K,V))
               END DO
            END DO
         END DO
      end do

!      WRITE(OUTUNIT,'(5(D20.13,1X))') (UNSTR%XYZ(i,1),I = 1,UNSTR%NPKT)
!      WRITE(OUTUNIT,*)
!      WRITE(OUTUNIT,'(5(D20.13,1X))') (UNSTR%XYZ(i,2),I = 1,UNSTR%NPKT)
!      WRITE(OUTUNIT,*)
   !   WRITE(OUTUNIT,'(5(D20.13,1X))') (((BLOCKS(N)%XYZ(i,j,k,3),i=1,BLOCKS(N)%NPI) &
   !                                                               ,j=1,BLOCKS(N)%NPJ) &
   !                                                               ,k=1,BLOCKS(N)%NPK)
!      WRITE(OUTUNIT,'(5(D20.13,1X))')   (UNSTR%PKT_VAR(I,1),I = 1,UNSTR%NPKT)
!      WRITE(OUTUNIT,*)
      DO v=1,GLOBAL%NVAR
         WRITE(OUTUNIT,'(5(D20.13,1X))') (((BLOCKS(N)%CVAR(i,j,k,v),i=1,BLOCKS(N)%NCI) &
                                                                   ,j=1,BLOCKS(N)%NCJ) &
                                                                   ,k=1,BLOCKS(N)%NCK)
         WRITE(OUTUNIT,*)
!         WRITE(OUTUNIT,'(5(D20.13,1X))') (((BLOCKS(N)%CVAR(i,j,k,v),i=1,BLOCKS(N)%NCI) &
!                                                                   ,j=1,BLOCKS(N)%NCJ) &
!                                                                   ,k=1,BLOCKS(N)%NCK)
!         WRITE(OUTUNIT,*)
      END DO
   END DO
   END SUBROUTINE

   SUBROUTINE OUTPUT_GRID()
   USE MOD_GLOBAL
   USE CONST
   IMPLICIT NONE
   INTEGER, PARAMETER :: GIT_UNIT = 25


   INTEGER :: GITDIM

   INTEGER :: B,I,J,K,V


   WRITE(*,'(2A,X,A)') "WRITEING ",TRIM(GLOBAL % GIT_OUT)

   IF (GLOBAL % AXSYM == 2) THEN
      GITDIM = 3
   ELSE
      GITDIM = 2
   END IF

   OPEN(GIT_UNIT,FILE=TRIM(GLOBAL% GIT_OUT),FORM="UNFORMATTED",access="STREAM",STATUS="replace")

   WRITE(GIT_UNIT) GLOBAL %AXSYM,GLOBAL % NBLOCK

   DO B = 1,GLOBAL % NBLOCK
      IF (GLOBAL % AXSYM == 2) THEN
         WRITE(GIT_UNIT) BLOCKS(B) % NPI, BLOCKS(B) % NPJ, BLOCKS(B) % NPK
      ELSE
         WRITE(GIT_UNIT) BLOCKS(B) % NPI, BLOCKS(B) % NPJ
      END IF

   END DO !B= 1,NB  SCHLEIFE ÜBER BLÖCKE GIT

   DO B = 1,GLOBAL % NBLOCK
      DO k = 1,BLOCKS(B) %  NPK
         DO j = 1,BLOCKS(B) % NPJ
            DO i= 1,BLOCKS(B) % NPI
               WRITE(GIT_UNIT) (BLOCKS(B) % XYZ(I,J,K,V),V=1,GITDIM)
            END DO
         END DO
      END DO
   END DO !B= 1,NB  SCHLEIFE ÜBER BLÖCKE GIT IN

   CLOSE(GIT_UNIT)


   END SUBROUTINE
end module
