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
