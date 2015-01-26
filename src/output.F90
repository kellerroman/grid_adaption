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

INTEGER, PARAMETER :: OUTUNIT = 10
INTEGER :: N, V, I, J, K
IF (SOLTIME == 0) THEN
OPEN(OUTUNIT,FILE="ani.plt",STATUS="REPLACE")
WRITE(OUTUNIT,'(A)',ADVANCE="NO") 'Variables = "X", "Y", "Z"'
DO v=1,GLOBAL%NVAR
 WRITE(OUTUNIT,'(3A)',ADVANCE="NO") ', "',TRIM(GLOBAL%VNAME(V)),'"'
END DO
WRITE(OUTUNIT,'(/)',ADVANCE="NO")

ELSE
OPEN(OUTUNIT,FILE="ani.plt",STATUS="old")
END IF
DO N = 1, GLOBAL%NBLOCK

   WRITE(OUTUNIT,'(A,I0,A,I4,A,I4,A,I4)') 'ZONE,SOLUTIONTIME=',SOLTIME &
   ,' I=',BLOCKS(N)%NPI,', J=',BLOCKS(N)%NPJ,', K=',BLOCKS(N)%NPK
   WRITE(OUTUNIT,'(A,I0,A)') 'DATAPACKING=BLOCK, VARLOCATION=([4-',GLOBAL%NVAR+4,']=CELLCENTERED)'
   WRITE(OUTUNIT,'(5(D20.13,1X))') (UNSTR%XYZ(i,1),I = 1,UNSTR%NPKT)
   WRITE(OUTUNIT,*)
   WRITE(OUTUNIT,'(5(D20.13,1X))') (UNSTR%XYZ(i,2),I = 1,UNSTR%NPKT)
   WRITE(OUTUNIT,*)
!   WRITE(OUTUNIT,'(5(D20.13,1X))') (((BLOCKS(N)%XYZ(i,j,k,3),i=1,BLOCKS(N)%NPI) &
!                                                               ,j=1,BLOCKS(N)%NPJ) &
!                                                               ,k=1,BLOCKS(N)%NPK)
   WRITE(OUTUNIT,'(5(D20.13,1X))')   (UNSTR%PKT_VAR(I,1),I = 1,UNSTR%NPKT)
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

