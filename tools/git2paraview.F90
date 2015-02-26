program git2paraview
    implicit none

    INTEGER :: I,J,K
    INTEGER :: NI,NJ,NK

    REAL(KIND=8),ALLOCATABLE :: xyz(:,:,:,:)

    OPEN(10,FILE="git.bin",FORM="UNFORMATTED",access="STREAM",STATUS="OLD")
!    READ(10) AXSYM,NB

    READ(10) NI,NJ
    NK = 1
    allocate(xyz(3,NI,NJ,NK))

    DO k = 1, NK
       DO j = 1,NJ
          DO i= 1,NI
             READ(10) XYZ(1,i,j,k),XYZ(2,i,j,k),XYZ(3,i,j,k)
          END DO
       END DO
    END DO

    CLOSE(10)
    OPEN(10,FILE="git.vtk")
    WRITE(10,"(A)") '# vtk DataFile Version 2.0'
    WRITE(10,"(A)") 'TASCOM-GITTER in PARAVIEW'
    WRITE(10,"(A)") 'ASCII'
    WRITE(10,"(A)") ""
    WRITE(10,"(A)") 'DATASET STRUCTURED_GRID'
    WRITE(10,"(A,3I4)") 'DIMENSIONS ',NI,NJ,NK
    WRITE(10,"(A,I8,X,A)") 'POINTS ',NI*NJ*NK,'double'
    K = 1
    DO j = 1,NJ
       DO I = 1,NI
          WRITE(10,*) XYZ(1,i,j,k),XYZ(2,i,j,k),XYZ(3,i,j,k)
       END DO
    END DO

!    write(21,*) 'POINT_DATA', def%imax*def%jmax
!write(21,*) 'SCALARS Density double 1'
!write(21,*) 'LOOKUP_TABLE default'
!do j=1,def%jmax
!    do i=1,def%imax
!         write(21,*) grid(i,j)%u(1)
!   end do
!end do
    CLOSE(10)
end program git2paraview
