!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!          GRID ADAPTION MAIN FILE          !!!!!!!
!!
!! AUTHOR:           ROMAN KELLER
!! START:            19.09.2013
!! LAST CHANGE:      27.01.2015
!!
!! CHANGELOG:
!!          27.01.201,RK: Added to GITHUB, new file structure
!!
!!
!! TODO:    CONTROL-INPUTFILE
!!          INTERPOLATION HöHERER ORDNUNG (VERHINDERUNG EINES WERTESPRUNGS BEIM REFERNZPUNKTWECHSEL)
!!          EINLESEN DER RANDBEINGUNGEN AUS DER ZS
!!          3D
!!          Mehrere Blöcke
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program grid_adaption
   use mod_global
   use io
   use grid
   implicit none


   CHARACTER(LEN=*),PARAMETER :: VERSION = "V0.1.2"
   CHARACTER(LEN=*),PARAMETER :: LAST_CHANGE = "03.10.2014"


   INTEGER :: i
   REAL(KIND=8) :: DN_SUM,DN_MAX
   INTEGER :: DN_MAX_POS

   WRITE(*,'(A)') "AUTMATISCHE GRID ADAPTION by ROMAN KELLER"
   WRITE(*,'(A)') "VERSION "//VERSION//" LAST CHANGE "//LAST_CHANGE

   CALL INPUT_CONTROL()

   WRITE(*,'(A)') "RUNNING ADAPTATION ON "//TRIM(GLOBAL % GIT_IN)

   CALL INPUT(.FALSE.)

   IF (Global % NITER == 0) THEN
      CALL CALC_GRID()
   ELSE
      WRITE(*,'(A9,X,2(A12,x),A)') "ITERATION","RES_SUM","RES_MAX","@(B,I,J,K)"
   END IF

   do i = 1, Global % NITER

      CALL CALC_GRID()

      IF (GLOBAL % OUTPUT_ANIMATION > 0 ) THEN
         IF ( MOD(I-1,GLOBAL % OUTPUT_ANIMATION) == 0 ) THEN
!         CALL OUTPUT_1D(i-1)
            IF (GLOBAL % OUTPUT_TYPE == 0) THEN
               CALL PARAVIEW_OUTPUT_UNSTR(I-1)
            ELSE
               CALL UNSTR2STR()
               CALL WRITE_TECPLOT_ANI(i-1)
            END IF
         END IF
      END IF

      CALL CALC_SCHIEBESPANNUNG()

      CALL RESIZE_GRID(DN_SUM,DN_MAX,DN_MAX_POS)

      WRITE(*,'(X,I8,X,2(D12.5,X),"@",I0," (",4(I0,X),")")') I,DN_SUM &
                        ,DN_MAX,DN_MAX_POS,UNSTR%PKT_REF(DN_MAX_POS,4),UNSTR%PKT_REF(DN_MAX_POS,1:3)
   end do

!   CALL OUTPUT_1D(i-1)
   IF (GLOBAL % OUTPUT_TYPE == 0) THEN
      CALL PARAVIEW_OUTPUT_UNSTR (-1)
      IF (GLOBAL % OUTPUT_ANIMATION > 0 ) THEN
         CALL PARAVIEW_OUTPUT_UNSTR(I-1)
      END IF
   ELSE
      CALL UNSTR2STR()
      IF (GLOBAL % OUTPUT_ANIMATION > 0 ) THEN
         CALL WRITE_TECPLOT_ANI(i-1)
      END IF
      CALL WRITE_TECPLOT("sol_plot.plt")
   END IF

   CALL OUTPUT_GRID()
   WRITE(*,*) "======================================================" &
             ,"FINISHED"  &
             ,"======================================================"
end program
