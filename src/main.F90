!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!          GRID ADAPTION MAIN FILE          !!!!!!!
!!
!! AUTHOR:           ROMAN KELLER
!! START:            19.09.2013
!! LAST CHANGE:      28.01.2015
!!
!! CHANGELOG:
!!          27.01.201,RK: Added to GITHUB, new file structure
!!
!!
!! TODO:
!!          INTERPOLATION HöHERER ORDNUNG (VERHINDERUNG EINES WERTESPRUNGS BEIM REFERNZPUNKTWECHSEL)
!!          3D
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program grid_adaption
   use mod_global
   use io
   use grid
   use init_mod, only: init
   use wall_refinement, only: check_wall_refinement
   use edge_stress
   implicit none


   CHARACTER(LEN=*),PARAMETER :: VERSION = "V0.1.3"
   CHARACTER(LEN=*),PARAMETER :: LAST_CHANGE = "28.01.2015"


   INTEGER :: i
   REAL(KIND=8) :: DN_SUM,DN_MAX,STRESSES_SUM
   INTEGER :: DN_MAX_POS

   WRITE(*,'(A)') "AUTMATISCHE GRID ADAPTION by ROMAN KELLER"
   WRITE(*,'(A)') "VERSION "//VERSION//" LAST CHANGE "//LAST_CHANGE

   CALL INPUT_CONTROL()

   WRITE(*,'(A)') "RUNNING ADAPTATION ON "//TRIM(GLOBAL % GIT_IN)

   CALL INPUT(.FALSE.)

   CALL INIT()

   IF (Global % NITER == 0) THEN
      CALL CALC_GRID()
      i = 1
   ELSE
      WRITE(*,'(A9,X,3(A12,x),A10)') "ITERATION","RES_SUM","RES_MAX","SUM_STRESSES","@(B,I,J,K)"


      MAIN_LOOP: do i = 1, Global % NITER

         if (I < 50 .OR. MOD(I,GLOBAL % NITER_OUTPUT) == 0 ) then
            iteration_output = .true.
         else
            iteration_output = .false.
         end if

         CALL CALC_GRID()

         IF (GLOBAL % OUTPUT_ANIMATION > 0 ) THEN
            IF ( MOD(I-1,GLOBAL % OUTPUT_ANIMATION) == 0 ) THEN
   !         CALL OUTPUT_1D(i-1)
               IF (GLOBAL % OUTPUT_TYPE == 0) THEN
                  CALL PARAVIEW_OUTPUT_UNSTR(I-1)
               ELSE
                  CALL UNSTR2STR()
                  CALL WRITE_TECPLOT_ANI(I-1)
               END IF
            END IF
         END IF

         CALL CALC_EDGE_STRESSES(STRESSES_SUM)

         CALL RESIZE_GRID(DN_SUM,DN_MAX,DN_MAX_POS)

         if (iteration_output) then
            WRITE(*,'(X,I8,X,3(ES12.5,X),"@",I0,"(",4(I0,X),")")')       &
                           I,DN_SUM,DN_MAX,STRESSES_SUM                &
                           ,DN_MAX_POS,UNSTR%PKT_REF(DN_MAX_POS,4)      &
                           ,UNSTR%PKT_REF(DN_MAX_POS,1:3)

            CALL CHECK_WALL_REFINEMENT()
         END IF
      end do MAIN_LOOP
   END IF
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

   CALL UNSTR2STR()
   CALL OUTPUT_GRID()

   WRITE(*,*) "======================================================" &
             ,"FINISHED"  &
             ,"======================================================"
end program
