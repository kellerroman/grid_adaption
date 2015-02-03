module wall_refinement
use const, only: dp
implicit none
private

INTEGER,save                           :: NKNT
!< Anzahl der WANDPUNKTE / WAND-KANTEN
REAL(KIND = DP), ALLOCATABLE,SAVE      :: KNT_FORCE(:)
!< KRAFT WELCHE AUFGEWÄNDET WIRD
!!! FÜR METHODE 1
INTEGER, ALLOCATABLE,SAVE              :: KNT(:)

!! METHODE 2
INTEGER, ALLOCATABLE, SAVE             :: PKT_CTW(:)
!< Wandnächster Punkt (Close To Wall)
INTEGER, ALLOCATABLE, SAVE             :: PKT_AW(:)
!< Wandpunkt (At Wall)
REAL(KIND = DP), ALLOCATABLE,SAVE      :: PKT_DXY(:,:)
!< Differenzvektor des idealen Wandnäcshten Punktes vom wandnächsten Punkt
!< für Type = 2 und 3 sehr einfach, da in x bzw y Richtung
!< 1 Parameter ist Punkt_ID , zweiter Dimension

REAL(KIND = DP), PARAMETER             :: MAX_INC = 1.01D+0
REAL(KIND = DP), PARAMETER             :: MIN_INC = 0.995D+0
REAL(KIND = DP),save                   :: WALL_DIST


public input_wall_refinement
public init_wall_refinement
public calc_wall_refinement
public check_wall_refinement
contains

   SUBROUTINE init_wall_refinement()
   ! Diese Routine erstell eine Liste der Knoten die von der Wandverfeinerung betroffen sind
      use MOD_GLOBAL
      implicit none
      integer, parameter :: KAW_MAX = 100000
      integer :: B , F, i, j, k, i2, j2, k2
      integer :: pkt1
      integer :: pkt2
      integer :: kn
      INTEGER :: KAW(KAW_MAX)
      integer :: typ
      !< Number Kanten At Wall

      INTEGER :: P1(KAW_MAX)
      INTEGER :: P2(KAW_MAX)

      !!
      !!
      !! DIESE ART DER WANDVERFEINERUNG ERZWINGT EINE VORGEGEBENE KANTENLÄNGE
      !! KANN BEI KOMPLIZIERTEN GEOMETRIEN ZU FEHLERN FÜHREN
      !!
      global_refinement: if (global % wall_refinement ==1) then

         NKNT = 0
         block_loop1: do B = 1, global % NBLOCK
            !! WEST BOUNDARY
            F = 1
            if (BLOCKS(B) % BLOCK_CONNECTION(F,1) == 0) then
               i  = 1
               i2 = 2
               do j = 1, BLOCKS(B) % NPJ
                  do k = 1, BLOCKS(B) % NPK

                     j2 = j
                     k2 = k
                     ! wandpunkt
                     pkt1 = BLOCKS(b) % assoc(i ,j ,k)
                     ! wandnaechster punkt
                     pkt2 = BLOCKS(b) % assoc(i2,j2,k2)
                     ! schleife über alle Kanten die mit PKT1 verbunden sind
                     do kn = 1, UNSTR % PKT_NKNT(pkt1)
                        !WENN KANTE kn von PKT1 mit PKT2 verbunden ist
                        if (pkt2 == UNSTR % PKT_NEIGH(pkt1,kn)) then
                           ! KANTE KN von PKT in LISTE AUFNEHMEN
                           NKNT = NKNT + 1
                           if (NKNT > KAW_MAX) then
                              write(*,'(A)') "ERROR in INIT_BOUNDARY"
                              write(*,'(A,X,I0)') "Number of Edges is larger than the temporary array size:",KAW_MAX
                              stop
                           end if
                           KAW(NKNT) = UNSTR % PKT_KNT(pkt1,kn)
                        end if
                     end do
                  end do
               end do
            end if
            !! OST BOUNDARY
            F = 2
            if (BLOCKS(B) % BLOCK_CONNECTION(F,1) == 0) then
               i  = BLOCKS(B) % NPI
               i2 = I-1
               do j = 1, BLOCKS(B) % NPJ
                  do k = 1, BLOCKS(B) % NPK

                     j2 = j
                     k2 = k
                     ! wandpunkt
                     pkt1 = BLOCKS(b) % assoc(i ,j ,k)
                     ! wandnaechster punkt
                     pkt2 = BLOCKS(b) % assoc(i2,j2,k2)
                     ! schleife über alle Kanten die mit PKT1 verbunden sind
                     do kn = 1, UNSTR % PKT_NKNT(pkt1)
                        !WENN KANTE kn von PKT1 mit PKT2 verbunden ist
                        if (pkt2 == UNSTR % PKT_NEIGH(pkt1,kn)) then
                           ! KANTE KN von PKT in LISTE AUFNEHMEN
                           NKNT = NKNT + 1
                           if (NKNT > KAW_MAX) then
                              write(*,'(A)') "ERROR in INIT_BOUNDARY"
                              write(*,'(A,X,I0)') "Number of Edges is larger than the temporary array size:",KAW_MAX
                              stop
                           end if
                           KAW(NKNT) = UNSTR % PKT_KNT(pkt1,kn)
                        end if
                     end do
                  end do
               end do
            end if
            !! SUED BOUNDARY
            F = 3
            if (BLOCKS(B) % BLOCK_CONNECTION(F,1) == 0) then
               j  = 1
               j2 = j+1
               do i = 1, BLOCKS(B) % NPI
                  do k = 1, BLOCKS(B) % NPK
                     i2 = I
                     k2 = k
                     ! wandpunkt
                     pkt1 = BLOCKS(b) % assoc(i ,j ,k)
                     ! wandnaechster punkt
                     pkt2 = BLOCKS(b) % assoc(i2,j2,k2)
                     ! schleife über alle Kanten die mit PKT1 verbunden sind
                     do kn = 1, UNSTR % PKT_NKNT(pkt1)
                        !WENN KANTE kn von PKT1 mit PKT2 verbunden ist
                        if (pkt2 == UNSTR % PKT_NEIGH(pkt1,kn)) then
                           ! KANTE KN von PKT in LISTE AUFNEHMEN
                           NKNT = NKNT + 1
                           if (NKNT > KAW_MAX) then
                              write(*,'(A)') "ERROR in INIT_BOUNDARY"
                              write(*,'(A,X,I0)') "Number of Edges is larger than the temporary array size:",KAW_MAX
                              stop
                           end if
                           KAW(NKNT) = UNSTR % PKT_KNT(pkt1,kn)
                        end if
                     end do
                  end do
               end do
            end if
            !! NORD BOUNDARY
            F = 4
            if (BLOCKS(B) % BLOCK_CONNECTION(F,1) == 0) then
               j  = BLOCKS(B) % NPJ
               j2 = j-1
               do i = 1, BLOCKS(B) % NPI
                  do k = 1, BLOCKS(B) % NPK
                     i2 = I
                     k2 = k
                     ! wandpunkt
                     pkt1 = BLOCKS(b) % assoc(i ,j ,k)
                     ! wandnaechster punkt
                     pkt2 = BLOCKS(b) % assoc(i2,j2,k2)
                     ! schleife über alle Kanten die mit PKT1 verbunden sind
                     do kn = 1, UNSTR % PKT_NKNT(pkt1)
                        !WENN KANTE kn von PKT1 mit PKT2 verbunden ist
                        if (pkt2 == UNSTR % PKT_NEIGH(pkt1,kn)) then
                           ! KANTE KN von PKT in LISTE AUFNEHMEN
                           NKNT = NKNT + 1
                           if (NKNT > KAW_MAX) then
                              write(*,'(A)') "ERROR in INIT_BOUNDARY"
                              write(*,'(A,X,I0)') "Number of Edges is larger than the temporary array size:",KAW_MAX
                              stop
                           end if
                           KAW(NKNT) = UNSTR % PKT_KNT(pkt1,kn)
                        end if
                     end do
                  end do
               end do
            end if
         end do block_loop1

         !ABSPEICHERN DER KANTEN IN DEM ENDGÜLTIGEN ARRAY
         NKNT = NKNT

         allocate(KNT(NKNT))

         allocate(KNT_FORCE(NKNT))

         KNT_FORCE = 1.0D-3
         do i = 1,NKNT
            KNT(i) = KAW(i)
         end do
!      write(*,*) NKNT
      !!
      !! DIESE ART DER WANDVERFEINERUNG VERSUCHT ORTHOGONALE WANDABSTÄNDE HERZUSTELLEN IDEM DER WANDNÄCHSTE PUNKT
      !! IN RICHTUNG DES IDEALEN PUNKTES VERSCHOBEN WIRD
      !!
      else if (global % wall_refinement == 2) then global_refinement
         NKNT = 0
         block_loop2: do B = 1, global % NBLOCK
            !! WEST BOUNDARY
            F = 1
            if (BLOCKS(B) % BLOCK_CONNECTION(F,1) == 0) then
               i  = 1
               i2 = 2
               do j = 1, BLOCKS(B) % NPJ
                  do k = 1, BLOCKS(B) % NPK
                     j2 = j
                     k2 = k
                     NKNT = NKNT + 1
                     P1(NKNT) = BLOCKS(b) % assoc(i ,j ,k)
                     P2(NKNT) = BLOCKS(b) % assoc(i2,j2,k2)
                  end do
               end do
            end if
            !! OST BOUNDARY
            F = 2
            if (BLOCKS(B) % BLOCK_CONNECTION(F,1) == 0) then
               i  = BLOCKS(B) % NPI
               i2 = I-1
               do j = 1, BLOCKS(B) % NPJ
                  do k = 1, BLOCKS(B) % NPK

                     j2 = j
                     k2 = k

                     NKNT = NKNT + 1
                     P1(NKNT) = BLOCKS(b) % assoc(i ,j ,k)
                     P2(NKNT) = BLOCKS(b) % assoc(i2,j2,k2)
                  end do
               end do
            end if
            !! SUED BOUNDARY
            F = 3
            if (BLOCKS(B) % BLOCK_CONNECTION(F,1) == 0) then
               j  = 1
               j2 = j+1
               do i = 1, BLOCKS(B) % NPI
                  do k = 1, BLOCKS(B) % NPK
                     i2 = I
                     k2 = k

                     NKNT = NKNT + 1
                     P1(NKNT) = BLOCKS(b) % assoc(i ,j ,k)
                     P2(NKNT) = BLOCKS(b) % assoc(i2,j2,k2)
                  end do
               end do
            end if
            !! NORD BOUNDARY
            F = 4
            if (BLOCKS(B) % BLOCK_CONNECTION(F,1) == 0) then
               j  = BLOCKS(B) % NPJ
               j2 = j-1
               do i = 1, BLOCKS(B) % NPI
                  do k = 1, BLOCKS(B) % NPK
                     i2 = I
                     k2 = k
                     NKNT = NKNT + 1
                     P1(NKNT) = BLOCKS(b) % assoc(i ,j ,k)
                     P2(NKNT) = BLOCKS(b) % assoc(i2,j2,k2)
                  end do
               end do
            end if
         end do block_loop2

         !ABSPEICHERN DER KANTEN IN DEM ENDGÜLTIGEN ARRAY

         allocate(PKT_AW (NKNT) )
         allocate(PKT_CTW(NKNT) )
         allocate(KNT_FORCE(NKNT))
         allocate(PKT_DXY(NKNT,2))

         KNT_FORCE = 1.0D-5
         PKT_DXY = 0.0D0
         do i = 1,NKNT
            PKT_AW (I) = P1(I)
            PKT_CTW(I) = P2(I)
            typ = UNSTR % PKT_TYPE(PKT_AW(i))
            if (typ == 3) then
               !!!! WAAGRECHTE WAND
               PKT_DXY(I,1) = 0.0D0
               PKT_DXY(I,2) = SIGN(WALL_DIST,UNSTR % XYZ(PKT_CTW(I),2)-UNSTR % XYZ(PKT_AW(I),2))
            else if (typ == 4) then
               !!!! SENKRECHTE WAND
               PKT_DXY(I,1) = SIGN(WALL_DIST,UNSTR % XYZ(PKT_CTW(I),1)-UNSTR % XYZ(PKT_AW(I),1))
               PKT_DXY(I,2) = 0.0D0
            else
               if (UNSTR % PKT_TYPE(PKT_CTW(i)) == 3) then
                  PKT_DXY(I,1) = SIGN(WALL_DIST,UNSTR % XYZ(PKT_CTW(I),1)-UNSTR % XYZ(PKT_AW(I),1))
                  PKT_DXY(I,2) = 0.0D0
               else if (UNSTR % PKT_TYPE(PKT_CTW(i)) == 4) then
                  PKT_DXY(I,1) = 0.0D0
                  PKT_DXY(I,2) = SIGN(WALL_DIST,UNSTR % XYZ(PKT_CTW(I),2)-UNSTR % XYZ(PKT_AW(I),2))
               else if (UNSTR % PKT_TYPE(PKT_CTW(i)) == 2) then
                  write(*,*) "ERROR in wall_refinement_init"
                  write(*,*) "NODE",PKT_CTW(i),"is fixed and cannot be moved"
                  stop
               else
                  if (ABS(UNSTR % XYZ(PKT_CTW(I),1)-UNSTR % XYZ(PKT_AW(I),1)) < 1D-10) then
                     !!!! WAAGRECHTE WAND
                     PKT_DXY(I,1) = 0.0D0
                     PKT_DXY(I,2) = SIGN(WALL_DIST,UNSTR % XYZ(PKT_CTW(I),2)-UNSTR % XYZ(PKT_AW(I),2))
                  else if (ABS(UNSTR % XYZ(PKT_CTW(I),2)-UNSTR % XYZ(PKT_AW(I),2)) < 1D-10) then
                     !!!! SENKRECHTE WAND
                     PKT_DXY(I,1) = SIGN(WALL_DIST,UNSTR % XYZ(PKT_CTW(I),1)-UNSTR % XYZ(PKT_AW(I),1))
                     PKT_DXY(I,2) = 0.0D0
                  else

                     write(*,'(3(I6,X),I1,2(X,ES12.5))') i,pkt_aw(i),pkt_ctw(i),TYP,PKT_DXY(I,1),PKT_DXY(I,2)
                     write(*,*) "ERROR in wall_refinement_init"
                     write(*,*) "Init Wall Refinement: ideal Wanddelta cannot be calculated"
                     stop
                  end if
               end if
            end if
         end do
      end if global_refinement

   END SUBROUTINE init_wall_refinement

   SUBROUTINE input_wall_refinement()
   USE MOD_GLOBAL
   !use boundary, only: wall_dist
   use utils, only: is_integer,lower_case
   IMPLICIT NONE

   CHARACTER(LEN= 1000) :: LINE
   CHARACTER(LEN= 50) :: VARNAME,VARVALUE,BLOCK_NUM,BLOCK_PHASE
   INTEGER :: ISTAT
   INTEGER :: POS
   INTEGER, PARAMETER :: IO_CF = 20
   LOGICAL :: FEXISTS

   INTEGER :: ZUSTAND
   !< 0: UNdefiniert: Es muss eine
   !< 1: WALL DEFINITION

   INTEGER :: iBLOCK

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !
   !                         CONFIGURATIONSDATEI AUSLESEN
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   if (global % wall_refinement > 0 ) then
      INQUIRE(FILE=TRIM(GLOBAL % WALL_REFINEMENT_FILE),EXIST=FEXISTS)
      IF(FEXISTS .EQV. .FALSE.) THEN
         WRITE(*,*) "CONTROL FILE konnte nicht gefunden werden: " // TRIM(GLOBAL % WALL_REFINEMENT_FILE)
         STOP
      END IF
      OPEN(IO_CF,FILE=TRIM(GLOBAL % WALL_REFINEMENT_FILE),STATUS="OLD")
      ZUSTAND = -1000
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
         line = lower_case(line)

         SELECT CASE(TRIM(ADJUSTL(LINE)))
         CASE ("wall")
            zustand = 0
         CASE ("sym")
            zustand = -1
         CASE ("inflow")
            zustand = -2
         CASE ("outflow")
            zustand = -3
         CASE ("slipwall")
            zustand = -4
         CASE DEFAULT
            POS = INDEX(LINE,"=")
            IF (POS > 0) THEN   !  VARIABLEN DEFINITION
               VARNAME = lower_case(TRIM(LINE(1:POS-1)))
               VARVALUE = TRIM(ADJUSTL(LINE(POS+1:)))
               SELECT CASE ( TRIM(VARNAME) )
               CASE ("wall_dist")
                  READ(VARVALUE,*) WALL_DIST
                  WRITE(*,*) "WANDABSTAND:",wall_dist
               CASE DEFAULT
                  WRITE(*,*) "VARIABLE NICHT ERkANNT:",TRIM(VARNAME)
                  STOP
               END SELECT
            ELSE
               DO
                  POS = INDEX(LINE,",")
                  IF (POS > 0) THEN
                     VARNAME = lower_case(TRIM(LINE(1:POS-1)))
                     LINE = TRIM(ADJUSTL(LINE(POS+1:)))
                  ELSE
                     IF (LEN_TRIM(LINE) == 0) EXIT
                     VARNAME = TRIM(LINE)
                     LINE = ""
                  END IF
                  POS = 1
                  DO
                     IF ( IS_INTEGER(TRIM(VARNAME(1:POS)))) THEN
                        POS = POS + 1
                        IF (LEN_TRIM(VARNAME) < POS) THEN
                           WRITE(*,*) "BLOCK FACE IS NOT DEFINED: ",TRIM(VARNAME)
                           STOP
                        END IF
      !                  WRITE(*,*) "IS_INT:",TRIM(VARNAME(1:POS-1))
                     ELSE
                        POS = POS - 1
                        BLOCK_NUM = TRIM(ADJUSTL(VARNAME(1:POS)))
                        BLOCK_PHASE = lower_case(TRIM(ADJUSTL(VARNAME(POS+1:))))
                        READ(BLOCK_NUM,*) iBlock
                        SELECT CASE (TRIM(BLOCK_PHASE))
                        CASE("n")
                           BLOCK_PHASE = "NORTH"
                           if (BLOCKS(iBLOCK) % BLOCK_CONNECTION(4,1) == -1000) then
                              BLOCKS(iBLOCK) % BLOCK_CONNECTION(4,1) = zustand
                              IF (zustand == 0) THEN
                              END IF
                           else
                              write(*,'(A)') "Warning in input_wall_refinement"
                              write(*,'(A,X,I0,X,A)') "NORTH SIDE OF BLOCK",iBlock,"is already defined"
                           end if
                        CASE("s")
                           BLOCK_PHASE = "SOUTH"
                           if (BLOCKS(iBLOCK) % BLOCK_CONNECTION(3,1) == -1000) then
                              BLOCKS(iBLOCK) % BLOCK_CONNECTION(3,1) = zustand
                              IF (zustand == 0) THEN
                              END IF
                           else
                              write(*,'(A)') "Warning in input_wall_refinement"
                              write(*,'(A,X,I0,X,A)') "NORTH SIDE OF BLOCK",iBlock,"is already defined"
                           end if
                        CASE("e")
                           BLOCK_PHASE = "EAST"
                           if (BLOCKS(iBLOCK) % BLOCK_CONNECTION(2,1) == -1000) then
                              BLOCKS(iBLOCK) % BLOCK_CONNECTION(2,1) = zustand
                              IF (zustand == 0) THEN
                              END IF
                           else
                              write(*,'(A)') "Warning in input_wall_refinement"
                              write(*,'(A,X,I0,X,A)') "NORTH SIDE OF BLOCK",iBlock,"is already defined"
                           end if
                        CASE("w")
                           BLOCK_PHASE = "WEST"
                           if (BLOCKS(iBLOCK) % BLOCK_CONNECTION(1,1) == -1000) then
                              BLOCKS(iBLOCK) % BLOCK_CONNECTION(1,1) = zustand
                              IF (zustand == 0) THEN
                              END IF
                           else
                              write(*,'(A)') "Warning in input_wall_refinement"
                              write(*,'(A,X,I0,X,A)') "NORTH SIDE OF BLOCK",iBlock,"is already defined"
                           end if
                        CASE("b")
                           BLOCK_PHASE = "BACK"
                           if (BLOCKS(iBLOCK) % BLOCK_CONNECTION(5,1) == -1000) then
                              BLOCKS(iBLOCK) % BLOCK_CONNECTION(5,1) = zustand
                              IF (zustand == 0) THEN
                              END IF
                           else
                              write(*,'(A)') "Warning in input_wall_refinement"
                              write(*,'(A,X,I0,X,A)') "NORTH SIDE OF BLOCK",iBlock,"is already defined"
                           end if
                        CASE("f")
                           BLOCK_PHASE = "FRONT"
                           if (BLOCKS(iBLOCK) % BLOCK_CONNECTION(6,1) == -1000) then
                              BLOCKS(iBLOCK) % BLOCK_CONNECTION(6,1) = zustand
                              IF (zustand == 0) THEN
                              END IF
                           else
                              write(*,'(A)') "Warning in input_wall_refinement"
                              write(*,'(A,X,I0,X,A)') "NORTH SIDE OF BLOCK",iBlock,"is already defined"
                           end if
                        CASE DEFAULT
                           WRITE(*,*) "FACE NICHT ERKANNT:",TRIM(BLOCK_PHASE)
                           STOP
                        END SELECT
      !                  WRITE(*,'(X,I4.4,2A)') iBLOCK ," : ", TRIM(BLOCK_PHASE)
                        EXIT
                     END IF
                  END DO
               END DO

      !         WRITE(*,'(A)') "LINIE NICHT ERKANNT: "//TRIM(LINE)
            END IF
         END SELECT

      END DO
      CLOSE(IO_CF)
   end if
   END SUBROUTINE input_wall_refinement


   SUBROUTINE calc_wall_refinement()
      USE MOD_GLOBAL
      IMPLICIT NONE

      integer :: k, i ,p1, p2, e,tempsort

      integer :: nedge
      !< Anzahl der kanten
      REAL(KIND=8) :: is2should
      REAL(KIND=8) :: ipos(2)

      REAL(KIND=8) :: fvec(2)
      !< force vector
      REAL(KIND=8) :: abs_fvec
      REAL(KIND=8) :: evec(4,2)
      !< edge vector
      REAL(KIND=8) :: sp(4)
      integer :: sort(4)

      do_wall_refinement: if (global % wall_refinement > 0) then
#ifdef DEBUG
         IF (GLOBAL%DBG >= 1)  THEN
            WRITE(*,'(A)') "WANDVERFEINERUNG"
         END IF
#endif

         IF (GLOBAL % AXSYM == 2) THEN
            WRITE(*,*) "3D NOCH NICHT UNTERSTÜTZT","WANDVERFEINERUNG"
            STOP
         END IF
         !!
         !!
         !! DIESE ART DER WANDVERFEINERUNG ERZWINGT EINE VORGEGEBENE KANTENLÄNGE
         !! KANN BEI KOMPLIZIERTEN GEOMETRIEN ZU FEHLERN FÜHREN
         !!
         if (GLOBAL % WALL_REFINEMENT == 1) then
            do i = 1,NKNT
               k = KNT(i)
               is2should = UNSTR % KNT_DN(k,1) / wall_dist

               is2should = MIN(MAX_INC, is2should)

               is2should = MAX(MIN_INC, is2should)

               KNT_FORCE(i) = KNT_FORCE(i) * is2should

               UNSTR % KNT_SPANNUNG(k,1) = UNSTR % KNT_SPANNUNG(k,1) + KNT_FORCE(i)

            end do
         !!
         !! DIESE ART DER WANDVERFEINERUNG VERSUCHT ORTHOGONALE WANDABSTÄNDE HERZUSTELLEN IDEM DER WANDNÄCHSTE PUNKT
         !! IN RICHTUNG DES IDEALEN PUNKTES VERSCHOBEN WIRD
         !!
         else if (GLOBAL % WALL_REFINEMENT == 2) then
            do i = 1,NKNT
               p1    = PKT_AW(I)
               p2    = PKT_CTW(I)
               ipos  = unstr % xyz(p1,1:2) + PKT_DXY(I,:)
               fvec   = ipos - unstr % xyz(p2,1:2)
               abs_fvec =  sqrt(fvec(1)*fvec(1)+fvec(2)*fvec(2))
               UNSTR % PKT_SOLL(p2,:) = fvec
               fvec = fvec / abs_fvec
               nedge = unstr % PKT_NKNT(p2)

               do k = 1, nedge
                  e = unstr % PKT_KNT (p2,k)
                  evec(k,:) = unstr % KNT_DN(e,2:3)
                  !! ORIENTIERUNG DES VEKTORS UMDREHEN, WENN ER AUF P2 ZEIGT
                  if (p2 == unstr % KNT(e,2) ) then
                     evec(k,:) = - evec(k,:)
                  end if
                  evec(k,:) = evec(k,:) / sqrt(evec(k,1)*evec(k,1)+evec(k,2)*evec(k,2))
                  sp(k) = dot_product(fvec,evec(k,:))
                  sort(k) = k
                  e = k+1
                  ! SORTIEREN DER ede_vectoren zum einfachsten kombinieren des force vectors
                  if (k > 1) then
                     do
                        e = e-1
                        if (sp(sort(e-1)) < sp(sort(e)) ) then
                           tempsort = sort(e-1)
                           sort(e-1) = sort(e)
                           sort(e) = tempsort
                        else
                           exit
                        end if
                        if (e == 2) exit
                     end do
                  end if

               end do
!               write(*,*) sort(1:nedge)
!               write(*,'(6(ES12.5,X))') unstr % xyz(p2,1:2), ipos,fvec
!               write(*,'(3(ES12.5,X))') (evec(k,:),sp(k),k=1,nedge)
               ! edge_vector == force vector (normiert)
               ! kraft wird einfach auf diese eine edge berechnet
!               if (sp(sort(1)) >= 9.0D-1) then

                  is2should = abs_fvec / wall_dist

                  is2should = MIN(MAX_INC, is2should)

                  is2should = MAX(MIN_INC, is2should)

                  KNT_FORCE(i) = KNT_FORCE(i) * is2should

                  UNSTR % KNT_SPANNUNG(UNSTR % PKT_KNT(p2,sort(1)),1) =  &
                  UNSTR % KNT_SPANNUNG(UNSTR % PKT_KNT(p2,sort(1)),1) + abs_fvec!KNT_FORCE(i)
!               else
!                  write(*,*) p1,sp(sort(1:nedge))
!
!                  stop "Muss Kraftvektor aufteilen"
!               end if
            end do
!            stop
         end if
      i = 222
      nedge = UNSTR % PKT_NKNT(i)
!      write(*,*) (UNSTR % PKT_NEIGH(i,k),k = 1,nedge)
      write(*,'(I5.5,4(X,ES12.5))') i,(UNSTR % KNT_SPANNUNG(UNSTR % PKT_KNT(i,k),1),k = 1,nedge)

      end if do_wall_refinement
   END SUBROUTINE calc_wall_refinement

   subroutine check_wall_refinement()
   use mod_global, only: unstr, global
   implicit none
   integer :: i,k
   REAL(KIND=8) :: FAKTOR
   logical :: unideal_edges
   if (GLOBAL % WALL_REFINEMENT == 1) then
      if (GLOBAL % WALL_REFINEMENT_CHECK == 1) then
         write(*,'(A6,X,A12,X,A12)') "EDGE#","LENGTH","FORCE"
         do i = 1, NKNT
            k = KNT(i)
            write(*,'(I6.6,X,ES12.5,X,ES12.5)') k,UNSTR % KNT_DN(k,1), KNT_FORCE(i)
         end do
      else if (GLOBAL % WALL_REFINEMENT_CHECK == 2) then
      !!! ES WERDEN NUR KANTEN MIT EINER ABWEICHUNG VON 10% AUSGEGEBEN
         unideal_edges = .FALSE.
         !! ÜBERPRÜFEN OB ES ÜBERhAUPT NOCH EDGES GIBT DIE SO STARK ABWEICHEN
         do i = 1, NKNT
            k = KNT(i)
            FAKTOR = ABS(UNSTR % KNT_DN(k,1) / wall_dist)
            if (FAKTOR < 0.9D0 .OR. FAKTOR >1.1D0) then
               unideal_edges = .TRUE.
               exit
            end if
         end do
         !!! AUSGABE DER ABWEICHENDEN EDGES
         if (unideal_edges) then
            write(*,'(A6,X,A12,X,A12)') "EDGE#","LENGTH","FORCE"
            do i = 1, NKNT
               k = KNT(i)
               FAKTOR = ABS(UNSTR % KNT_DN(k,1) / wall_dist)
               if (FAKTOR < 0.9D0 .OR. FAKTOR >1.1D0) then
                  write(*,'(I6.6,X,ES12.5,X,ES12.5)') k,UNSTR % KNT_DN(k,1), KNT_FORCE(i)
               end if
            end do
         end if
      end if
   else
!      do i = 1, NKNT
!      write(*,'(I6.6,X,ES12.5,X,ES12.5)') i,KNT_FORCE(i)
!      end do

   end if
   end subroutine check_wall_refinement
end module wall_refinement
