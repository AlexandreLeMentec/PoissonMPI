!parallel.f90
!!!!
!!subroutine initialisation_mpi
!!subroutine creation_topologie
!!subroutine domaine
!!subroutine voisinage
!!subroutine type_derive
!!subroutine communication
!!function erreur_globale
!!subroutine ecrire_mpi
!!subroutine finalisation_mpi
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE parallel
  USE TYPE_PARAMS
  USE MPI_F08
  IMPLICIT NONE

  !Rang du sous-domaine local
  INTEGER                                   :: rang
  INTEGER                                   :: ierr
  !Nombre de processus
  INTEGER                                   :: nb_procs
  !Communicateur de la topologie cartesienne
  TYPE(MPI_Comm)                            :: comm2d
  !Nombre de dimensions de la grille
  INTEGER, PARAMETER                        :: ndims = 2
  !Nombre de processus dans chaque dimension definissant la topologie
  INTEGER, DIMENSION(ndims)                 :: dims
  !Periodicite de  la topologie
  LOGICAL, DIMENSION(ndims)                 :: periods
  ! Coordonnees du domaine locale dans la topologie cartesienne
  INTEGER, DIMENSION(ndims)                 :: coords
  ! Tableau definissant les voisins de chaque processus
  INTEGER, PARAMETER                        :: NB_VOISINS = 4
  INTEGER, PARAMETER                        :: N=1, E=2, S=3, W=4
  INTEGER, DIMENSION(NB_VOISINS)            :: voisin
  ! Types derives
  TYPE(MPI_Datatype)                        :: typedp,type_ligne, type_colonne, type_tableau
  !Constantes MPI
  INTEGER                                   :: code

CONTAINS

  SUBROUTINE initialisation_mpi
    !************
    !Initialisation pour chaque processus de son rang et du
    !nombre total de processus nb_procs
    !************
    !Initialisation de MPI
    call MPI_INIT(ierr)
    !Savoir quel processus je suis
    call MPI_COMM_RANK(MPI_COMM_WORLD, rang, ierr)
    !Connaitre le nombre total de processus
    call MPI_COMM_SIZE(MPI_COMM_WORLD, nb_procs, ierr)
    !print* , nb_procs, rang
  END SUBROUTINE initialisation_mpi

  SUBROUTINE creation_topologie
    !************
    !Creation de la topologie cartesienne
    !************

    !Constantes MPI
    LOGICAL, PARAMETER                        :: reorganisation = .FALSE.

    ! Lecture du nombre de points ntx en x et nty en y
    OPEN(10, FILE='poisson.data', STATUS='OLD')
    READ(10, *) ntx
    READ(10, *) nty
    CLOSE(10)

    !Connaitre le nombre de processus selon x et le nombre de processus
    !selon y en fonction du nombre total de processus
    
    CALL MPI_DIMS_CREATE(nb_procs, ndims, dims, code)

    !Creation de la grille de processus 2D sans periodicite
    periods(1) = .FALSE.
    periods(2) = .FALSE.
    
    IF (rang == 0) THEN
      WRITE (*,'(A)') '-----------------------------------------'
      WRITE (*,'(A,i4,A)') 'Execution code poisson avec ', nb_procs, ' processus MPI'
      WRITE (*,'(A,i4,A,i4)') 'Taille du domaine : ntx=', ntx, ' nty=', nty
      WRITE (*,'(A,i4,A,i4,A)') 'Dimension de la topologie : ', &
                                 dims(1), ' suivant x, ', dims(2), ' suivant y'
      WRITE (*,'(A)') '-----------------------------------------'
    END IF

  END SUBROUTINE creation_topologie


  SUBROUTINE domaine
    !************
    !Calcul des coordonnées globales limites du sous domaine local
    !************

    ! Création de la topologie
    call MPI_Cart_create(MPI_COMM_WORLD, 2, dims, periods, .FALSE., comm2d)
    ! Connaitre mes coordonnees dans la topologie
    call MPI_Cart_coords(comm2d, rang, ndims, coords, ierr)

    ! WRITE(*,*) rang, 'de coords', coords
    !!Calcul pour chaque processus de ses indices de debut et de fin suivant x
    sx = (coords(1)*ntx)/dims(1)+1
    ex = ((coords(1)+1)*ntx)/dims(1)

    !Calcul pour chaque processus de ses indices de debut et de fin suivant y
    sy = (coords(2)*nty)/dims(2)+1
    ey = ((coords(2)+1)*nty)/dims(2)

    WRITE (*,'(A,i4,A,i4,A,i4,A,i4,A,i4,A)') 'Rang dans la topologie : ', rang, &
         ' Indice des tableaux :', sx, ' a', ex, ' suivant x, ', &
         sy, ' a', ey, ' suivant y'

  END SUBROUTINE domaine

  SUBROUTINE voisinage
    !************
    !Calcul des processus voisins pour chaque processus
    !************

    !Recherche des voisins Nord et Sud
    IF (coords(2) - 1 < 0  .AND. .NOT. periods(2)) THEN
      voisin(N) = MPI_PROC_NULL ! Ici on utilise MPI_PROC_NULL pour annuler la communication avec ce processeur sans passer par un IF
    ELSE
      call MPI_CART_RANK(comm2d, [coords(1), coords(2)-1], voisin(N), ierr)
    END IF

    IF (coords(2) + 1 > dims(2)-1  .AND. .NOT. periods(2)) THEN
      voisin(S) = MPI_PROC_NULL
    ELSE
      call MPI_CART_RANK(comm2d, [coords(1), coords(2)+1], voisin(S), ierr)
    END IF

    !Recherche des voisins Ouest et Est
    IF (coords(1) - 1 < 0  .AND. .NOT. periods(1)) THEN
      voisin(W) = MPI_PROC_NULL
    ELSE
      call MPI_CART_RANK(comm2d, [coords(1)-1, coords(2)], voisin(W), ierr)
    END IF

    IF (coords(1) + 1> dims(1)-1  .AND. .NOT. periods(1)) THEN
      voisin(E) = MPI_PROC_NULL
    ELSE
      call MPI_CART_RANK(comm2d, [coords(1)+1, coords(2)], voisin(E), ierr)
    END IF

    WRITE (*,'(A,i4,A,i4,A,i4,A,i4,A,i4)') "Processus ", rang, &
     " a pour voisin : N", voisin(N), " E", voisin(E), &
     " S", voisin(S), " W", voisin(W)

  END SUBROUTINE voisinage


  SUBROUTINE type_derive
    !************
    !Creation des types derives type_ligne et type_colonne
    !************

    !type dérivé dp
    CALL MPI_TYPE_CREATE_F90_REAL(15,307,typedp) 
    CALL MPI_TYPE_COMMIT(typedp, ierr)
    !Creation du type type_ligne pour echanger les points
    !au nord et au sud
    CALL MPI_TYPE_VECTOR(ey-sy+1, 1, ex-sx+3, typedp, type_ligne, ierr)
    CALL MPI_TYPE_COMMIT(type_ligne, ierr)

    !Creation du type type_colonne pour echanger
    !les points  a l'ouest et a l'est
    CALL MPI_TYPE_CONTIGUOUS(ex-sx+1, typedp, type_colonne, ierr)
    CALL MPI_TYPE_COMMIT(type_colonne, ierr)

    ! Creation d'un type_tableau pour échanger
    ! le champ de vitesse
    ! write(*,*) 'type_tableau', rang, "limites" , sx, ex, sy, ey
    ! CALL MPI_TYPE_VECTOR(ey-sy+1, ex-sx+1, (ey-sy+1), typedp, type_tableau, ierr)
    ! CALL MPI_TYPE_COMMIT(type_tableau, ierr)
  END SUBROUTINE type_derive


  SUBROUTINE communication(u)
    !************
    !Echange des points aux interfaces
    !************

    REAL(kind=dp), ALLOCATABLE, DIMENSION(:, :), INTENT(inout) :: u

    !Constantes MPI
    INTEGER, PARAMETER                   :: etiquette=125
    TYPE(MPI_Status)                     :: statut

    !Envoi au voisin N et reception du voisin 
    CALL MPI_SENDRECV(u(sx, sy), 1, type_ligne,   voisin(N), &
         etiquette, u(ex+1, sy), 1, type_ligne,   voisin(S), etiquette, comm2d, statut, code)

    !Envoi au voisin S et reception du voisin N
    CALL MPI_SENDRECV(u(ex, sy), 1, type_ligne,   voisin(S), &
         etiquette, u(sx-1, sy), 1, type_ligne,   voisin(N), etiquette, comm2d, statut, code)

    !Envoi au voisin W et reception du voisin E
    CALL MPI_SENDRECV(u(sx, sy), 1, type_colonne, voisin(W), &
         etiquette, u(sx, ey+1), 1, type_colonne, voisin(E), etiquette, comm2d, statut, code)

    !Envoi au voisin E et  reception du voisin W
    CALL MPI_SENDRECV(u(sx, ey), 1, type_colonne, voisin(E), &
         etiquette, u(sx, sy-1), 1, type_colonne, voisin(W), etiquette, comm2d, statut, code)
    
  END SUBROUTINE communication

  SUBROUTINE vitesse_affichage(u_affichage, u)
    
    !********************
    ! Calcul de la vitesse d'affichage
    !********************
    REAL(kind=dp), ALLOCATABLE, DIMENSION(:, :), INTENT(inout) :: u_affichage, u
    integer :: sen_count, rec_count,j
    write(*,*) 'vitesse_affichage', rang, "waiting"
    call MPI_BARRIER(comm2d, ierr)
    sen_count = (ex-sx+1)*(ey-sy+1)
    rec_count =  size(u_affichage(sx:ex,sy:ey))
    write(*,*) 'taille sen_count', sen_count, 'rec_count', rec_count
    ! call MPI_GATHER(u(sx:ex, sy:ey), (ex-sx+1)*(ey-sy+1), type_tableau, &
    !                 u_affichage(sx:ex,sy:ey), (ex-sx+1)*(ey-sy+1), &
    !                 type_tableau, 0, comm2d, ierr)
    
    ! Gather ligne par ligne
    do j = sy, ey
      call MPI_GATHER(u(sx:ex, j), ex-sx+1, type_ligne, &
                      u_affichage(sx:ex,j), ex-sx+1, type_ligne, 0, comm2d, ierr)
    end do
    write(*,*) 'vitesse_affichage', rang, "gathered, waiting..."
    call MPI_BARRIER(comm2d, ierr)
    write(*,*) 'vitesse_affichage', rang, "gathered, done"

  END SUBROUTINE vitesse_affichage

  FUNCTION erreur_globale(u, u_nouveau)
    !************
    !Calcul de l'erreur globale (maximum des erreurs locales)
    !************
    REAL(kind=dp), ALLOCATABLE, DIMENSION(:, :), INTENT(in) :: u, u_nouveau

    REAL(kind=dp)              :: erreur_globale, erreur_locale


    erreur_locale = MAXVAL(ABS(u(sx:ex, sy:ey) - u_nouveau(sx:ex, sy:ey)))

    !Calcul de l'erreur sur tous les sous-domaines

    call MPI_Allreduce(erreur_locale, erreur_globale, 8, typedp, MPI_SUM, comm2d)

  END FUNCTION erreur_globale

  SUBROUTINE ecrire_mpi(u)
    !********************
    ! Ecriture du tableau u a l'interieur d'un domaine pour chaque processus
    ! dans le fichier donnees.dat
    !********************
    REAL(kind=dp), ALLOCATABLE, DIMENSION(:, :), INTENT(inout) :: u

    !Constantes MPI
    TYPE(MPI_Status)                 :: statut
    TYPE(MPI_File)                   :: descripteur
    INTEGER(kind = MPI_OFFSET_KIND)  :: deplacement_initial
    INTEGER, PARAMETER               :: rang_tableau=2
    INTEGER, DIMENSION(rang_tableau) :: profil_tab, profil_sous_tab, coord_debut
    INTEGER, DIMENSION(rang_tableau) :: profil_tab_vue, profil_sous_tab_vue, coord_debut_vue
    TYPE(MPI_Datatype)               :: type_sous_tab, type_sous_tab_vue
    ! Error handling
    ! CHARACTER(LEN=MPI_MAX_ERROR_STRING) :: erreur
    ! INTEGER                             :: longueur_erreur

    write(*,*) 'ecrire_mpi', rang, "waiting"
    !Ouverture du fichier "donnees.dat" en écriture
    CALL MPI_FILE_OPEN(comm2d, 'donnees.dat', MPI_MODE_WRONLY + MPI_MODE_CREATE, MPI_INFO_NULL, descripteur, ierr)
    CALL MPI_BARRIER(comm2d, ierr)

    !Ouverture du fichier "donnees.dat" en écriture
    CALL MPI_FILE_OPEN(comm2d, "donnees.dat", &
         MPI_MODE_WRONLY + MPI_MODE_CREATE, &
         MPI_INFO_NULL, descripteur)

    !Creation du type derive type_sous_tab correspondant a la matrice u
    !sans les cellules fantomes

    !Profil du tableau u
    profil_tab(:) = SHAPE(u)

    !Profil du sous tableau
    profil_sous_tab(:) = SHAPE(u(sx:ex, sy:ey))

    !Coordonnees de depart du sous tableau
    coord_debut(:) = (/ 1, 1 /)

    !Creation du type derive type_sous_tab
    CALL MPI_TYPE_CREATE_SUBARRAY(rang_tableau, profil_tab, profil_sous_tab, &
     coord_debut, MPI_ORDER_FORTRAN, typedp, type_sous_tab)

    !Validation du type_derive type_sous_tab
    CALL MPI_TYPE_COMMIT(type_sous_tab)

    !Creation du type type_sous_tab_vue pour la vue sur le fichier

    !Profil du tableau global
    profil_tab_vue(:) = (/ ntx, nty /)

    !Profil du sous tableau
    profil_sous_tab_vue(:) = SHAPE(u(sx:ex, sy:ey))

    !Coordonnees de depart du sous tableau
    coord_debut_vue(:) =  (/ sx-1, sy-1 /)

    !Creation du type_derive type_sous_tab_vue
    CALL MPI_TYPE_CREATE_SUBARRAY(rang_tableau, profil_tab_vue, &
     profil_sous_tab_vue, coord_debut_vue, &
     MPI_ORDER_FORTRAN, typedp, type_sous_tab_vue)

    !Validation du type_derive type_sous_tab_vue
    CALL MPI_TYPE_COMMIT(type_sous_tab_vue)

    !Définition de la vue sur le fichier a partir du debut
    deplacement_initial = 0
    CALL MPI_FILE_SET_VIEW(descripteur, deplacement_initial, typedp, &
     type_sous_tab_vue, "native", MPI_INFO_NULL)

    !Ecriture du tableau u par tous les processus avec la vue
    CALL MPI_FILE_WRITE_ALL(descripteur, u, 1, type_sous_tab, statut)

    ! Fermeture du fichier
    CALL MPI_FILE_CLOSE(descripteur)

    ! Nettoyage des types MPI
    CALL MPI_TYPE_FREE(type_sous_tab)
    CALL MPI_TYPE_FREE(type_sous_tab_vue)
    write(*,*) 'ecrire_mpi', rang, "done"
  END SUBROUTINE ecrire_mpi

  SUBROUTINE finalisation_mpi
    !************
    !Desactivation de l'environnement MPI
    !************
    ! Nettoyages des types et comm MPI
    call MPI_COMM_FREE(comm2d)
    write(*,*) 'finalisation_mpi', rang, "waiting for 2"
    call MPI_TYPE_FREE(type_ligne)
    write(*,*) 'finalisation_mpi', rang, "waiting for 3"
    call MPI_TYPE_FREE(type_colonne)
    write(*,*) 'finalisation_mpi', rang, "waiting for 4"
    ! call MPI_TYPE_FREE(type_tableau)
    !call MPI_TYPE_FREE(typedp)
    ! Desactivation de MPI
    call MPI_FINALIZE()

  END SUBROUTINE finalisation_mpi

  subroutine VTSWriter(Time,Step,nx,ny,x,y,T,opt)
    !-----------------------------------------------------------------------------#
    !  Time    : Reel, temps physique                                             #
    !  Step    : Entier, pas de temps = numero dans le nom de fichier,            #
    !            si Step<0 on ecrit !                                             #
    !            dans sol_exacte.vts, entier                                      #
    !  nx      : Entier, nombre des cellules en direction x                       #
    !  ny      : Entier, nombre des cellules en direction y                       #
    !  x       : Tableau reel (de taille nx+1,ny+1) des abscisses des noeuds      #
    !            des  volumes                                                     #
    !  y       : Tableau reel (de taille nx+1,ny+1) des ordonnees des noeuds      #
    !            des  volumes                                                     #
    !  T       : Tableaux reel (de taille nx par ny) des valeurs a tracer         #
    !            (valeurs au centre des volumes de controle)                      #
    !  U       : Tableaux reel (de taille nx+1   par ny) des valeurs a tracer     #
    !            (valeurs au centre des facettes de normale + ou -x               #
    !  V       : Tableaux reel (de taille nx par ny+1  ) des valeurs a tracer     #
    !            (valeurs au centre des facettes de normale + ou -y               #
    !  opt     : Variable de type chaine des characteres qui doit prendre         #
    !            l'une des valeurs suivantes :                                    #
    !              - 'ini' pour le premier appel a VTSWriter                      #
    !              - 'int' pour un appel standard a VTSWriter                     #
    !              - 'end' pour le dernier appel a VTSWriter                      #
    !-----------------------------------------------------------------------------#
      implicit none
    
      real, intent(in)                       :: Time
      integer, intent(in)                    :: Step, nx, ny
      real, dimension(nx+1,ny+1), intent(in) :: x, y
      real, dimension(nx,ny)    , intent(in) :: T
      character(3), intent(in)               :: opt
    
      character(100) :: num2char
      character(200) :: FileName, formatperso
      integer :: i, j
    
      !  --- Ecriture d un fichier temporel au format paraview  ---
      write(num2char,'(i9.9)') Step
      FileName = 'sol_'//trim(num2char)//'.vts'
      open(8,file=FileName)
      write(num2char,*) 3*(nx+1)*(ny+1)
      formatperso = '('//trim(num2char)//'(E15.9,1x))'
      write(8,'(a)') '<?xml version="1.0"?>'
      write(8,'(a)') '<VTKFile type="StructuredGrid">'
      write(8,'(a,6i6,a)') '<StructuredGrid WholeExtent="', 0,nx,0,ny,0,0,'">'
      write(8,'(a,6i6,a)') '<Piece Extent="',0,nx,0,ny,0,0,'">'
      write(8,'(a)') '<Points>'
      write(8,'(a)') '<DataArray type="Float32" NumberOfComponents="3"/>'
      ! Ecriture des coordonnees du maillage
      DO j=1,ny+1
         write(8,formatperso) (x(i,j),y(i,j),0.,i=1,nx+1)
      END DO
      write(8,'(a)') '</Points>'
      write(8,'(a)') '<CellData Scalars="Temperature, U, V">'
    
      ! Ecriture du scalaire (temperature / concentration)
      write(8,'(a)') '<DataArray type="Float32" Name="Temp, K"/>'
      write(num2char,*) (nx)*(ny)
      DO j=1,ny
         write(8,formatperso) (T(i,j),i=1,nx)
      END DO
  
    
      write(8,'(a)') '</CellData>'
      write(8,'(a)') '</Piece>'
      write(8,'(a)') '</StructuredGrid>'
      write(8,'(a)') '</VTKFile>'
      close(8)
    
      ! - Remplissage du fichier "Collection" determinant l evolution temporelle -
      if (opt == 'ini' ) then
        open(10,file='sol.pvd')
        write(10,'(a)') '<?xml version="1.0"?>'
        write(10,*) '<VTKFile type="Collection" version="0.1" format="ascii">'
        write(10,*) '<Collection>'
      else
        open(10,file='sol.pvd',position='append')
      end if
      if (Step >= 0) write(10,*) '<DataSet timestep="',Time,'" group="" part="0" file="',trim(FileName),'"/>'
      if ( opt == 'end') then
        write(10,*) '</Collection>'
        write(10,*) '</VTKFile>'
      end if
      close(10)
    
    end subroutine VTSWriter
    
    !***********************************
    subroutine mesh(x,y,nx,ny)
      !***********************************
      
      implicit none
      
      integer, intent(in) :: nx,ny
      real, dimension(nx+1,ny+1), intent(out) :: x,y
      
      integer ::i,j
      
      print*,'creating mesh...'
      
      do i=1,nx+1
        do j=1,ny+1
          x(i,j)=real(i-1)/real(nx)
          y(i,j)=real(j-1)/real(ny)
        end do
      end do
      
      end subroutine mesh 

END MODULE parallel

