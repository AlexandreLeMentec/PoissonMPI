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
  TYPE(MPI_Datatype)                        :: typedp,type_ligne, type_colonne
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

    !Creation du type type_ligne pour echanger les points
    !au nord et au sud
    call MPI_TYPE_VECTOR(ex-sx+1,1,ey-sy+1,MPI_REAL,type_ligne) ! not exactly sure about the second 1 here 
    call MPI_TYPE_COMMIT(type_ligne)

    !Creation du type type_colonne pour echanger
    !les points  a l'ouest et a l'est
    call MPI_TYPE_CONTIGUOUS(ey-sy+1,MPI_REAL,type_colonne)
    call MPI_TYPE_COMMIT(type_colonne)

    !type dérivé dp
    call MPI_TYPE_CREATE_F90_REAL(15,307,typedp) 
  END SUBROUTINE type_derive


  SUBROUTINE communication(u)
    !************
    !Echange des points aux interfaces
    !************

    REAL(kind=dp), ALLOCATABLE, DIMENSION(:, :), INTENT(inout) :: u

    !Constantes MPI
    INTEGER, PARAMETER                   :: etiquette=100
    TYPE(MPI_Status)                     :: statut

    ! Note de Alex 🐱 : Je pense qe le plus pratique ici serait d'utiliser des communications bloquantes au début. Sauf cas extrêmes, les procs devraient
    ! recevoir une charge de travail proche et il parait donc assez dérisoire pour le moment de vouloir gagner en fluidité au risque de perdre des donnée 
    ! en utilisant des méthodes bufferisé. Pour éviter des deadlock, go utiliser un simple sendrecv

    !Envoi au voisin N et reception du voisin S
    call MPI_Sendrecv(u(sx:ex,sy), ex-sx+1, type_ligne, voisin(N), 1, u(sx:ex,ey+1), ex-sx+1, type_ligne, voisin(S), 1, comm2d, MPI_STATUS_IGNORE)
    ! bon la fonction a de la gueule mais pour le moment à part avoir de pb de pointeur ça fait pas grand chose. TODO: check le pas dans la subroutine précédente

    !Envoi au voisin S et reception du voisin N
    call MPI_Sendrecv(u(sx:ex,ey), ex-sx+1, type_ligne, voisin(S), 2, u(sx:ex,sy-1), ex-sx+1, type_ligne, voisin(N), 2, comm2d, MPI_STATUS_IGNORE)

    !Envoi au voisin W et reception du voisin E 
    call MPI_Sendrecv(u(sx,ey:sy), ey-sy+1, type_colonne, voisin(W), 3, u(ex+1,ey:sy), ey-sy+1, type_colonne, voisin(E), 3, comm2d, MPI_STATUS_IGNORE) 

    !Envoi au voisin E et reception du voisin W 
    call MPI_Sendrecv(u(ex,ey:sy), ey-sy+1, type_colonne, voisin(E), 4, u(sx-1,ey:sy), ey-sy+1, type_colonne, voisin(W), 4, comm2d, MPI_STATUS_IGNORE) 
  END SUBROUTINE communication

  FUNCTION erreur_globale(u, u_nouveau)
    !************
    !Calcul de l'erreur globale (maximum des erreurs locales)
    !************
    REAL(kind=dp), ALLOCATABLE, DIMENSION(:, :), INTENT(in) :: u, u_nouveau

    REAL(kind=dp)              :: erreur_globale, erreur_locale

    erreur_locale = MAXVAL(ABS(u(sx:ex, sy:ey) - u_nouveau(sx:ex, sy:ey)))

    !Calcul de l'erreur sur tous les sous-domaines

    call MPI_Allreduce(erreur_locale, erreur_globale, 1, typedp, MPI_SUM, comm2d)

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
    CHARACTER(LEN=MPI_MAX_ERROR_STRING) :: erreur
    INTEGER                             :: longueur_erreur

    !Ouverture du fichier "donnees.dat" en écriture
    CALL MPI_FILE_OPEN(comm2d, 'donnees.dat', MPI_MODE_WRONLY + MPI_MODE_CREATE, MPI_INFO_NULL, descripteur, ierr)

    ! Changement du gestionnaire d'erreur pour les fichiers
    IF (ierr /= MPI_SUCCESS) THEN
      CALL MPI_ERROR_STRING(ierr, erreur, longueur_erreur, ierr)
      WRITE(*,*) 'Erreur MPI : ', erreur
      CALL MPI_ABORT(MPI_COMM_WORLD, 1, ierr)
    END IF


    !Creation du type derive type_sous_tab correspondant a la matrice u
    !sans les cellules fantomes

    !Creation du type type_sous_tab_vue pour la vue sur le fichier

    !Définition de la vue sur le fichier a partir du debut

    !Ecriture du tableau u par tous les processus avec la vue

    ! Fermeture du fichier
    CALL MPI_FILE_CLOSE(descripteur, ierr)

    ! Nettoyage des types MPI

  END SUBROUTINE ecrire_mpi

  SUBROUTINE finalisation_mpi
    !************
    !Desactivation de l'environnement MPI
    !************
    ! Nettoyages des types et comm MPI
    call MPI_COMM_FREE(comm2d)
    call MPI_TYPE_FREE(type_ligne)
    call MPI_TYPE_FREE(type_colonne)
    call MPI_TYPE_FREE(typedp)
    ! Desactivation de MPI
    call MPI_FINALIZE()

  END SUBROUTINE finalisation_mpi

END MODULE parallel

