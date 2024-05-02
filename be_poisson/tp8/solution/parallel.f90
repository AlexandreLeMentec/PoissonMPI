!parallel.f90
!!!!
!!subroutine initialisation_mpi
!!subroutine creation_topologie
!!subroutine domaine
!!subroutine voisinage
!!subroutine type_derive
!!subroutine communication
!!subroutine communication_non_bloquante
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
  ! Etiquettes pour les communications non bloquantes
  TYPE(MPI_Request), DIMENSION(2*NB_VOISINS):: requete
  !Constantes MPI
  INTEGER                                   :: code

CONTAINS

  SUBROUTINE initialisation_mpi
    !************
    !Initialisation pour chaque processus de son rang et du
    !nombre total de processus nb_procs
    !************

    !Initialisation de MPI
    CALL MPI_INIT()

    !Savoir quel processus je suis
    CALL MPI_COMM_RANK(MPI_COMM_WORLD, rang)

    !Connaitre le nombre total de processus
    CALL MPI_COMM_SIZE(MPI_COMM_WORLD, nb_procs)

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
    dims(:)= 0
    CALL MPI_DIMS_CREATE(nb_procs, ndims, dims)

    !Creation de la grille de processus 2D sans periodicite
    periods(:) = .FALSE.
    CALL MPI_CART_CREATE(MPI_COMM_WORLD, ndims, dims, periods, &
         reorganisation, comm2d)

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

    ! Connaitre mes coordonnees dans la topologie
    CALL MPI_CART_COORDS(comm2d, rang, ndims, coords)

    !Calcul pour chaque processus de ses indices de debut et de fin suivant x
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
    CALL MPI_CART_SHIFT(comm2d, 0, 1, voisin(N), voisin(S))

    !Recherche des voisins Ouest et Est
    CALL MPI_CART_SHIFT(comm2d, 1, 1, voisin(W), voisin(E))

    WRITE (*,'(A,i4,A,i4,A,i4,A,i4,A,i4)') "Processus ", rang, &
     " a pour voisin : N", voisin(N), " E", voisin(E), &
     " S", voisin(S), " W", voisin(W)

  END SUBROUTINE voisinage


  SUBROUTINE type_derive
    !************
    !Creation des types derives type_ligne et type_colonne
    !************
    CALL MPI_TYPE_CREATE_F90_REAL(15,307,typedp)
    !Creation du type type_ligne pour echanger les points
    !au nord et au sud
    CALL MPI_TYPE_VECTOR(ey-sy+1, 1, ex-sx+3, typedp, type_ligne)
    CALL MPI_TYPE_COMMIT(type_ligne)

    !Creation du type type_colonne pour echanger
    !les points  a l'ouest et a l'est
    CALL MPI_TYPE_CONTIGUOUS(ex-sx+1, typedp, type_colonne)
    CALL MPI_TYPE_COMMIT(type_colonne)

  END SUBROUTINE type_derive


  SUBROUTINE communication(u)
    !************
    !Echange des points aux interfaces
    !************

    REAL(kind=dp), ALLOCATABLE, DIMENSION(:, :), INTENT(inout) :: u

    !Constantes MPI
    INTEGER, PARAMETER                   :: etiquette=100
    TYPE(MPI_Status)                     :: statut
!    !MPI 3.0 version 
!    INTEGER                              :: nbligne, nbcol, sizedouble
!    INTEGER(KIND=MPI_ADDRESS_KIND)       :: displ
    
!    nbligne = ex-sx+3
!    nbcol   = ey-sy+3
!    call MPI_TYPE_SIZE(MPI_DOUBLE_PRECISION, sizedouble, code)
!    displ = sizedouble
    
!    call MPI_NEIGHBOR_ALLTOALLW(u, &
!                                (/1,1,1,1/), &
!                                (/ displ*(nbligne+1), displ*(2*nbligne-2),displ*(nbligne+1),displ*((nbcol-2)*nbligne+1)/), &
!                                (/type_ligne,type_ligne,type_colonne,type_colonne/), &
!                                u, &
!                                (/1,1,1,1/), &
!                                (/ displ*(nbligne), displ*(2*nbligne-1), displ*1, displ*((nbcol-1)*nbligne+1)/), &
!                                (/type_ligne,type_ligne,type_colonne,type_colonne/), &
!                                comm2d, &
!                                code)

    !Envoi au voisin N et reception du voisin S
    CALL MPI_SENDRECV(u(sx, sy), 1, type_ligne,   voisin(N), &
         etiquette, u(ex+1, sy), 1, type_ligne,   voisin(S), &
         etiquette, comm2d, statut)

    !Envoi au voisin S et reception du voisin N
    CALL MPI_SENDRECV(u(ex, sy), 1, type_ligne,   voisin(S), &
         etiquette, u(sx-1, sy), 1, type_ligne,   voisin(N), &
         etiquette, comm2d, statut)

    !Envoi au voisin W et reception du voisin E
    CALL MPI_SENDRECV(u(sx, sy), 1, type_colonne, voisin(W), &
         etiquette, u(sx, ey+1), 1, type_colonne, voisin(E), &
         etiquette, comm2d, statut)

    !Envoi au voisin E et  reception du voisin W
    CALL MPI_SENDRECV(u(sx, ey), 1, type_colonne, voisin(E), &
         etiquette, u(sx, sy-1), 1, type_colonne, voisin(W), &
         etiquette, comm2d, statut)

  END SUBROUTINE communication

  SUBROUTINE debut_communication(u)
    !************
    !Echange des points aux interfaces
    !************

    REAL(kind=dp), ALLOCATABLE, DIMENSION(:, :), INTENT(inout) :: u

    !Constantes MPI
    INTEGER, PARAMETER                        :: etiquette=100

    !Envoi au voisin N et reception du voisin S
    CALL MPI_IRECV(u(ex+1, sy), 1, type_ligne, voisin(S), &
         etiquette, comm2d, requete(1))
    CALL MPI_ISEND(u(sx, sy),   1, type_ligne, voisin(N), &
         etiquette, comm2d, requete(2))

    !Envoi au voisin S et reception du voisin N
    CALL MPI_IRECV(u(sx-1, sy), 1, type_ligne, voisin(N), &
         etiquette, comm2d, requete(3))
    CALL MPI_ISEND(u(ex, sy),   1, type_ligne, voisin(S), &
         etiquette, comm2d, requete(4))

    !Envoi au voisin W et  reception du voisin E
    CALL MPI_IRECV(u(sx, ey+1), 1, type_colonne, voisin(E), &
         etiquette, comm2d, requete(5))
    CALL MPI_ISEND(u(sx, sy),   1, type_colonne, voisin(W), &
         etiquette, comm2d, requete(6))

    !Envoi au voisin E et  reception du voisin W
    CALL MPI_IRECV(u(sx, sy-1), 1, type_colonne, voisin(W), &
         etiquette, comm2d, requete(7))
    CALL MPI_ISEND(u(sx, ey),   1, type_colonne, voisin(E), &
         etiquette, comm2d, requete(8))
  END SUBROUTINE debut_communication

  SUBROUTINE fin_communication
    !************
    !Echange des points aux interfaces
    !************

    !Constantes MPI
    TYPE(MPI_Status), DIMENSION(2*NB_VOISINS) :: tab_statut

    CALL MPI_WAITALL(2*NB_VOISINS, requete, tab_statut)
  END SUBROUTINE fin_communication

  FUNCTION erreur_globale(u, u_nouveau)
    !************
    !Calcul de l'erreur globale (maximum des erreurs locales)
    !************
    REAL(kind=dp), ALLOCATABLE, DIMENSION(:, :), INTENT(in) :: u, u_nouveau

    REAL(kind=dp)              :: erreur_globale, erreur_locale

    erreur_locale = MAXVAL(ABS(u(sx:ex, sy:ey) - u_nouveau(sx:ex, sy:ey)))

    !Calcul de l'erreur sur tous les sous-domaines
    CALL MPI_ALLREDUCE(erreur_locale, erreur_globale, 1, typedp, &
                       MPI_MAX, comm2d)

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

    ! Changement du gestionnaire d'erreur pour les fichiers
    call MPI_FILE_SET_ERRHANDLER(MPI_FILE_NULL,MPI_ERRORS_ARE_FATAL)

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

  END SUBROUTINE ecrire_mpi

  SUBROUTINE finalisation_mpi
    !************
    !Desactivation de l'environnement MPI
    !************

    ! Nettoyages des types et comm MPI
    CALL MPI_COMM_FREE(comm2d)
    CALL MPI_TYPE_FREE(type_ligne)
    CALL MPI_TYPE_FREE(type_colonne)
    ! Desactivation de MPI
    CALL MPI_FINALIZE()

  END SUBROUTINE finalisation_mpi

END MODULE parallel

