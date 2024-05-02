PROGRAM lire_tab

  USE mpi

  IMPLICIT NONE

  INTEGER, dimension(MPI_STATUS_SIZE)            :: statut
  INTEGER                                        :: rang, code, ntx, nty
  INTEGER                                        :: descripteur
  ! Kind pour double precision
  integer, parameter                             :: dp = selected_real_kind(15,307)
  REAL(kind=dp), ALLOCATABLE, DIMENSION(:, :)    :: u_lu
  INTEGER(KIND=MPI_OFFSET_KIND)                  :: taille_fichier
  INTEGER                                        :: taille_reel
  integer                                        :: typedp
  character(len=MPI_MAX_ERROR_STRING)            :: texte_erreur
  integer                                        :: texte_longueur
  
  CALL MPI_INIT(code)

  CALL MPI_COMM_RANK(MPI_COMM_WORLD, rang, code)

  OPEN(11, FILE='poisson.data', STATUS='OLD')
  READ(11, *) ntx
  READ(11, *) nty
  CLOSE(11)

  ALLOCATE(u_lu(ntx, nty))
  u_lu(:, :) = 0.d0

  !Ouverture du fichier "donnees.dat" en lecture
  CALL MPI_FILE_OPEN(MPI_COMM_WORLD, "donnees.dat", &
   MPI_MODE_RDONLY, MPI_INFO_NULL, descripteur, code)
  if (code /= MPI_SUCCESS) then
    CALL MPI_ERROR_STRING(code,texte_erreur,texte_longueur)
    print *, texte_erreur(1:texte_longueur)
    call MPI_ABORT(MPI_COMM_WORLD,42)
  end if

  CALL MPI_FILE_SET_ERRHANDLER(MPI_FILE_NULL,MPI_ERRORS_ARE_FATAL,code)
  CALL MPI_FILE_GET_SIZE(descripteur, taille_fichier, code)
  CALL MPI_TYPE_CREATE_F90_REAL(15,307,typedp,code)
  CALL MPI_TYPE_SIZE(typedp, taille_reel, code)
  if (taille_fichier /= ntx*nty*taille_reel) then
    print *, " ATTENTION Donnees.dat n'a pas la bonne longueur"
    print *, " Taille du fichier : ",taille_fichier
    print *, " Taille attendue : ", ntx*nty*taille_reel
    write(11,*) 0
  else
    CALL MPI_FILE_READ(descripteur, u_lu, SIZE(u_lu), &
     typedp, statut, code)
    WRITE(11, 101)  u_lu
101 FORMAT (E19.12)
  end if

  CALL MPI_FILE_CLOSE(descripteur, code)
  
  CALL MPI_FINALIZE(code)

END PROGRAM lire_tab
