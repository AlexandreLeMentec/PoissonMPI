program write_at
    use mpi_f08
    implicit none
    integer, parameter              :: nb_valeurs = 10
    integer                         :: i, rang, code, nb_octets_entier
    TYPE(MPI_FILE)                  :: descripteur
    integer(kind=MPI_OFFSET_KIND)   :: position_fichier
    integer, dimension(nb_valeurs)  :: valeurs
    TYPE(MPI_STATUS)                :: statut

    call MPI_INIT()
    call MPI_COMM_RANK(MPI_COMM_WORLD, rang)
    valeurs(:)= (/(i+rang*100,i=1,nb_valeurs)/)
    print*, "Ecriture processus", rang, ":", valeurs(:)

    call MPI_FILE_OPEN(MPI_COMM_WORLD, "donnees.dat", &
                       MPI_MODE_WRONLY+MPI_MODE_CREATE, &
                       MPI_INFO_NULL, descripteur, code)
    IF (code /= MPI_SUCCESS) THEN
        print*, "Erreur ouverture fichier"
        call MPI_ABORT(MPI_COMM_WORLD, 42)
    END IF
    call MPI_TYPE_SIZE(MPI_INTEGER, nb_octets_entier)
    position_fichier = rang*nb_valeurs*nb_octets_entier

    call MPI_FILE_SET_ERRHANDLER(descripteur, MPI_ERRORS_RETURN)
    CALL MPI_FILE_WRITE_AT(descripteur, position_fichier, &
                           valeurs, nb_valeurs, MPI_INTEGER, statut)

    call MPI_FILE_CLOSE(descripteur)
    call MPI_FINALIZE()

end program write_at