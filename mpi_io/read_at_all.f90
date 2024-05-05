program read_at_all
    use mpi_f08
    implicit none

    integer, parameter              :: nb_valeurs = 10
    integer                         :: rang, code, nb_octets_entier
    TYPE(MPI_FILE)                  :: descripteur
    integer(kind=MPI_OFFSET_KIND)   :: position_fichier
    integer, dimension(nb_valeurs)  :: valeurs
    TYPE(MPI_Status)                :: statut

    call MPI_INIT()
    call MPI_COMM_RANK(MPI_COMM_WORLD, rang)

    ! Ouverture du fichier
    call MPI_FILE_OPEN(MPI_COMM_WORLD, 'donnees.dat', MPI_MODE_RDONLY, MPI_INFO_NULL, descripteur)

    call MPI_TYPE_SIZE(MPI_INTEGER, nb_octets_entier)
    position_fichier = rang * nb_valeurs * nb_octets_entier
    call MPI_FILE_READ_AT_ALL(descripteur, position_fichier, valeurs, nb_valeurs, MPI_INTEGER, statut)

    print *, 'Lecture processus', rang, ':', valeurs(:)

    ! Fermeture du fichier
    call MPI_FILE_CLOSE(descripteur)
    call MPI_FINALIZE()

end program read_at_all

