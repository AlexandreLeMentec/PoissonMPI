program seek
    use mpi_f08
    implicit none

    integer, parameter                  :: nb_valeurs = 10
    integer                             :: rang, nb_octets_entier, code
    TYPE(MPI_FILE)                      :: descripteur
    integer(kind=MPI_OFFSET_KIND)       :: position_fichier
    integer, dimension(nb_valeurs)      :: valeurs
    TYPE(MPI_Status)                    :: statut

    call MPI_INIT()
    call MPI_COMM_RANK(MPI_COMM_WORLD, rang)
    call MPI_FILE_OPEN(MPI_COMM_WORLD, 'donnees.dat', MPI_MODE_RDONLY, MPI_INFO_NULL, descripteur)

    call MPI_FILE_READ(descripteur, valeurs, 3, MPI_INTEGER, statut)
    call MPI_TYPE_SIZE(MPI_INTEGER, nb_octets_entier)
    position_fichier = 8 * nb_octets_entier

    call MPI_FILE_SEEK(descripteur, position_fichier, MPI_SEEK_CUR)
    call MPI_FILE_READ(descripteur, valeurs(4), 3, MPI_INTEGER, statut)
    position_fichier = 4 * nb_octets_entier

    call MPI_FILE_SEEK(descripteur, position_fichier, MPI_SEEK_SET)
    call MPI_FILE_READ(descripteur, valeurs(7), 4, MPI_INTEGER, statut)

    print *, 'Lecture processus', rang, ': valeurs =', valeurs

    call MPI_FILE_CLOSE(descripteur)
    call MPI_FINALIZE()

end program seek