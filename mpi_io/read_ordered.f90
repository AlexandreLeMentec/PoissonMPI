program read_ordered
    use mpi_f08
    implicit none

    integer                         :: rang, code
    TYPE(MPI_FILE)                  :: descripteur
    integer, parameter              :: nb_valeurs = 10
    integer, dimension(nb_valeurs)  :: valeurs
    TYPE(MPI_Status)                :: statut

    call MPI_INIT()
    call MPI_COMM_RANK(MPI_COMM_WORLD, rang)

    ! Ouverture du fichier
    call MPI_FILE_OPEN(MPI_COMM_WORLD, 'donnees.dat', MPI_MODE_RDONLY, MPI_INFO_NULL, descripteur)
   
    call MPI_FILE_READ_ORDERED(descripteur, valeurs, 4, MPI_INTEGER, statut, code)
    call MPI_FILE_READ_ORDERED(descripteur, valeurs(5), 6, MPI_INTEGER, statut, code)
    print *, 'Lecture processus', rang, ':', valeurs(:)

    ! Fermeture du fichier
    call MPI_FILE_CLOSE(descripteur)
    call MPI_FINALIZE()

end program read_ordered

