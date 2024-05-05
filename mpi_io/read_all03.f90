program read_all03
    use mpi_f08
    implicit none

    integer, parameter              :: nb_valeurs = 10
    integer                         :: rang, code
    TYPE(MPI_FILE)                  :: descripteur
    integer, dimension(nb_valeurs)  :: valeurs=0
    TYPE(MPI_Status)                :: statut

    call MPI_INIT()
    call MPI_COMM_RANK(MPI_COMM_WORLD, rang)

    ! Ouverture du fichier
    call MPI_FILE_OPEN(MPI_COMM_WORLD, 'donnees.dat', MPI_MODE_RDONLY, MPI_INFO_NULL, descripteur)
   
    if (rang == 0) then
        call MPI_FILE_READ_ALL(descripteur, valeurs(3), 4, MPI_INTEGER, statut)
    else
        call MPI_FILE_READ_ALL(descripteur, valeurs(5), 5, MPI_INTEGER, statut)
    end if

    print *, 'Lecture processus', rang, ':', valeurs(:)

    ! Fermeture du fichier
    call MPI_FILE_CLOSE(descripteur)
    call MPI_FINALIZE()

end program read_all03

