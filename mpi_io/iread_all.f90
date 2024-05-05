program iread_all
    use mpi_f08
    implicit none

    integer                         :: rang, code
    TYPE(MPI_File)                  :: descripteur
    integer, parameter              :: nb_valeurs = 10
    integer, dimension(nb_valeurs)  :: valeurs
    TYPE(MPI_Status)                :: statut
    TYPE(MPI_REQUEST)               :: req

    call MPI_Init()
    call MPI_Comm_rank(MPI_COMM_WORLD, rang)

    call MPI_File_open(MPI_COMM_WORLD, "donnees.dat", MPI_MODE_RDONLY, MPI_INFO_NULL, descripteur)

    call MPI_File_iread_all(descripteur, valeurs, 4, MPI_INTEGER, req)
    print *, "Processus numero  = ", rang
    call MPI_Wait(req, statut)
    
    print *, "Lecture processus = ", rang, " : ", valeurs(1:4)

    call MPI_File_close(descripteur)
    call MPI_Finalize()

end program iread_all
