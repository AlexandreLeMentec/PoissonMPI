program read_view01
    use mpi_f08
    implicit none

    integer, parameter              :: nb_valeurs = 10
    integer                         :: rang, coord, code
    type(MPI_Datatype)              :: motif
    type(MPI_File)                  :: descripteur
    integer (kind=MPI_OFFSET_KIND)  :: deplacement_initial
    integer, dimension(nb_valeurs)  :: valeurs
    type(MPI_Status)                :: statut


    call MPI_Init()
    call MPI_Comm_rank(MPI_COMM_WORLD, rang)

    if (rang == 0) coord = 1
    if (rang == 1) coord = 3

    call MPI_Type_create_subarray(1 , (/4/), (/2/), (/coord-1/), &
     MPI_ORDER_FORTRAN, MPI_INTEGER, motif)

    call MPI_Type_commit(motif)

    call MPI_File_open(MPI_COMM_WORLD, 'donnees.dat', MPI_MODE_RDONLY, &
     MPI_INFO_NULL, descripteur)

    deplacement_initial = 0
    call MPI_File_set_view(descripteur, deplacement_initial, MPI_INTEGER, &
     motif, 'native', MPI_INFO_NULL)

    call MPI_File_read_all(descripteur, valeurs, nb_valeurs, MPI_INTEGER, statut)

    print *, 'Lecture processus', rang, 'Valeurs', valeurs

    call MPI_File_close(descripteur)
    call MPI_Finalize()

end program read_view01
