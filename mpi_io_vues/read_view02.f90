program read_view02
    use mpi_f08
    implicit none

    integer, parameter              :: nb_valeurs = 10
    integer                         :: rang, code, nb_octets_entier
    type(MPI_File)                  :: descripteur
    type(MPI_Datatype)              :: motif_1, motif_2
    integer (kind=MPI_OFFSET_KIND)  :: deplacement_initial
    integer, dimension(nb_valeurs)  :: valeurs
    type(MPI_Status)                :: statut


    call MPI_Init()
    call MPI_Comm_rank(MPI_COMM_WORLD, rang)

    call MPI_Type_create_subarray(1 , (/4/), (/2/), (/0/), &
     MPI_ORDER_FORTRAN, MPI_INTEGER, motif_1)

    call MPI_Type_commit(motif_1)

    call MPI_Type_create_subarray(1 , (/3/), (/1/), (/2/), &
    MPI_ORDER_FORTRAN, MPI_INTEGER, motif_2)

   call MPI_Type_commit(motif_2)

    call MPI_File_open(MPI_COMM_WORLD, 'donnees.dat', MPI_MODE_RDONLY, &
     MPI_INFO_NULL, descripteur)

     ! Lecture en utilisant le motif 1
    deplacement_initial = 0
    call MPI_File_set_view(descripteur, deplacement_initial, MPI_INTEGER, &
     motif_1, 'native', MPI_INFO_NULL)

    call MPI_FILE_READ(descripteur, valeurs, 4, MPI_INTEGER, statut)
    call MPI_FILE_READ(descripteur, valeurs(5), 3, MPI_INTEGER, statut)

  ! Lecture en utilisant le motif 2
    call MPI_TYPE_SIZE(MPI_INTEGER, nb_octets_entier)
    deplacement_initial = 2 * nb_octets_entier
    call MPI_File_set_view(descripteur, deplacement_initial, MPI_INTEGER, &
     motif_2, 'native', MPI_INFO_NULL)

    call MPI_FILE_READ(descripteur, valeurs(8), 3, MPI_INTEGER, statut)

    print *, 'Lecture processus', rang, 'Valeurs', valeurs

    call MPI_File_close(descripteur)
    call MPI_Finalize()

end program read_view02
