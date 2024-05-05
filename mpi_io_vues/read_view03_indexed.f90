program read_view03_indexed
    use mpi_f08
    implicit none

    integer, parameter              :: nb_valeurs = 9
    integer                         :: rang, code, nb_octets_entier
    type(MPI_File)                  :: descripteur
    type(MPI_Datatype)              :: motif_temp,motif
    integer (kind=MPI_OFFSET_KIND)  :: deplacement_initial
    integer (kind=MPI_ADDRESS_KIND) :: borne_inf, etendue
    integer, dimension(2)           :: longueurs, deplacements
    integer, dimension(nb_valeurs)  :: valeurs
    type(MPI_Status)                :: statut


    call MPI_Init()
    call MPI_Comm_rank(MPI_COMM_WORLD, rang)

    ! motif_temp: type MPI d’etendu 4*MPI_INTEGER
    deplacements(1) = 0
    longueurs(1) = 2
    deplacements(2) = 3
    longueurs(2) = 1

    call MPI_TYPE_INDEXED(2, longueurs, deplacements, MPI_INTEGER, &
     motif_temp)
    
    ! motif : type MPI d'etendu 5*MPI_INTEGER
     call MPI_TYPE_SIZE(MPI_INTEGER, nb_octets_entier)
     call MPI_TYPE_GET_EXTENT(motif_temp, borne_inf, etendue)
    etendue = etendue + nb_octets_entier

    call MPI_TYPE_CREATE_RESIZED(motif_temp, borne_inf, borne_inf + etendue, motif)
    call MPI_TYPE_COMMIT(motif)

    call MPI_File_open(MPI_COMM_WORLD, 'donnees.dat', MPI_MODE_RDONLY, &
     MPI_INFO_NULL, descripteur)

    deplacement_initial = 0

    call MPI_File_set_view(descripteur, deplacement_initial, MPI_INTEGER, &
     motif, 'native', MPI_INFO_NULL, code)



    call MPI_FILE_READ(descripteur, valeurs, 9, MPI_INTEGER, statut)

    print *, 'Lecture processus', rang, 'Valeurs', valeurs

    call MPI_File_close(descripteur)
    call MPI_Finalize()

end program read_view03_indexed
