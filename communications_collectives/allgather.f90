program allgather
    use mpi_f08
    implicit none
    
    integer, parameter              :: nb_valeurs = 8
    integer                         :: nb_procs, rang, longueur_tranche, i
    real, dimension(nb_valeurs)     :: donnees
    real, allocatable, dimension(:) :: valeurs

    call MPI_Init()

    call MPI_Comm_size(MPI_COMM_WORLD, nb_procs)
    call MPI_Comm_rank(MPI_COMM_WORLD, rang)

    longueur_tranche = nb_valeurs / nb_procs
    allocate(valeurs(longueur_tranche))

    valeurs = [(1000.0 + rang * longueur_tranche + i, i = 1, longueur_tranche)]
    call MPI_ALLGATHER(valeurs, longueur_tranche, MPI_REAL, donnees, longueur_tranche, &
    MPI_REAL, MPI_COMM_WORLD)

    print*, 'Moi, processus', rang, 'ai recu', donnees(1:nb_valeurs)

    call MPI_Finalize()

end program allgather