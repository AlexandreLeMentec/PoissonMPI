program gather
    use mpi_f08
    implicit none
    integer, parameter              :: nb_valeurs =8
    integer                         :: nb_procs, rang, longueur_tranche, i
    real, dimension(nb_valeurs)     :: donnees
    real, allocatable, dimension(:) :: valeurs

    call MPI_Init()
    call MPI_Comm_size(MPI_COMM_WORLD, nb_procs)
    call MPI_Comm_rank(MPI_COMM_WORLD, rang)

    longueur_tranche = nb_valeurs / nb_procs

    allocate(valeurs(longueur_tranche))

    valeurs = [(1000.0 + rang * longueur_tranche + i, i = 1, longueur_tranche)]
    print*, 'Moi, processus', rang, 'envoie mon tableau valeurs : ', & 
        valeurs(1:longueur_tranche)
    
    call MPI_gather(valeurs, longueur_tranche, mpi_real, donnees, longueur_tranche, &
        mpi_real, 2, MPI_COMM_WORLD)

    if (rang == 2)  then
            print*, 'Moi, processus 2', ' ai recu', donnees(1:nb_valeurs)
    end if 
    call MPI_Finalize()
end program gather