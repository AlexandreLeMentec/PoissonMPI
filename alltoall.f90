program alltoall
    USE mpi_f08
    implicit none
 
    integer             :: nb_procs, rang, longueur_tranche, i  
    integer, parameter  :: nb_valeurs = 8
    real, dimension(nb_valeurs) :: valeurs, donnees

    call MPI_Init()
    call MPI_Comm_size(MPI_COMM_WORLD, nb_procs)
    call MPI_Comm_rank(MPI_COMM_WORLD, rang)

    valeurs(:) = [( 1000.+rang*nb_valeurs+i,i=1,nb_valeurs)]
    longueur_tranche = nb_valeurs / nb_procs

    print*, 'Moi, processus,', rang, "envoie mon tableau valeurs : ", &
        valeurs(1:nb_valeurs)

    call MPI_ALLTOALL(valeurs,longueur_tranche,mpi_real, donnees, longueur_tranche, &
        mpi_real, MPI_COMM_WORLD)

    print*, 'Moi, processus', rang, 'ai recu', donnees(1:nb_valeurs)

    call MPI_Finalize()

end program alltoall
    