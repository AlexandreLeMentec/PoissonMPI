program scatter
    use MPI_F08
    implicit none

    integer, parameter  :: nb_valeurs = 8
    integer             :: nb_procs, rang, longueur_tranche,i
    real, allocatable, dimension(:) :: valeurs,donnees

    call MPI_Init()
    call MPI_Comm_size(MPI_COMM_WORLD, nb_procs)
    call MPI_Comm_rank(MPI_COMM_WORLD, rang)
    longueur_tranche = nb_valeurs / nb_procs            ! longueur des messages que les processus vont recevoir du processeur 2 

    allocate(donnees(longueur_tranche))

    if (rang==2) then
        allocate(valeurs(nb_valeurs))
        valeurs(:) = (/ (1000.+i, i=1, nb_valeurs)/)
        print*,'Moi, processus', rang, 'envoie mon tableau valeurs : ', & 
            valeurs(1:nb_valeurs)
    end if

    call mpi_scatter(valeurs, longueur_tranche, MPI_REAL, donnees, longueur_tranche, &
    mpi_real, 2, MPI_COMM_WORLD)
    print*, 'Moi, processus', rang, ',ai recu', donnees(1:longueur_tranche), &
        'du processus 2'

    call MPI_Finalize()

end program scatter