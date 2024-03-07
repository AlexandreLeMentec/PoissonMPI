program allreduce

    use mpi_f08
    implicit none

    integer :: nb_procs, rang, valeur, produit

    call MPI_Init()
    call MPI_Comm_size(MPI_COMM_WORLD, nb_procs)
    call MPI_Comm_rank(MPI_COMM_WORLD, rang)

    if (rang ==0) then  
        valeur = 10
    else
        valeur = rang
    endif

    call MPI_ALLREDUCE(valeur,produit,1, MPI_INTEGER,MPI_PROD, MPI_COMM_WORLD)

    print*,'Moi, processus', rang, 'ai recu la valeur du produit global', produit

    call MPI_Finalize()

end program allreduce
    