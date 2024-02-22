program bcast
    use MPI_F08
    implicit none

    integer :: rang, valeur

    call MPI_INIT()
    call MPI_Comm_rank(MPI_Comm_world, rang)

    if (rang == 2) 
    valeur = rang + 1000

    call MPI_BCAST(valeur,1,MPI_INTEGER,2,MPI_COMM_WORLD)
    print*, 'Moi, processus', rang, 'ai recu', valeur, 'du processus 2'

    call MPI_Finalize()

end program bcast