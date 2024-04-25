program sendrecv
    use mpi_f08
    implicit none
    integer :: rang, valeur, num_proc
    integer, parameter :: etiquette =110

    call MPI_Init()
    call MPI_Comm_rank(MPI_COMM_WORLD, rang)

    ! On définit le rang du processus avec lequel on va communiquer (on suppose avoir exactement 2 processus)
    num_proc = mod(rang+1,2)

    call MPI_Sendrecv(rang+1000, 1, MPI_INTEGER, num_proc, etiquette, valeur, 1, MPI_INTEGER, &
     num_proc, etiquette, MPI_COMM_WORLD, MPI_STATUS_IGNORE)
    print *, "Processus ", rang, "ai reçu", valeur, "du processus", num_proc

    call MPI_Finalize()
end program sendrecv
