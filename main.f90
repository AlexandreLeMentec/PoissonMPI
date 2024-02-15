program main

    use m_type
    use mpi_f08

    implicit none

    !data declaration for MPI
    type (donnees) :: param
    integer :: ierr   ! error signal variable. Standard Value = 0
    integer :: rank   ! process ID/ nb
    integer :: nprocs ! nb process

    ! Initialize MPI
    ! Initializer subroutine
    call MPI_INIT(ierr)

    !setup Communicator Size
    !variable order : Communicator, number of 
    call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierr)

    !setup Ranks/IDs for each process
    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)

    print *, "Hello world from process", rank+1, "of", nprocs

    !Finalize MPI
    !Finalizer subroutine
    call MPI_FINALIZE(ierr)

end program main


