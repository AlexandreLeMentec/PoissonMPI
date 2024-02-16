! Hello World Program in Fortran using MPI

! Write MPI subroutines / variables in UPPER CASE, Fortran subroutines / variables in lower case
!           - user prefeence
!           - makes it easier to distinguish between MPI and Fortran subroutines / variables
program main

    !use m_type
    ! Include MPI library
    use mpi_f08
    implicit none

    TYPE(mpi_Status) :: statut
    ! Data declarations
    !integer :: ierr             !error signal variable. Standard Value = 0
    integer, parameter :: etiquette=100
    integer :: rang, valeur
   ! type (donnees) :: param                          ! Fortran strucutre containing the initial numerical and physical parameters

    ! Initialize MPI
    ! Initialize subroutine
    call MPI_INIT()

    ! Setup Ranks / Process ID
    ! variable order : Communicator, Number of Processes, Error Signal
    call MPI_COMM_RANK(MPI_COMM_WORLD, rang)

    ! Type the main code
    !print *, 'Hello World from process', rank, 'of', nprocs
    if (rang ==2) then
        valeur = 1000
        call MPI_SEND(valeur, 1, MPI_INTEGER, 3, etiquette, MPI_COMM_WORLD)
    elseif (rang == 3) then
        call MPI_RECV(valeur, 1, MPI_INTEGER, 2, etiquette, MPI_COMM_WORLD, statut)
        print *, 'Moi, processus 3, ai recu', valeur, 'du processus 2'
    end if
    ! Finalize MPI
    ! Finalizer subroutine
    call MPI_FINALIZE()

end program main