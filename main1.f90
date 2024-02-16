
program point_a_point

    !use m_type
    ! Include MPI library
    use mpi
    implicit none

    ! Data declarations
    TYPE(MPI_Status) :: statut
    integer, parameter :: etiquette=100
    integer :: rang, valeur

    call MPI_INIT()

    call MPI_COMM_RANK(MPI_COMM_WORLD, rang)
    
    if (rang == 2) then
        valeur = 1000
        call MPI_SEND(valeur, 1, MPI_INTEGER, 5, etiquette, MPI_COMM_WORLD)
    else if (rang == 1) then
        call MPI_RECV(valeur, 1, MPI_INTEGER, 2, etiquette, MPI_COMM_WORLD, statut)
        print *, 'Processus 5 a recu la valeur', valeur, 'du processus 2'
    end if
    
    call MPI_FINALIZE()

end program point_a_point



