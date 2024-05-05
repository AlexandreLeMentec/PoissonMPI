program iread_at
    use mpi_f08
    implicit none

    integer, parameter              :: nb_valeurs = 10        
    integer                         :: i, nb_iterations = 0, rang, nb_octets_entier, code
    TYPE(MPI_REQUEST)               :: requete
    TYPE(MPI_File)                  :: descripteur
    integer(kind= MPI_OFFSET_KIND)  :: position_fichier
    integer, dimension(nb_valeurs)  :: valeurs
    TYPE(MPI_Status)                :: statut
    logical                         :: termine

    call MPI_INIT()
    call MPI_COMM_RANK(MPI_COMM_WORLD, rang)

    call MPI_FILE_OPEN(MPI_COMM_WORLD, 'donnees.dat', MPI_MODE_RDONLY, MPI_INFO_NULL, descripteur)
    call MPI_TYPE_SIZE(MPI_INTEGER, nb_octets_entier)

    position_fichier = rang*nb_valeurs*nb_octets_entier

    call MPI_FILE_IREAD_AT(descripteur, position_fichier, valeurs, nb_valeurs, MPI_INTEGER, requete)

    do while (nb_iterations < 5000)
        nb_iterations = nb_iterations + 1
        ! Calculs recouvrant le temps demande par l'operation de lecture
        call MPI_TEST(requete, termine, statut)
        if (termine) exit
    end do
    if (.not. termine) then
        call MPI_WAIT(requete, statut)
    end if  

    print*, "Apres", nb_iterations, "iterations, le processus", rang, ":", valeurs
    call MPI_FILE_CLOSE(descripteur)
    call MPI_FINALIZE()

end program iread_at