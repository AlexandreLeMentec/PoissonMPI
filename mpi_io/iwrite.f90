program iwrite
    use mpi_f08
    implicit none

    integer, parameter              :: nb_valeurs = 10        
    TYPE(MPI_File)                  :: descripteur
    TYPE(MPI_REQUEST)               :: requete
    integer                         :: code, nb_iterations=0
    integer(kind= MPI_OFFSET_KIND)  :: deplacement
    integer, dimension(nb_valeurs)  :: valeurs,temp
    logical                         :: termine

    call MPI_INIT()
    !call MPI_COMM_RANK(MPI_COMM_WORLD, rang)

    call MPI_FILE_OPEN(MPI_COMM_WORLD, 'donnees.dat', MPI_MODE_WRONLY+ MPI_MODE_CREATE, &
                     MPI_INFO_NULL, descripteur)
    
    temp = valeurs
    call MPI_FILE_SEEK(descripteur, deplacement, MPI_SEEK_SET)
    call MPI_FILE_IWRITE(descripteur, temp, nb_valeurs, MPI_INTEGER, requete)

    do while (nb_iterations < 5000)
        nb_iterations = nb_iterations + 1
  
        call MPI_TEST(requete, termine, MPI_STATUS_IGNORE)
        if (termine) then
            temp = valeurs
            call MPI_FILE_SEEK(descripteur, deplacement, MPI_SEEK_SET)
            call MPI_FILE_IWRITE(descripteur, valeurs, nb_valeurs, MPI_INTEGER, requete)
        end if
    end do

    call MPI_WAIT(requete, MPI_STATUS_IGNORE)
    call MPI_FILE_CLOSE(descripteur)
    call MPI_FINALIZE()

end program iwrite