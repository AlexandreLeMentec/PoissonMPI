program joker
    use mpi_f08
    implicit none
    integer, parameter :: m=4, etiquette =11
    integer, dimension(m,m) :: A 
    integer :: nb_procs, rang, i 
    integer :: k,l                               !affichage de A
    TYPE(MPI_Status) :: statut

    call MPI_Init()
    call MPI_Comm_size(MPI_COMM_WORLD, nb_procs)
    call MPI_Comm_rank(MPI_COMM_WORLD, rang)
    A(:,:) = 0

    if (rang == 0) then
        ! initialisation de la matrice A sur le processus 0
        A(:,:) = reshape((/ (i,i=1,m*m) /), (/m,m/))
        !print*, 'Matrice A avant envoi', A
        ! envoi de 3 éléments de la matrice A au procesus 1
        !Envoie les 3 premiers éléments de la première colonne de la matrice A au processus avec le rang 1.
            call MPI_Send(A(1,1), 3, MPI_INTEGER, 1, etiquette, MPI_COMM_WORLD)
        
    else
        ! on reçoit le message
        call MPI_Recv(A(1,2), 3, MPI_INTEGER, MPI_ANY_SOURCE, MPI_ANY_TAG, &
         MPI_COMM_WORLD, statut)
        print*, 'Moi, processus ', rang, ' j ai reçu 3 elements du processus', &
        statut%MPI_SOURCE, ' avec comme etiquette ', statut%MPI_TAG, &
        ' et voici les 3 elements : ', A(1:3,2)
    end if
    
   ! print*, 'Matrice A après reception', A
    

    call MPI_Finalize()
end program joker
