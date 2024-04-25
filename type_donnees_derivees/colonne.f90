program colonne
    use mpi_f08
    implicit none

    integer, parameter          :: nb_lignes =5, nb_colonnes=6
    integer, parameter          :: etiquette = 100
    real, dimension(nb_lignes, nb_colonnes)     :: a 
    TYPE(MPI_STATUS)            :: statut
    integer                     :: rang
    TYPE(MPI_Datatype)          :: type_colonne

    call MPI_INIT()
    call MPI_COMM_RANK(MPI_COMM_WORLD, rang)

    ! Initialisation de la matrice sur chaque processus
    a(:,:) = real(rang)

    ! Definition du type type_colonne
    call MPI_TYPE_CONTIGUOUS(nb_lignes, MPI_REAL, type_colonne)

    ! Validition du type type_colonne
    call MPI_TYPE_COMMIT(type_colonne)

    ! Envoi de la première colonne
    if (rang ==0) then
        call MPI_SEND( a(1,1), 1, type_colonne, 1, etiquette, MPI_COMM_WORLD)

    ! Reception dans la dernière colonne
    elseif (rang == 1) then
        call MPI_RECV(a(1,nb_colonnes), nb_lignes, MPI_REAL,0,etiquette, &
            MPI_COMM_WORLD, statut)
    endif
    ! Libere le type
    call MPI_TYPE_FREE(type_colonne)

    call MPI_Finalize()

    !print*, a

    print*, "Le processus 0 a envoyé : ", a(1,1)
    print*, "Dans la dernière colonne, le rang 1 reçoit du rang 0 : ", a(1,nb_colonnes)
end program colonne

