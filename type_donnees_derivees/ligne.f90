program ligne
    use mpi_f08
    implicit none

    integer, parameter  :: nb_lignes =5, nb_colonnes =6
    integer, parameter          :: etiquette = 100
    real, dimension(nb_lignes, nb_colonnes)     :: a 
    TYPE(MPI_STATUS)            :: statut
    integer                     :: rang
    TYPE(MPI_Datatype)          :: type_ligne

    call MPI_INIT()
    call MPI_COMM_RANK(MPI_COMM_WORLD, rang)

    ! Initialisation de la matrice sur chaque processus
    a(:,:) = real(rang)

    ! Definition du type type_colonne
    call MPI_TYPE_VECTOR(nb_colonnes, 1, nb_lignes, MPI_REAL, type_ligne)
    ! Validition du type type_colonne
    call MPI_TYPE_COMMIT(type_ligne)

    ! Envoi 
    if (rang ==0) then
        call MPI_SEND( a(2,1), nb_colonnes, MPI_REAL,1, etiquette, MPI_COMM_WORLD)

    ! Reception dans l'avant-derniere ligne
    elseif (rang == 1) then
        call MPI_RECV(a(nb_lignes-1,1), 1, type_ligne,0,etiquette, &
            MPI_COMM_WORLD, statut)
    endif
    ! Libere le type
    call MPI_TYPE_FREE(type_ligne)

    call MPI_Finalize()

end program ligne

