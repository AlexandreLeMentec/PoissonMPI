program bloc
    use mpi_f08
    implicit none
    
    integer, parameter      :: nb_lignes =5, nb_colonnes = 6
    integer, parameter      :: etiquette = 100
    integer, parameter      :: nb_lignes_bloc =2, nb_colonnes_bloc =3
    real, dimension(nb_lignes, nb_colonnes)     :: a 
    TYPE(MPI_Status)           :: statut
    integer                 :: rang
    TYPE(MPI_Datatype)      :: type_bloc

    call MPI_INIT()
    call MPI_Comm_rank(MPI_COMM_WORLD, rang)

    ! Initialisation de la matrice sur chaque processus
    a(:,:) = real(rang)

    ! Creation du type type_bloc
    call MPI_TYPE_VECTOR(nb_colonnes_bloc, nb_lignes_bloc, nb_lignes, &
        MPI_REAL, type_bloc)

    ! Validation du type type_bloc
    call MPI_TYPE_COOMIT(type_bloc)

    ! Envoi d'un bloc
    if (rang ==0) then
        call MPI_SEND(a(1,1),1, type_bloc, 1, etiquette, MPI_COMM_WORLD)

    ! Reception du bloc
    elseif (rang ==1) then
        call MPI_RECV(a(nb_lignes-1, nb_colonnes-2), 1, type_bloc,0, etiquette, &
        MPI_COMM_WORLD, statut)
    end if

    ! Liberation du type type_bloc
    call MPI_TYPE_FREE(type_bloc)

    call FINALIZE()

end program bloc