program subarray
    use mpi_f08
    implicit none

    integer, parameter           :: nb_lignes = 4, nb_colonnes = 3, &
                                    etiquette = 1000, nb_dims = 2
    integer                      :: code, rang, i
    type(MPI_Datatype)           :: type_sous_tab
    integer, dimension(nb_lignes, nb_colonnes) :: tab
    integer, dimension(nb_lignes) :: profil_tab, profil_sous_tab, coord_debut
    TYPE(MPI_Status)              :: statut

    call MPI_Init()
    call MPI_Comm_rank(MPI_COMM_WORLD, rang)

    ! Initialisation du tableau sur chaque processus
    tab(:,:) = reshape ((/ (sign(i,-rang), i=1, nb_lignes*nb_colonnes)/), &
                        (/ nb_lignes, nb_colonnes/))

    ! Profil du tableau tab a partir duquel on va extraire un sous-tableau
    profil_tab(:) = shape(tab)

    ! La fonction F95 shape donne le profil du tableau passe en argument.
    ! ATTENTION, si le tableau concerne n’a pas ete alloue sur tous les processus,
    ! il faut mettre explicitement le profil du tableau pour qu’il soit connu
    ! sur tous les processus, soit profil_tab(:) = (/ nb_lignes,nb_colonnes /)

    ! Profil du sous-tableau
    profil_sous_tab(:) = (/ 2,2 /)
    
    ! Coordonnees du debut du sous-tableau
    ! pour le processus 0 on part de l'élément tab(2,1)
    ! pour le processus 1 on part de l'élément tab(3,2)
    coord_debut(:) = (/ rang+1, rang/)

    ! Creation du type de sous-tableau
    call MPI_Type_create_subarray(nb_dims, profil_tab, profil_sous_tab, &
                                  coord_debut, MPI_ORDER_FORTRAN, MPI_INTEGER, type_sous_tab)
    call MPI_Type_commit(type_sous_tab)

    ! Permutation des elements du sous-tableau
    call MPI_Sendrecv_replace(tab, 1, type_sous_tab, mod(rang+1,2), etiquette, &
                       mod(rang+1,2), etiquette, MPI_COMM_WORLD, statut)

    ! Affichage du tableau
    do i = 1, nb_lignes
        print *, tab(i,:)
    end do

    call MPI_Type_free(type_sous_tab)
    call MPI_Finalize()

end program subarray
