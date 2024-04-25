        program demi_ligne
            use mpi_f08
            implicit none
            integer, parameter      :: nb_lignes = 5, nb_colonnes =6, &
                                    taille_demi_ligne = nb_colonnes/2, etiquette = 1000
            integer, dimension(nb_lignes, nb_colonnes)  :: a
            integer                 :: rang, i, taille_integer
            TYPE(mpi_datatype)      :: type_demi_ligne1, type_demi_ligne2
            integer(kind = MPI_ADDRESS_KIND)    :: borne_inf1, etendue1, borne_inf2, etendue2
            TYPE(mpi_status)        :: statut

            call MPI_INIT()
            call MPI_Comm_rank(MPI_COMM_WORLD,rang)

            ! Initialisation de la matrice A sur chaque processus
            a(:,:) = reshape ( (/ (sign(i,-rang), i=1, nb_lignes*nb_colonnes)/) ,& 
                (/ nb_lignes, nb_colonnes/))
            
            ! Construction du type derive type_demi_ligne1
            call MPI_TYPE_VECTOR(taille_demi_ligne, 1, nb_lignes, MPI_INTEGER, type_demi_ligne1)

            ! Connaitre la taille du type de base MPI_INTEGER
            call MPI_TYPE_SIZE(MPI_INTEGER, taille_integer)

            ! Informations sur le type derivee type_demi_ligne1
            call MPI_TYPE_GET_EXTENT(type_demi_ligne1, borne_inf1, etendue1)
            if (rang == 0) print*, "type_demi_ligne1 : borne_inf =", borne_inf1, ",etendue = ", etendue1

            ! Construction du type derive type_demi_ligne2
            borne_inf2 =0
            etendue2 = taille_integer
            call MPI_TYPE_CREATE_RESIZED(type_demi_ligne1, borne_inf2, etendue2, &
                            type_demi_ligne2)
            ! Informations sur le type derive type_demi_ligne2
            call MPI_TYPE_GET_EXTENT(type_demi_ligne2, borne_inf2, etendue2)
            if (rang == 0) print*, "type_demi_ligne2 : borne_inf =", borne_inf2, ",etendue = ", etendue2

            ! Validation du type type_demi_ligne2
            call MPI_TYPE_COMMIT(type_demi_ligne2)

            if (rang == 0) then
                ! Envoi de la matrice A au processus 1 avec le type type_demi_ligne2
                call MPI_SEND(A(1,1), 2, type_demi_ligne2, 1, etiquette, &
                                MPI_COMM_WORLD)
            else
                ! Reception pour le processus 1 dans la matrice A
                call MPI_RECV(A(1,nb_colonnes-1), 6, MPI_INTEGER, 0, etiquette, & 
                MPI_COMM_WORLD, statut)
                print*, 'Matrice A sur le processus 1'
                do i=1, nb_lignes
                    print*, A(i,:)
                end do
            end if

            call MPI_Finalize
end program demi_ligne
        
