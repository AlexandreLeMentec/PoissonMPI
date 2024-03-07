program gatherv
    use mpi_f08
    implicit none
    integer, parameter  :: nb_valeurs =10
    integer             :: reste, nb_procs, rang, longueur_tranche, i
    real, dimension(nb_valeurs) :: donnees
    real, allocatable, dimension(:)    :: valeurs
    integer, allocatable, dimension(:) :: nb_elements_recus, deplacements  

    !nb_elemnts_recus : tableau spécifiant le nombre d'éléments reçus de chaque processus dans le processus racine
    !deplacements : tableau spécifiant les décalages des données reçues dans le tableau global de données

    call MPI_Init()
    call MPI_Comm_size(MPI_COMM_WORLD, nb_procs)
    call MPI_Comm_rank(MPI_COMM_WORLD, rang)


    ! Ajuster la longueur de la tranche pour chaque processus en fonction du reste

    longueur_tranche = nb_valeurs/nb_procs               
    reste = mod(nb_valeurs, nb_procs)                      ! le reste de la division de nb_valeurs par le nombre de processus, &
                             !nécessaire pour déterminer la longueur des tranches pour les processus supplémentaires
    
    
    ! Si le rang du processus est inférieur au reste, il y aura une tranche supplémentaire pour ce processus
    if (rang < reste) then
        longueur_tranche = longueur_tranche +1
    endif

    ! Chaque processus génère ses propres valeurs en fonction de son rang7
    ! avec longueur_tranche ajusté pour correspondre à la tranche spécifique de ce processus.
    
    ALLOCATE(valeurs(longueur_tranche))
    valeurs(:) = [( 1000.+(rang*(nb_valeurs/nb_procs)) + min(rang, reste)+i, &
        i=1, longueur_tranche)]

    print*, 'Moi, processus ', rang, 'envoie mon tableau de valeurs : ', &
        valeurs(1:longueur_tranche)

    if (rang == 2) then
        allocate(nb_elements_recus(nb_procs), deplacements(nb_procs))
        nb_elements_recus(1) = nb_valeurs / nb_procs
        if (reste > 0 )  then   
            nb_elements_recus(1) = nb_elements_recus(1) +1
             deplacements(1) =0
        endif   
        ! . S'il y a un reste, le premier processus (rang 0) recevra une tranche supplémentaire
        do i =2, nb_procs
            deplacements(i) = deplacements(i-1) + nb_elements_recus(i-1)
            nb_elements_recus(i) = nb_valeurs / nb_procs
            if (i-1 < reste) then
                 nb_elements_recus(i) = nb_elements_recus(i)+1
            endif 
        end do
    end if 

    call MPI_gatherv(valeurs, longueur_tranche, mpi_real, donnees, nb_elements_recus, &
        deplacements,  mpi_real, 2, MPI_COMM_WORLD)
    

    if (rang==2) print*,'Moi, processus 2 ai reçu', donnees(1: nb_valeurs)
    call MPI_Finalize()

end program gatherv




