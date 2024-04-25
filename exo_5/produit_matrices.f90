    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! -*- Mode: F90 -*- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! produit_matrices.f90 --- TP5 : produit de matrices
    !
    ! Auteur          : Jalel Chergui (CNRS/IDRIS - France) <Jalel.Chergui@idris.fr>
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Remarques :
    ! ---------
    !
    !   * On veut réaliser le produit de matrices C = A * B en parallèle.
    !
    !   * On suppose que ces matrices sont carrées et que leur ordre N
    !     est divisible par le nombre Nprocs de processus.
    !
    !   * Le processus 0 initialise les matrices A et B qu'il distribue
    !     ensuite aux autres processus.
    !
    !   * La distribution de A se fait par bandes horizontales.
    !     La distribution de B se fait par bandes verticales.
    !
    !   * Chaque processus possède une bande des matrices A et B.
    !
    !   * Chaque processus calcule ainsi un bloc de la diagonale principale
    !     de C avec les éléments qu'il possède. Le calcul des blocs
    !     extra-diagonaux nécessite des communications avec les autres
    !     processus.
    !
    !   * En fait, l'opération se reduit ici à un produit de matrices par bloc.

    program produit_matrices

        use mpi_f08
        implicit none
    
        integer, parameter                  :: etiquette=1000
        integer                             :: rang, Nprocs, N, NL, k,i,j
        TYPE(MPI_Datatype)                  :: type_temp, type_tranche
        integer                             :: rang_suivant, rang_precedent, taille_type_reel
        TYPE(MPI_Status)                    :: statut
        real                                :: Emax
        real, allocatable, dimension(:,:)   :: A, B, C, CC, E
        real, allocatable, dimension(:,:)   :: AL, BL, CL, TEMP
        integer(kind=MPI_ADDRESS_KIND)      :: borne_inferieure=0, taille_deplacement_type_tranche
        character(len=256)                  :: arg
    
        ! Initialisation de MPI
        call MPI_INIT()
        call MPI_COMM_RANK(MPI_COMM_WORLD, rang)
        call MPI_COMM_SIZE(MPI_COMM_WORLD, Nprocs)
    
        if (rang == 0) then
        if (command_argument_count() < 1) then
            N=4
        else
            call get_command_argument(1,arg)
            read(arg, '(i6)') N
        end if
        end if
    
        ! Le processus 0 diffuse N à tous les autres processus
        call MPI_BCAST(N, 1, MPI_INTEGER, 0, MPI_COMM_WORLD) 
    
        ! Il faut que N soit divisible par Nprocs
        if ( mod(N, Nprocs) == 0 ) then
        if (rang == 0) print *, "Longueur matrice: ", N
        NL = N / Nprocs
        else
        print *, 'N=',N, ' n''est pas divisible par Nprocs=', Nprocs
        ! On arrete l'execution
        call MPI_ABORT(MPI_COMM_WORLD, 1)
        end if
    
        ! Le processus 0 initialise les matrices A et B
        if (rang == 0) then
        ! Allocation dynamique de mémoire, entre autres, des matrices A, B et C
        allocate( A(N,N), B(N,N), C(N,N), CC(N,N) )
    
        ! Initialisation de A et B
        call RANDOM_NUMBER(A)
        call RANDOM_NUMBER(B)
    
        ! Calcul monoprocesseur du produit matriciel A*B
        CC(:,:) = matmul(A(:,:), B(:,:))
        else
        ! Eviter problem avec option -check all
        allocate(A(0,0),B(0,0),C(0,0))
        end if
    
        ! Allocation dynamique de mémoire des divers tableaux locaux
        allocate( AL(NL,N), BL(N,NL), CL(N,NL), TEMP(NL,N) )
    
        call MPI_TYPE_SIZE(MPI_REAL, taille_type_reel)
    
        ! Construction du type qui correspond a 1 bloc de NL lignes et N colonnes
          call MPI_TYPE_VECTOR(N, NL, N, MPI_REAL, type_temp)
        taille_deplacement_type_tranche = taille_type_reel*NL
         call MPI_TYPE_CREATE_RESIZED(type_temp, borne_inferieure, taille_deplacement_type_tranche, &
                                 type_tranche)
         call MPI_TYPE_COMMIT(type_tranche)
  
         ! Le processus 0 distribue dans AL les tranches horizontales de la matrice A
          call MPI_SCATTER(A, 1, type_tranche, AL, N*NL, MPI_REAL, 0, MPI_COMM_WORLD)
  
        ! Le processus 0 distribue dans BL les tranches verticales de la matrice B
         call MPI_SCATTER(B, N*NL, MPI_REAL, BL, N*NL, MPI_REAL, 0, MPI_COMM_WORLD)
  
          ! Calcul des blocs diagonaux de la matrice resultante.
         CL(rang*NL+1:(rang+1)*NL,:) = matmul( AL(:,:), BL(:,:) )
        
        ! Premier algorithme (deux fois plus coûteux que le second)
     do k = 0, Nprocs-1
        ! Chaque processus ENVOIE sa tranche AL au processus k
        ! et REÇOIT dans TEMP la tranche AL du processus k
          if (rang /= k) then
       call MPI_SENDRECV(AL, NL*N, MPI_REAL, k, etiquette, &
                            TEMP, NL*N, MPI_REAL, k, etiquette, MPI_COMM_WORLD, statut)
            ! Chaque processus calcule les blocs situés au-dessus
            ! et en dessous du bloc de la diagonale principale
         CL(k*NL+1:(k+1)*NL,:)=matmul(TEMP(:,:),BL(:,:))
       end if
     end do
    
        ! Second algorithme
      !  rang_precedent = mod(Nprocs+rang-1,Nprocs)
    !    rang_suivant   = mod(rang+1,Nprocs)
    !    do k = 1, Nprocs-1
          ! Chaque processus ENVOIE sa tranche AL au processus précédent
          ! et REÇOIT la tranche AL du processus suivant (mais les contenus changent)
   !       call MPI_SENDRECV_REPLACE(AL, NL*N, MPI_REAL, rang_precedent, etiquette, &
     !                               rang_suivant, etiquette, MPI_COMM_WORLD, statut)
      
          ! Chaque processus calcule les blocs situés au-dessus
          ! et en dessous du bloc de la diagonale principale
     !     CL(mod(rang+k,Nprocs)*NL+1:(mod(rang+k,Nprocs)+1)*NL,:)=matmul(AL(:,:),BL(:,:))
     !   end do
      
        ! Le processus 0 collecte les tranches CL de tous les processus
        ! pour former la matrice résultante C
        call MPI_GATHER(CL, NL*N, MPI_REAL, C, NL*N, MPI_REAL, 0, MPI_COMM_WORLD)
      
        ! Les tableaux locaux sont désormais inutiles
        deallocate( AL, BL, CL, TEMP )
    

        ! Vérification des résultats
        if (rang == 0) then
        allocate( E(N,N) )
        E(:,:) = abs(C(:,:) - CC(:,:))
        Emax   = maxval( E(:,:) ) / N**2
        deallocate( A, B, C, CC, E )
    
        if ( Emax <= epsilon(1.0) ) then
            print'(/,40X,"Bravo !",/,  &
                & 20X,"Le produit matriciel A*B calcule en parallele",/, &
                & 20X,"est bien egal a celui calcule en monoprocesseur")'
        else
            print'(/,33X,"Resultat incorrect !",/, &
                & 20X,"Le produit matriciel A*B calcule en parallele",/, &
                & 20X,"est different de celui calcule en monoprocesseur")'
        end if
    
        end if
    
        ! Nettoyage MPI
        call MPI_TYPE_FREE(type_temp)
        call MPI_TYPE_FREE(type_tranche)
        call MPI_FINALIZE()
    
    end program produit_matrices
    
    