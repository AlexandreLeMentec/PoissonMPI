!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! -*- Mode: F90 -*- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! ping_pong_2.f90 --- TP2 : Communications point à point : ping-pong
!! 
!! Auteur          : Denis GIROU (CNRS/IDRIS - France) <Denis.Girou@idris.fr>
!! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program ping_pong_2
    USE MPI_F08
    implicit none
  
    TYPE(MPI_Status)                     :: statut
    integer, parameter                   :: nb_valeurs=1000,etiquette=99
    integer                              :: rang
    ! Kind pour double precision
    integer, parameter                   :: dp = selected_real_kind(15,307)
    real(kind=dp)                        :: temps_debut,temps_fin
    real(kind=dp), dimension(nb_valeurs) :: valeurs
    TYPE(MPI_Datatype)                   :: typedp
  
  !......................................................................
    call MPI_Init()
    call MPI_Comm_rank(MPI_COMM_WORLD, rang)
    call MPI_TYPE_CREATE_F90_REAL(15,307,typedp)

!......................................................................

    if (rang == 0) then
      call random_number(valeurs)
      temps_fin=MPI_WTIME()

      call MPI_Send(valeurs, nb_valeurs, MPI_REAL, 1, etiquette, MPI_COMM_WORLD)
      temps_debut=MPI_WTIME()
  
  !......................................................................
     call MPI_Recv(valeurs, nb_valeurs, MPI_REAL, 1, etiquette, MPI_COMM_WORLD, statut)
      print ('("Moi, processus 0, j''ai envoye et recu ",i5, &
          & " valeurs (derniere = ",f4.2,") du processus 1", &
          & " en ",f8.6," secondes.")'), &
            nb_valeurs,valeurs(nb_valeurs),temps_fin-temps_debut
  
  !......................................................................
    elseif (rang == 1) then
        call MPI_Send(valeurs, nb_valeurs, MPI_REAL, 0, etiquette, MPI_COMM_WORLD)
        call MPI_Recv(valeurs, nb_valeurs, MPI_REAL, 0, etiquette, MPI_COMM_WORLD, statut)

    end if
  call MPI_Finalize()

  !......................................................................
  
  end program ping_pong_2
  