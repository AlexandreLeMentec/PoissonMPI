!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! -*- Mode: F90 -*- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! ping_pong_1.f90 --- TP2 : Communications point à point :
!!                           envoi d'un message du processus 0 au processus 1
!! 
!! Auteur          : Denis GIROU (CNRS/IDRIS - France) <Denis.Girou@idris.fr>
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program ping_pong_1
    USE MPI_F08
    implicit none
  
    TYPE(MPI_Status)                     :: statut
    integer, parameter                   :: nb_valeurs=1000,etiquette=99
    integer                              :: rang
    ! Kind pour double precision
    integer, parameter                   :: dp = selected_real_kind(15,307)
    real(kind=dp), dimension(nb_valeurs) :: valeurs
    TYPE(MPI_Datatype)                   :: typedp

    
    call MPI_Init()
    call MPI_Comm_rank(MPI_COMM_WORLD, rang)

    call MPI_TYPE_CREATE_F90_REAL(15,307,typedp)

  !.....................................................................
  
  !......................................................................
     if (rang == 0) then
        call random_number(valeurs)
        call MPI_Send(valeurs, nb_valeurs, MPI_REAL, 1, etiquette, MPI_COMM_WORLD)
    else if (rang == 1) then
        call MPI_Recv(valeurs, nb_valeurs, MPI_REAL, 0, etiquette, MPI_COMM_WORLD, statut)

      print ('("Moi, processus 1, j''ai recu ",i4," valeurs (derniere = ", &
             & f4.2,") du processus 0.")'), nb_valeurs,valeurs(nb_valeurs)
  
    end if 
  !......................................................................
    call MPI_Finalize()
  
  end program ping_pong_1
  
  

  ! Correction :
! Ajouter   call MPI_TYPE_CREATE_F90_REAL(15,307,typedp) pour créer un type MPI pour les réels en double précision
!     call random_number(valeurs) après le if (rang == 0) pour initialiser le tableau valeurs
! mettre nb_valeurs au lieu 1000 pour la taille du tableau valeurs dans le mpi_send et mpi_recv
