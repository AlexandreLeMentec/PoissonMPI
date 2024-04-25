SUBROUTINE debut_communication(u)

    ! Envoi au voisin N et réception du voisin S
    CALL MPI_IRECV(u(,),1, type_ligne, voisin(S), & 
    etiquette, comm2d, requete(1))
    CALL MPI_ISEND( u(,), 1, type_ligne, voisin(N), & 
    etiquette, comm2d, requete(2))

    ! Envoi au voisin S et réception du voisin N
    CALL MPI_IRECV(u(,),1, type_ligne, voisin(N), & 
    etiquette, comm2d, requete(3))
    CALL MPI_ISEND( u(,), 1, type_ligne, voisin(S), & 
    etiquette, comm2d, requete(4))
    
     ! Envoi au voisin W et réception du voisin E
    CALL MPI_IRECV(u(,),1, type_ligne, voisin(E), & 
    etiquette, comm2d, requete(5))
    CALL MPI_ISEND( u(,), 1, type_ligne, voisin(W), & 
    etiquette, comm2d, requete(6))

     ! Envoi au voisin E et réception du voisin W
    CALL MPI_IRECV(u(,),1, type_ligne, voisin(W), & 
    etiquette, comm2d, requete(7))
    CALL MPI_ISEND( u(,), 1, type_ligne, voisin(R), & 
    etiquette, comm2d, requete(8))

END SUBROUTINE debut_communication

SUBROUTINE fin_communication(u)
    CALL MPI_WAITALL(2*NB_VOISINS, requete, tab_statut)
    if (.not.(MPI_ASYNC_PROTECTS_NONBLOCKING) CALL mpi_f_sync_reg(u))
END SUBROUTINE fin_communication

DO WHILE ((.NOT. convergence) .AND. (it < it_max))
    it = it + 1 
    u(sx:ex; sy:ey) = u_nouveau(sx:ex, sy:ey)

    ! Echange des points aux interfaces pour u à l'itération n
    call debut_communication(u)

    ! Calcul de u à l'itération n+1
    call calcul(u,u_nouveau, sx+1, ex-1, sy+1, ey-1)

    call fin_communication(u)

    ! nord
    call calcul(u, u_nouveau, sx, sx, sy,ey)

    ! sud
    call calcul(u, u_nouveau, ex, ex, sy, ey)

    ! ouest
    call calcul(u, u_nouveau, sx, ex, sy, sy)

    ! est
    call calcul(u, u_nouveau, sx, ex, ey ,ey )

    ! Calcul de l'erreur global
    diffnorm = erreur_globale(u, u_nouveau)
    ! Arrêt du programme si on a atteint la précision machine obtenu
    ! par la fonction f90 epsilon
    convergence = (diffnorm < eps)

END DO
