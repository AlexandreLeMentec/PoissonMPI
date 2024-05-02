SUBROUTINE initialisation(u, u_nouveau, u_exact)  
    !**************************
    !Initialisation des valeurs
    !**************************    
    !Solution u et u_nouveau a l'iteration n et n+1
    REAL, ALLOCATABLE, DIMENSION(:, :), INTENT(out) :: u, u_nouveau
    !Solution exacte
    REAL, ALLOCATABLE, DIMENSION(:, :), INTENT(out) :: u_exact

    !Compteurs
    INTEGER                               :: i, j
    !Coordonnees globales suivant x et y
    REAL                                  :: x, y
    !Pas en x et en y
    REAL                                  :: hx, hy 

    !Allocation dynamique des tableaux u, u_nouveau, u_exact, f
    ALLOCATE(u(sx-1:ex+1, sy-1:ey+1), &
             u_nouveau(sx-1:ex+1, sy-1:ey+1))
    ALLOCATE(f(sx-1:ex+1, sy-1:ey+1), &
             u_exact(sx-1:ex+1, sy-1:ey+1))

    !Initialisation des matrices
    u(sx-1:ex+1, sy-1:ey+1)         = 0.
    u_nouveau(sx-1:ex+1, sy-1:ey+1) = 0.
    f(sx-1:ex+1, sy-1:ey+1)         = 0.
    u_exact(sx-1:ex+1, sy-1:ey+1)   = 0.

    !Pas
    hx = 1./REAL(ntx+1)
    hy = 1./REAL(nty+1)

    !Calcul des coefficients
    coef(1) = (0.5*hx*hx*hy*hy)/(hx*hx+hy*hy)
    coef(2) = 1./(hx*hx)
    coef(3) = 1./(hy*hy)

    !Initialisation du second membre et calcul de la solution exacte
    DO i=sx, ex
      DO j=sy, ey
        x = i*hx
        y = j*hy
        f(i, j) = 2*(x*x-x+y*y-y)
        u_exact(i, j) = x*y*(x-1)*(y-1)
      END DO
    END DO

END SUBROUTINE initialisation


SUBROUTINE calcul(u, u_nouveau, x1, x2, y1, y2)
    !*****************
    ! Calcul de la solution u_nouveau a l'iteration n+1
    !*****************
    !Solution u  a l'iteration n 
    REAL, ALLOCATABLE, DIMENSION(:, :), INTENT(in)   :: u

    !Solution u_nouveau a l'iteration n+1
    REAL, ALLOCATABLE, DIMENSION(:, :), INTENT(inout):: u_nouveau

    ! Borne du calcul
    INTEGER, INTENT(in) :: x1, x2, y1, y2

    !Compteur
    INTEGER             :: i, j

    DO j=y1, y2
      DO i=x1, x2
        u_nouveau(i, j) = coef(1) * (coef(2)*(u(i+1, j)+u(i-1, j)) &
                        + coef(3)*(u(i, j+1)+u(i, j-1)) - f(i, j))
      END DO
    END DO

END SUBROUTINE calcul


SUBROUTINE sortie_resultats(u, u_exact)  
    !**************************
    !Affichage 
    !**************************    
    !Solution u a l'iteration n 
    REAL, ALLOCATABLE, DIMENSION(:, :), INTENT(in) :: u
    !Solution exacte
    REAL, ALLOCATABLE, DIMENSION(:, :), INTENT(in) :: u_exact

    INTEGER :: j
    
    PRINT *, 'Solution exacte u_exact ', 'Solution calculee u'
    DO j=1, nty
      PRINT 10, u_exact(1, j), u(1, j)
10    FORMAT('u_exact=  ', E12.5, ' u =  ', E12.5)
    END DO


END SUBROUTINE  sortie_resultats