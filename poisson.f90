!**********************************************************************
!   poisson.f90 - Resolution de l'equation de Poisson par une methode aux differences finies
!   en utilisant un solveur Jacobi sur le domaine [0,1]x[0,1].
!
!   Delta u = f(x,y)= 2*(x*x-x+y*y -y)
!   u sur les bords vaut 0
!   La solution exacte est u = x*y*(x-1)*(y-1)
!
!   La valeur de u est donnee par la formule:
!    coef(1) = (0.5*hx*hx*hy*hy)/(hx*hx+hy*hy)
!    coef(2) = 1./(hx*hx)
!    coef(3) = 1./(hy*hy)
!
!    u(i,j)(n+1)= coef(1) * ( coef(2)*(u(i+1,j)+u(i-1,j)) &
!               + coef(3)*(u(i,j+1)+u(i,j-1)) - f(i,j))
!
!   Dans cette version, on se donne le nombre de points interieurs
!   total suivant x (ntx) et le nombre de points interieurs total
!   suivant y (nty).
! 
!   hx represente le pas suivant x, hy le pas suivant y.
!    hx = 1./(ntx+1)
!    hy = 1./(nty+1)
!
!   Pour chaque processus :
!   1) decomposer le domaine
!   2) connaitre ses 4 voisins
!   3) echanger les points aux interfaces
!   4) calculer
!   5) recomposer la matrice u dans un fichier de sortie donnees.dat
!
!   Auteur          : Isabelle DUPAYS (CNRS/IDRIS - France)
!                     <Isabelle.Dupays@idris.fr>
!   Cree le         : Novembre 2010
!****************************************************************************

PROGRAM poisson
  USE TYPE_PARAMS
  USE PARALLEL
  USE CALCUL_POISSON

  IMPLICIT NONE

  !Solution u et u_nouveau a l'iteration n et n+1
  REAL(kind=dp), ALLOCATABLE, DIMENSION(:, :) :: u, u_nouveau
  !Solution exacte
  REAL(kind=dp), ALLOCATABLE, DIMENSION(:, :) :: u_exact
  !Nombre iterations en temps
  INTEGER                                  :: it
  !Convergence
  REAL(kind=dp)                             :: diffnorm
  !Mesure du temps
  REAL(kind=dp)                             :: t1, t2
  !Test de convergence
  LOGICAL                                  :: convergence

  !****************************************************************************
  !Initialisation de MPI
  CALL initialisation_mpi

  !Creation de la topologie cartesienne 2D
  CALL creation_topologie

  !Determinination des indices de chaque sous domaine
  CALL domaine

  !Initialisation du second membre, u, u_nouveau et u_exact
  CALL initialisation(u, u_nouveau, u_exact)

  !Recherche de ses 4 voisins pour chaque processus
  CALL voisinage

  !Creation des types derives type_ligne et type_colonne
  CALL type_derive

  !Schema iteratif en temps
  it = 0
  convergence = .FALSE.

  !Mesure du temps passe dans la boucle en temps (en secondes)
  t1 = MPI_WTIME()

  DO WHILE ((.NOT. convergence) .AND. (it < it_max))

    it = it +1

    u(sx:ex, sy:ey) = u_nouveau(sx:ex, sy:ey)

    !Echange des points aux interfaces pour u a l'iteration n
    CALL communication(u)

    !Calcul de u a l'iteration n+1
    CALL calcul(u,  u_nouveau)

    !Calcul de l'erreur globale 
    diffnorm = erreur_globale(u, u_nouveau)

    !Arret du programme si on a atteint la precision machine obtenu
    !par la fonction F90 EPSILON
    convergence = (diffnorm < eps)

    !Affichage pour le processus 0 de la difference
    IF ((rang == 0) .AND. (MOD(it, 100) == 0)) THEN
       PRINT *, 'Iteration ', it, ' erreur_globale = ', diffnorm
    END IF

  END DO

  !Mesure du temps a la sortie de la boucle
  t2 = MPI_WTIME()

  IF (rang == 0) THEN
    !Affichage du temps de convergence par le processus 0
    PRINT *, 'Convergence apres ', it, ' iterations en  ', t2 - t1, ' secs '

    !Comparaison de la solution calculee et de la solution exacte
    !sur le processus 0
    CALL sortie_resultats(u, u_exact)
  END IF
 
  !Ecriture des resultats u(sx:ex, sy:ey) 
  !pour chaque processus
  CALL ecrire_mpi(u)

  !Desactivation de MPI
  CALL finalisation_mpi

END PROGRAM poisson
