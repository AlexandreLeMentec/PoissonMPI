PROGRAM calcul_exact
  IMPLICIT NONE

  ! Kind pour double precision
  integer, parameter                    :: dp = selected_real_kind(15,307)
  !Solution  u_exact
  REAL(kind=dp)                         :: u_exact, u1, u2, tmp
  REAL(kind=dp), ALLOCATABLE, DIMENSION(:, :) :: u_calc
  !Compteurs
  INTEGER                               :: i, j, ntx, nty
  !Coordonnees globales suivant x et y
  REAL(kind=dp)                         :: x, y
  !Pas en x et en y
  REAL(kind=dp)                         :: hx, hy 
  REAL(kind=dp)                         :: erreur
  integer                               :: iocode

  !Lecture de ntx et nty
  OPEN(10, FILE='poisson.data', STATUS='OLD')
  READ(10, *) ntx
  READ(10, *) nty
  CLOSE(10)

  !Pas
  hx = 1./REAL(ntx+1)
  hy = 1./REAL(nty+1)

  ALLOCATE(u_calc(1:ntx, 1:nty))
  READ(11, 101,IOSTAT=iocode) u_calc

  if (iocode == 0) then
    erreur = 0
    do i=1, ntx
      do j=1, nty
        x = i*hx
        y = j*hy
        u_exact = x*y*(x-1)*(y-1)
        !tmp = abs((u_calc(i,j)-u_exact)/u_exact)
        tmp = abs(u_calc(i,j)-u_exact)
        if (tmp > erreur) then
          erreur = tmp
          u1 = u_exact
          u2 = u_calc(i,j)
        end if
      end do
    end do
    print *, "max ecart numerique ", erreur
    print *, "u_exact u_calc", u1, u2
    if (erreur < 1d-6) then
      print *, "BRAVO, Vous avez fini"
    else
      print *, "COURAGE, le fichier n'est pas bon"
    end if
  else
    print *, "COURAGE, le fichier n'est pas correctement écrit"
  end if


101 FORMAT  (E19.12)

END PROGRAM calcul_exact

