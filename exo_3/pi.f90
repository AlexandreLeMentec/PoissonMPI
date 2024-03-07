program pi

  use mpi
  implicit none
  
  integer            :: code,rang, nbprocs
  ! Kind pour double precision
  integer, parameter :: dp = selected_real_kind(15,307)
  ! Kind pour entier long
  integer, parameter :: li = selected_int_kind(15)
  integer(kind=li)   :: nbbloc,i,debut,fin
  real(kind=dp)      :: largeur,somme,global,x
  integer            :: typedp,typeli

  call MPI_INIT(code)
  call MPI_COMM_RANK(MPI_COMM_WORLD,rang,code)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,nbprocs,code)

  ! Nombre d'intervalles
  nbbloc = 3*1000*1000_li*100
  ! largeur des intervalles
  largeur = 1._dp / real(nbbloc,dp)

  somme = 0._dp

  ! Distribution repartie (Attention aux overflow)
  ! rang*nbbloc must be less than 9.10^18
  debut = (rang*nbbloc)/nbprocs+1
  fin = ((rang+1)*nbbloc)/nbprocs

  ! Idem
  ! rang*nbbloc must be less than  9.10^15
  !debut = ((1._dp*rang)*nbbloc)/nbprocs+1
  !fin = ((1._dp*(rang+1))*nbbloc)/nbprocs

  ! Le reste est distribue sur les premiers rangs
  !debut = rang*(nbbloc/nbprocs)+1+min(rang,mod(nbbloc,nbprocs))
  !fin = debut+(nbbloc-(rang+1))/nbprocs

  ! Idem
  !debut = rang*(nbbloc/nbprocs)+1+min(rang,mod(nbbloc,nbprocs))
  !fin = debut+(nbbloc/nbprocs)-1
  !if (rang < mod(nbbloc,nbprocs)) fin = fin+1

  ! Le reste est distribue sur les derniers rangs
  !debut = rang*(nbbloc/nbprocs)+1+max(mod(nbbloc,nbprocs)+rang-nbprocs,0)
  !fin = debut+(nbbloc+rang)/nbprocs-1

  print "(i2,a,i11,a,i11,a,i11)", rang, " debut: ", debut, " fin: ", fin, " delta: ", fin-debut+1

  do i=debut, fin
    ! Point au milieu de l'intervalle
    x = largeur*(i-0.5_dp)
    ! Calcul de l'aire
    somme = somme + largeur*(4._dp / (1._dp + x**2))
  end do

  call mpi_Type_create_f90_real(15, 307, typedp, code)
  call MPI_Reduce(somme, global, 1, typedp, MPI_SUM, 0, MPI_COMM_WORLD, code)

  if (rang ==0) then
    print *, "Pi =", global
    print *, "Ecart =", global-4._dp*atan(1._dp)
  end if

  call MPI_Type_create_f90_integer(15, typeli, code)
  call MPI_Reduce(fin-debut+1, i, 1, typeli, MPI_SUM, 0, MPI_COMM_WORLD, code)
  !call MPI_REDUCE(fin-debut+1,i, 1, MPI_INTEGER8, MPI_SUM, 0, MPI_COMM_WORLD,code)
  if (rang ==0) print *, "Nb =", i

  call MPI_FINALIZE(code)
end program
