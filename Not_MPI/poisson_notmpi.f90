program poisson
! Initialisation
    use type_params

    implicit none

    real, allocatable, dimension(:,:) :: u, u_nouveau
    real, allocatable, dimension(:,:) :: u_exact

    integer :: it
    real :: diffnorm
    real :: t1, t2

    logical :: convergence

! Code

call initialisation(u, u_nouveau, u_exact)

it = 0
convergence = .false.

do while ((.not. convergence) and (it < maxit))
    it = it + 1
    u = u_nouveau


    call calcul(u, u_nouveau)
    diffnorm = maxval(abs(u_nouveau - u))

    convergence = (diffnorm < eps)

    print *, 'Iteration ', it, ' : ', diffnorm
    if (diffnorm < tol) then
        convergence = .true.
    else
        u = u_nouveau
    end if

    if mod(it, 100) == 0 then
        print *, 'Iteration ', it, ' : ', diffnorm
    end if
end do



end program poisson