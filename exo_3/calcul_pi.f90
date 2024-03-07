program calcul_pi 
    implicit none
    integer :: i, nbbloc =1000000
    real :: dx, integral, a,b,x

    a = 0
    b = 1

    dx = (b-a)/nbbloc
    integral = 0

    do i = 1, nbbloc
        x = a + (i-0.5)*dx
        integral = integral + dx * (4/(1 + (x)**2))
    end do

    ! Afficher le résultat
    print*, 'Le résultat de pi est : ', integral

end program calcul_pi