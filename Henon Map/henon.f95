program henon
    integer :: n,j
    doubleprecision :: x_ii,x_i,x_ij,y_ii,y_i,lamda ! el _ii=i+1 , _i=i , _ij=i-1
    real :: a,b,c
    c=0.1
    n=1000
    a=0.2
    b=-0.99990
    x_i=0.0
    y_i=0.0
    x_ij=y_i
    open(15,file='henomap.txt',status='unknown')
    open(13,file='lyapunovexp.txt',status='unknown')

    write(*,*) x_i,y_i
    write(15,*) x_i,y_i

    ! LOS do while() son para poder graficar el conjunto para diferentes valores de
    ! los parametros a,b
    !

    !do while(b<=4.0)

        !do while(a<=1.4)
            !write(*,*) 'para a=',a
            !write(15,*) 'para a=',a

            do j=1,n
                    if(j<=n-1) then
                        lamda=log(1+a*x_i**(2)+b*x_ij)
                        write(*,*) j,lamda
                        write(13,*)j,lamda
                    end if
                    x_ii=1-a*x_i**(2)+y_i
                    y_ii=b*x_i
                    x_i=x_ii
                    y_i=y_ii
                    x_ij=y_i
                    write(*,*)x_i,y_i
                    write(15,*) x_i,y_i


            end do
            !a=a+c
        !end do
        !b=b+c
    !end do

    !exponente de lyapunov
    ! x_i+1=f(x_i)=1-ax_i^2+bx_i-1
    print*,'exponentee lyapunov:',lamda


    close(15)
    close(13)
end program henon
