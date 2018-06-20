program variation 
implicit none 
integer::N , m, i,j
real:: ki, hcross ,a , b , V0 ,dummy1 , dummy2,kj,kinetic , potential 
double precision,allocatable:: H(:,:) , PE(:,:) , KE(:,:) ,W(:), U(:,:)
print*, 'Enter the number of basis functions'
read*, N 
allocate(H(N,N) ,PE(N,N) , KE(N,N) , W(N),U(N,N))
print*, 'enter value for a'
read*, a
!!!!!!!!!!!!!!!!!!!!! initial conditions!!!!!!!!!!!!!!!!!!!!!!!!!
V0 = 1
b=2
m=1
hcross= 1      !plack constant
dummy1 = V0/a

!!!!!!!!!!!!!!! finiding the kinetic energy matrix!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
do i = 1,N
    do j = 1,N 
        if (i==j) then 
            ki = 2*3.14*i/a
            KE(i,j) = ((hcross*ki)**2)/(2*m)
        else 
            KE(i,j) =0
        endif
     enddo
enddo

!!!!!!!!!!!!!! finding the potential energy matrix!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!! Potential energy matrix = V0/a sin(b(ki-kj)/2)/(ki-kj)/2!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1

do i = 1,N
    do j=1,N
        if (i==j) then 
            PE(i,j) = V0*b/a
        else
            ki = 2*3.14*i/a
            kj = 2*3.14*j/a
            dummy2 = (ki-kj)/2
            PE(i,j) = (sin(b*dummy2))/dummy2
        endif
    enddo
enddo


!!!!!!!!!!!!!!!!! writing hamiltonian !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

do i = 1,N
    do j = 1,N 
        kinetic = KE(i,j)
        potential = PE(i,j)
        H(i,j) = kinetic+potential 
    enddo
enddo

do i = 1,N
    do j = 1,N 
        if (i<j) then 
            U(i,j) = 0
        else 
            U(i,j) = H(i,j)
        endif
    enddo
enddo

!!!!!!!!!!!!!!!!! Diagonalization!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

CALL DIASYM(H,W,N)
print*, w(1)
end program variation 








subroutine diasym(a,eig,n)
implicit none

 integer:: n,l,inf
 real*8 ::  a(n,n),eig(n),work(n*(3+n/2))

 l=n*(3+n/2)
 call dsyev('V','U',n,a,n,eig,work,l,inf)
END SUBROUTINE diasym
