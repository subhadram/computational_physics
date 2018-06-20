program variation
IMPLICIT NONE
real*8,dimension(:,:),allocatable::H,E,ke,pe,x
real*8::a,V0,b,ki,kj,dummy1,dummy2
real*8,dimension(:),allocatable::k,w,work,eig
Integer::i,j,m
 CHARACTER :: V, U

Print*,"Enter N"
Read*, m

print*,"Enter a"
Read*,a
v0 = 1.d0
b= 2.d0
allocate(H(m,m))
allocate(E(m,m))
allocate(ke(m,m))
allocate(pe(m,m))
allocate(k(m))
allocate(w(m))
allocate(work(m))
allocate(eig(m))
allocate(x(m,m))
!Finding k,u,v values

do i=1,m
	do j = 1,m		
		if (i==j) then
			ki = 2*3.14*dfloat(i)/a
			ke(i,j) = ki**2/2
		else
		ke(i,j) = 0
			
		end if
	end do
end do

do i =1,m
	do j = 1,m
			if (i==j) then
				pe(i,j) = V0/a*b
			else	
				ki = 2*3.14*dfloat(i)/a
				kj = 2*3.14*dfloat(j)/a
				dummy2 = (ki-kj)/2	
				p(i,j) = V0/a*(sin(b*dummy2))/(dummy2)
		end if
	end do
end do

!print*,v
x = pe + ke
!diagonalizing the h array

do i = 1,m
	do j = 1,m
		if (i.lt.j) then
			h(i,j) = 0
		else
			h(i,j) = x(i,j)
		end if
	end do
end do

!!CALL diasym(x,eig,m)
call dsyev('V','U',m,x,m,eig,work,l,inf)


print*, eig(1)
END PROGRAM VARIATION

subroutine diasym(a,eig,n)
 implicit none

 integer n,l,inf
 real*8  a(n,n),eig(n),work(n*(3+n/2))

 l=n*(3+n/2)
 call dsyev('V','U',n,a,n,eig,work,l,inf)

 end subroutine diasym

