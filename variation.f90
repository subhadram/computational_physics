program variation
IMPLICIT NONE
real*8,dimension(:,:),allocatable::H,E,v,p,x
real*8::a,V0,b
real*8,dimension(:),allocatable::k,w,work,eig
Integer::i,j,m
 CHARACTER :: N, U

Print*,"Enter N"
Read*, m

print*,"Enter a"
Read*,a
v0 = 1.d0
b= 2.d0
allocate(H(m,m))
allocate(E(m,m))
allocate(v(m,m))
allocate(p(m,m))
allocate(k(m))
allocate(w(m))
allocate(work(m))
allocate(eig(m))
allocate(x(m,m))
!Finding k,u,v values

do i=1,m
	k(i) = 2.d0*3.14*dfloat(i)/a
	do j = 1,m
		if (i==j) then
			v(i,j) = k(i)*k(i)/2.d0
			p(i,j) = V0/a*b
		else
			v(i,j) = 0.d0
			p(i,j) = V0/a*(sin(b*(k(i)-k(j))/2.d0))/((k(i) - k(j))/2.d0)
		end if
	end do
end do
print*,v
x = p + V
!diagonalizing the h array

do i = 1,m
	do j = 1,m
		if (i.lt.j) then
			h(i,j) = 0.d0
		else
			h(i,j) = x(i,j)
		end if
	end do
end do

CALL diasym(h,eig,m)



print*, x
END PROGRAM VARIATION

subroutine diasym(a,eig,n)
 implicit none

 integer n,l,inf
 real*8  a(n,n),eig(n),work(n*(3+n/2))

 l=n*(3+n/2)
 call dsyev('V','U',n,a,n,eig,work,l,inf)

 end subroutine diasym

