PROGRAM mc2
IMPLICIT NONE
REAL*8 :: x(3),y(3),sm,a,s,t(3),u,v,fn,c(3),d(3),vol,ai,sd,si,sdd,AS,AA,AV,BS,BV,CV
!REAL*8,DIMENSION(:),ALLOCATABLE::A
!REAL*8,DIMENSION(:),ALLOCATABLE::AA
INTEGER :: nm,n,m,i,j,p,z
vol = 10.0**6
sm = 0.d0
sd = 0.d0
AS = 0.d0
BS = 0.d0
z = 1
PRINT*, "Enter no. of random no. per average"
READ*,nm
!PRINT*, "Enter no. of no. of averages"
!READ*,m
!f = (nm-10)/10
!ALLOCATE(AA(f))
!aLLOCATE(AA(m))

do n = 100,nm,100
	call random_seed(size = z)
	do m = 1 , n
		!do i = 1,100000
		
		CALL RANDOM_NUMBER(c)
		x = 10.d0*(c-0.5)
		CALL RANDOM_NUMBER(d)
		y = 10.d0*(d-0.5)
		sm = sm + fn(x,y)
		sd = sd + (fn(x,y)*fn(x,y))
	end do
	a = sm/dfloat(n)
	sdd = (sd- a*a)/dfloat(n)
	AA = a*vol
	!Print*,A
       	sm = 0.d0
	sd = 0.d0
		!BS = BS +(AA**2)
		!AS = AS + AA
        Write(66,*) n,AA,sdd
end do
!!DEALLOCATE(AA)
END PROGRAM mc2


FUNCTION fn(x,y)
IMPLICIT NONE 
REAL*8::x(3),y(3),fn,s,t(3),u,v
s = DOT_PRODUCT(x,x)
t = x-y
v = DOT_PRODUCT(t,t)
u = DOT_PRODUCT(y,y)
fn = exp (-s-u-(v/2.d0))
END FUNCTION fn

