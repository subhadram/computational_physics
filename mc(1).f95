PROGRAM mc
IMPLICIT NONE
REAL*8 x(3),y(3),I,fn,sm,a
!REAL*8,DIMENSION(:),ALLOCATABLE::A
!REAL*8,DIMENSION(:),ALLOCATABLE::AA
INTEGER n,m,i,j
sm = 0.d0
PRINT*, "Enter no. of random no. per average"
READ*,n
PRINT*, "Enter no. of no. of averages"
READ*,m
ALLOCATE(A(n)
ALLOCATE(AA(m))
do i = 1,n
	CALL RANDOM_NUMBER(x)
	CALL RANDOM_NUMBER(y)
	sm = sm + fn(x,y)
end do
a = sm/real(n)

FUNCTION fn(x,y)
IMPLICIT NONE
REAL::fn,x,y
s = DOT_PRODUCT(x,x)
t = x-y
v = DOT_PRODUCT(t,t)
u = DOT_PRODUCT(y,y)
fn = exp (-s-u-(v/2.d0))
END FUNCTION fn

END PROGRAM mc



