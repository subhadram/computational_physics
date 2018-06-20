PROGRAM mcc
IMPLICIT NONE
REAL*8 :: v1,v2,r,s,z(6),x(3),y(3),u(3),v(3),t(3),pi,vol,ai,sd,si,sdd,fn,l,AA,a,sm
Integer :: i,j,n
pi = 3.14159
vol = 10.d0**6
sm = 0.d0
sd = 0.d0
z = 0.d0
PRINT*, "enter a no. "
read*, n
do i = 1,n
	do j = 1,6
		CALL RANDOM_NUMBER(v1)
		CALL RANDOM_NUMBER(v2)
		s = (v1*v1) + (v2*v2)
		!pRINT*, 'A1'
		if((s.gt.1.d0)) s = s-1.d0
		!pRINT*, 'A3' 
		r = dsqrt(s)
		l = dsqrt(-2*(log(s)))
		z(j) = l*v1/r
		!ELSE 
		!CALL RANDOM_NUMBER(v1)
		!CALL RANDOM_NUMBER(v2)
		!s = (v1*v1) + (v2*v2)
		!pRINT*, 'A1'
	end do
	u(1) = z(1)
	v(1) = z(2)
	u(2) = z(3)
	v(2) = z(4)
	u(3) = z(5)
	v(3) = z(6)
	print*,u
	x = 10.d0*(u-0.5)
	y = 10.d0*(v-0.5)
	sm = sm + fn(x,y)
	sd = sd + (fn(x,y)*fn(x,y))
end do
pRINT*, 'A2'
a = sm/dfloat(n)
sdd = (sd- a*a)/dfloat(n)
AA = a*vol
!Print*,A
sm = 0.d0
sd = 0.d0
		!BS = BS +(AA**2)
		!AS = AS + AA
Write(16,*) n,AA,sdd
END PROGRAM mcc


FUNCTION fn(x,y)
IMPLICIT NONE 
REAL*8::x(3),y(3),fn,t(3),w
t = x-y
w = DOT_PRODUCT(t,t)
fn = exp (-(w/2.d0)*(3.14159**3))
END FUNCTION fn
	
		
