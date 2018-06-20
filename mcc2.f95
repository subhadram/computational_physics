PROGRAM mcc2
IMPLICIT NONE
REAL*8 :: v1,v2,r,s,z(6),x(3),y(3),u(3),v(3),t(3),pi,vol,ai,sd,si,sdd,fn,l,AA,a,sm,rm,av,bv,cv,bs,as,sad
Integer :: i,j,n,nm,m,p
pi = 3.14159
vol = 1.d0**6
sm = 0.d0
sd = 0.d0
z = 0.d0
PRINT*, "enter a no. "
read*, nm
Read* ,p
do n = 10,nm,10
	do m = 1 , n
	!call random_seed(size = z)
		do i = 1,p
			do j = 1,6
				z(j) = RM()
			end do
		X(1) = z(1)
		Y(1) = z(2)
		X(2) = z(3)
		Y(2) = z(4)
		X(3) = z(5)
		Y(3) = z(6)
		sm = sm + fn(x,y)
		sd = sd + (fn(x,y)*fn(x,y))
		end do
		a = sm/dfloat(p)
		sad = sd/float(p)
		sdd = sqrt((sad- a*a)/dfloat(p))
		AA = a*vol
		write(186,*) m,AA,sdd
		sm = 0.d0
		sd = 0.d0
		BS = BS +(AA**2)
		AS = AS + AA
	END DO
	AV = AS/dfloat(n)
	BV = (BS/dfloat(n))
	CV = sqrt((BV - (AV*AV))/dfloat(n))
        Write(176,*) n,AV,CV
        AS = 0.0
	BS = 0.0
	CV = 0.0
end do
END PROGRAM mcc2
		

FUNCTION RM()
IMPLICIT NONE
    REAL*8 :: s, v1,v2,u,v,rm
    DO
      CALL RANDOM_NUMBER(v1)
      CALL RANDOM_NUMBER(v2)
      u = 2.0*(v1 - 0.50)
      v = 2.0*(v2 - 0.50)
      s = u**2 + v**2
      IF (s<1.0) EXIT
    END DO
    RM=u*SQRT(-LOG(s)/s)
  END FUNCTION RM



FUNCTION fn(x,y)
IMPLICIT NONE 
REAL*8::x(3),y(3),fn,t(3),w
t = x-y
w = DOT_PRODUCT(t,t)
fn = (3.14159**3)*exp (-(w/2.d0))
END FUNCTION fn
	
		
