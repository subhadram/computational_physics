PROGRAM integrate
IMPLICIT NONE
REAL:: xi,I,E,y
INTEGER:: j,n,k
PRINT*,"Type no. of bins."
READ*,n
OPEN(unit = 1, file = 'error.dat')
DO k = 1,n 
CALL inte(k,I)
E = (3.1415926 - I)
write(1,12) k, E, I
END DO
CLOSE(1)
12 format(I5,1X,2(E18.9,1X))
END PROGRAM Integrate

SUBROUTINE inte(n,I)
IMPLICIT NONE
REAL::a = 0.0,b = 1.0,f,fn,I,h
INTEGER::j,n
h = (1.0-0.0)/real(n)
DO j =1,n-1
f = f + fn(real(h*j))
END DO	
I = ((h/2.0)*(fn(a) + fn(b))) + h*f
END SUBROUTINE inte

FUNCTION fn(y)
IMPLICIT NONE
REAL::fn,y
fn = 4.0 /(1.0+(y*y))
END FUNCTION fn


