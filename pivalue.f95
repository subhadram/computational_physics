PROGRAM pivalue
IMPLICIT NONE
REAL*8 :: x,y,p,t,h,r,s
INTEGER :: n,i,j,ccnt
 ccnt = 0
h = 1.0
PRINT*, "Enter no.of points "
READ*, n 
DO i = 1,n 
	CALL RANDOM_NUMBER(x)
	CALL RANDOM_NUMBER(y)
	s = 2*(x-0.5)
	r = 2*(y-0.5)
	t = (s*s)+(r*r)
	if (t.le.h) then
		ccnt = ccnt + 1
	end if
END DO
p = 4.0 *(ccnt / real(n)) 
PRINT*, p
END PROGRAM pivalue 

