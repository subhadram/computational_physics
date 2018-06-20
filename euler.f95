PROGRAM eulers
IMPLICIT NONE
REAL*8:: x,I,E,y,h,xi,yi,y1,y2,x1,x2,e1,e2,e3,ty2,ty3,ty4,f1,f2,y3
INTEGER:: j,n,k
h = 0.0024
n = 650
x = 0.01
y1 = 0.01
y2 = 0.01
y3 = 0.01
DO k = 1,n 
   y1 = e1(x,y1)
   y2 = e2(x,y2)
   y3 = e3(x,y3)
   write(100,*) x,y1,y2,y3,TAN(X)
    x = x + h
end do

End program 

function e1(x,y1)
IMPLICIT NONE
REAL*8 :: x,y1,h,e1
h = 0.0024
e1 = y1 + ((1.d0 + y1*y1)*h)
END FUNCTION e1

function e2(x,y2)
IMPLICIT NONE
REAL*8 :: x,y2,h,e2,ty2
h = 0.0024
ty2 = y2 + (h/2.d0)
e2 = y2 + ((1.d0 + ty2*ty2)*h)
END FUNCTION e2

function e3(x,y3)
IMPLICIT NONE
REAL*8 :: x,y3,h,e2,ty3,ty4,f1,f2,e3
h = 0.0024
ty3 = y3 + (h)
f1 = 1.d0 + y3*y3
f2 = 1.d0 + ty3*ty3
ty4 = (f1+f2)/2.d0
e3 = y3 + (ty4*h)
END FUNCTION e3
