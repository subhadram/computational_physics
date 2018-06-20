PROGRAM RK
IMPLICIT NONE
REAL*8:: x,y,h,f0,f1,f2,f3,rk4,y1,y2,y3,y4,y5,t,k,rk3,f0v,f2v,f1v,f3v,x2,x3,x4,x1
INTEGER:: n,m
k = 2.d0
t = 0.d0
h = 0.4
n = 10000
x = 0.01
y = 0.01
DO m = 1,n 
   f0 = -k*x 
x1 = x+(h*f0/2.d0)
f1 = -k*x1
x2 = x + (h*f1/2.d0) 
f2 = -k*x2
x3 = x + (h*f2)
f3 = -k*x3
x4 = h*(f0 + 2.d0*f1 + 2.d0*f2 + f3)/6.d0
y  = y + x4
f0v = y 
y1 = y+(h*f0v/2.d0)
f1v = y1
y2 = y + (h*f1v/2.d0) 
f2v = y2
y3 = y + (h*f2v)
f3v = Y3
y4 = h*(f0v + 2.d0*f1v + 2.d0*f2v + f3v)/6.d0
x = x + y4
   write(66,*) t,x,y
   t = t + h
END DO

END PROGRAM RK

