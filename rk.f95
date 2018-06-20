PROGRAM RK
IMPLICIT NONE
REAL*8:: x,v,h,f0x,f1x,f2x,f3x,rk4,v1,v2,v3,v4,v5,t,k,rk3,f0v,f2v,f1v,f3v,x2,x3,x4,x1,f4x,f4v,energy
INTEGER:: n,m
k = 1.d0
t = 0.d0
h = 0.02
n = 1000
x = 0.00
v = 0.100
DO m = 1,n 

f0v = -k*x 
f0x = v 
x1 = x+(h*f0x/2.d0)
v1 = v+(h*f0v/2.d0)
f1v = -k*x1
f1x = v1
x2 = x + (h*f1x/2.d0) 
v2 = v + (h*f1v/2.d0) 
f2v = -k*x2
f2x = v2
x3 = x + (h*f2x)
v3 = v + (h*f2v)
f3v = -k*x3
f3x = v3
f4v = h*(f0v + 2.d0*f1v + 2.d0*f2v + f3v)/6.d0
f4x = h*(f0x + 2.d0*f1x + 2.d0*f2x + f3x)/6.d0
v  = v + f4v
x = x + f4x
energy = 0.5d0*(v*v + k*x*x)
   write(77,*) t,x,v,energy
   t = t + h
END DO

END PROGRAM RK

