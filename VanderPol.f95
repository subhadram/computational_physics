PROGRAM RK
IMPLICIT NONE
REAL*8:: x,y,h,f0x,f1x,f2x,f3x,rk4,y1,y2,y3,y4,y5,t,k,rk3,f0y,f2y,f1y,f3y,x2,x3,x4,x1,f4x,f4y,energy,ep,fx,fy
INTEGER:: n,m
ep = 2.8
k = 1.d0
t = 0.d0
h = 0.02
n = 10000
x = 0.10
y = 0.100
DO m = 1,n 

f0x = y
f0y = ep*(1-(x**2))*y - x
x1 = x+(h*f0x/2.d0)
y1 = y+(h*f0y/2.d0)
f1x = y1
f1y = ep*(1-(x1**2))*y1 - x1
x2 = x + (h*f1x/2.d0) 
y2 = y + (h*f1y/2.d0) 
f2x = y2
f2y = ep*(1-(x2**2))*y2 - x2
x3 = x + (h*f2x)
y3 = y + (h*f2y)
f3x = y3
f3y = ep*(1-(x3**2))*y3 - x3
f4y = h*(f0y + 2.d0*f1y + 2.d0*f2y + f3y)/6.d0
f4x = h*(f0x + 2.d0*f1x + 2.d0*f2x + f3x)/6.d0
y  = y + f4y
x = x + f4x

 energy = 0.5d0*(y*y + k*x*x)
   write(77,*) t,x,y,energy
   t = t + h
END DO

END PROGRAM RK


