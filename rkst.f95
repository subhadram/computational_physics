PROGRAM RK
IMPLICIT NONE
REAL*8:: x(10),v(10),h,f0x,f1x,f2x,f3x,rk4,v1,v2,v3,v4,v5,t,k,rk3,f0v,f2v,f1v,f3v,x2,x3,x4,x1(3),f4x,f4v,energy
Real*8:: x11,x12,x13,x21,x22,x23,x31,x32,x33,ngh(3)
INTEGER:: n,m,i
k = 1.d0
t = 0.d0
h = 0.02
n = 10000
x = [0.1,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
v = [0.1d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,0.d0]
energy = 0.0
DO m = 1,n 
do i = 1,10
ngh(1) = x(mod(i-2,10)+1)
ngh(2) = x(mod(i+1,10))
f0v = k*(ngh(2)+ngh(1) - 2*x(i))
f0x = v(i) 
x11 = x(i)+(h*f0x/2.d0)
x12 = ngh(1)+(h*f0x/2.d0)
x13 = ngh(2)+(h*f0x/2.d0)
v1 = v(i)+(h*f0v/2.d0)
f1v = k*(x12+x13-2*x11)
f1x = v1
x21 = x(i) + (h*f1x/2.d0) 
x22 = ngh(1) + (h*f1x/2.d0)
x23 = ngh(2) + (h*f1x/2.d0)
v2 = v(i) + (h*f1v/2.d0) 
f2v = k*(x22+x23 - 2*x21)
f2x = v2
x31 = x(i) + (h*f2x)
x32 = ngh(1) + (h*f2x)
x33 = ngh(2) + (h*f2x)
v3 = v(i) + (h*f2v)
f3v = k*(x32+x33 - 2*x31)
f3x = v3
f4v = h*(f0v + 2.d0*f1v + 2.d0*f2v + f3v)/6.d0
f4x = h*(f0x + 2.d0*f1x + 2.d0*f2x + f3x)/6.d0
v(i)  = v(i) + f4v
x(i) = x(i) + f4x
energy = energy + 0.5d0*(v(i)*v(i)) - 0.5d0*k*((ngh(1)+ngh(2)-2*x(i))**2)
end do
   write(278,*) t,x,v,energy
   energy = 0.0 
   t = t + h
END DO

END PROGRAM RK



