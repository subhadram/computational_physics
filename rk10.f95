PROGRAM RK
IMPLICIT NONE
REAL*8:: x(10),v(10),h,f0x(10),f1x(10),f2x(10),f3x(10),rk4,v1(10),v2(10),v3(10),v4(10),v5(10)
REAL*8::t,k,rk3,f0v(10),f2v(10),f1v(10),f3v(10),f4x(10),f4v(10),energy(10)
Real*8:: x1(10),x12(10),x13(10),x2(10),x22(10),x23(10),x3(10),x32(10),x33(10),ngh(3)
INTEGER:: n,m,i,n1,n2
k = 1.d0
t = 0.d0
h = 0.02
n = 10000
x = [0.1,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
v = [0.2d0,0.2d0,0.2d0,0.2d0,0.2d0,0.2d0,0.2d0,0.2d0,0.2d0,0.2d0]
energy = 0.0
DO m = 1,n 
do i = 1,10
n1 = (i-1)
n2 = (i + 1)
if (n1 == 0) n1 = 10
if (n2 == 11) n2 = 1
f0v(i) = k*(x(n2)+x(n1) - 2*x(i))
f0x(i) = v(i) 
end do
do i = 1,10
n1 = (i-1)
n2 = (i + 1)
if (n1 == 0) n1 = 10
if (n2 == 11) n2 = 1
x1(i)= x(i)+(h*f0x(i)/2.d0)
!x12(i) = x(n1)+(h*f0x(i)/2.d0)
!x13(i) = x(n2)+(h*f0x(i)/2.d0)
v1(i) = v(i)+(h*f0v(i)/2.d0)

end do
do i = 1,10
n1 = (i-1)
n2 = (i + 1)
if (n1 == 0) n1 = 10
if (n2 == 11) n2 = 1
f1v(i) = k*(x1(n1)+x1(n2)-2*x1(i))
f1x(i) = v1(i)
x2(i) = x(i) + (h*f1x(i)/2.d0) 
!x2(i) = x(n1) + (h*f1x(i)/2.d0)
!x23(i) = x(n2) + (h*f1x(i)/2.d0)
v2(i) = v(i) + (h*f1v(i)/2.d0) 

end do
do i = 1,10
n1 = (i-1)
n2 = (i + 1)
if (n1 == 0) n1 = 10
if (n2 == 11) n2 = 1
f2v(i) = k*(x2(n1)+x2(n2) - 2*x2(i))
f2x(i) = v2(i)
x3(i) = x(i) + (h*f2x(i))
!x32(i) = x(n1) + (h*f2x(i))
!x33(i) = x(n2) + (h*f2x(i))
v3(i) = v(i) + (h*f2v(i))

end do
do i = 1,10
n1 = (i-1)
n2 = (i + 1)
if (n1 == 0) n1 = 10
if (n2 == 11) n2 = 1
f3v(i) = k*(x3(n1)+x3(n2) - 2*x3(i))
f3x(i) = v3(i)
end do 
do i = 1,10
n1 = (i-1)
n2 = (i + 1)
if (n1 == 0) n1 = 10
if (n2 == 11) n2 = 1
f4v(i) = h*(f0v(i) + 2.d0*f1v(i) + 2.d0*f2v(i) + f3v(i))/6.d0
f4x(i) = h*(f0x(i) + 2.d0*f1x(i) + 2.d0*f2x(i) + f3x(i))/6.d0
v(i)  = v(i) + f4v(i)
x(i) = x(i) + f4x(i)
end do
do i = 1,10
n1 = (i-1)
n2 = (i + 1)
if (n1 == 0) n1 = 10
if (n2 == 11) n2 = 1
energy(i) = energy(i) + 0.5d0*(v(i)*v(i)) + 0.25d0*k*(((x(n1)-x(i))**2)+((x(n1)-x(i))**2))
end do
   write(278,*) t,x,v,sum(energy)
   energy = 0.0 
   t = t + h
END DO

END PROGRAM RK

