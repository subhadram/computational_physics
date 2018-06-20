PROGRAM Waveeq
IMPLICIT NONE
REAL*8::Y(100,101),t,dt,x,c,h
integer::i,j,k
h = 0.01
c = 2.d0
x = 0.100
dt = 1.d0
do k = 1,101
	y(1,k) = sin(2*3.14*x)
x = h*dfloat(k)
WRITE(222,*) 1,Y(:,1)
end do

 do i = 2,100
   do j = 1,100
      y(i,j) = ((y(i-1,j-1) + y(i-1,j+1))*(c*c*dt*dt/(h*h))) + 2*(1 - ((c*c*dt*dt/(h*h))*(y(i-1,j)))) - y(i-2,j)
end do
      	write(222,*) i,y(:,i) 
   end do
end program waveeq
