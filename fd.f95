PRogram FINITEDIFFERENCE
IMPLICIT NONE
REAL*8::x,y(102),h,a,b,y1(102)
INTEGER::i,j,k
a = 0.d0
b = 1.d0
x = 0.d0
h = 0.01
do k = 1,102
	y(k) = 100.d0*x
Write(111,*) x,y(k)
x = h*dfloat(k)
end do
do i = 1,1000
y1 = y
 do j = 1,101
	y(j) = ((1.d0 - ((5.d0*h)/2.d0)*y1(j+1)) + (1.d0 + ((5.d0*h)/2.d0)*y1(j-1)))*(1.d0/(2.d0 - 10*h*h))*10.d0*h
	write(444,*) y1(j)
	if (i == 999) then
         Write(888,*) x ,y(j)
	end if
	x =  h*dfloat(j)
end do
x = 0.d0
end do
end program FINITEDIFFERENCE
