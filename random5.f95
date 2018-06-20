program test4
real*8,dimension(:),allocatable::y
real*8::ck,m,s=0.0,ms,h=0.0,ik=0.0,t=0.0,g
integer::n,i,k
print*,"enter no. of random numbers"
read*,n
print*,"enter range for k"
read*,k
allocate(y(n))
do i=1,n
y(i)=rand()
end do
do i=1,n
    s=s+y(i)
    h=h+y(i)*y(i)
end do
m=s/real(n)
print*,"mean=",m
ms=h/real(n)
print*,"second moment=",ms
do j=0,k
    do i=1,n-j
       t=t+y(i)*y(i+j)
    end do
    g=t/real(n-j)
    ck=(g-(m*m))/(ms-(m*m))
    write(25,*)j,ck
    t=0.0
end do
deallocate(y)
end program test4
