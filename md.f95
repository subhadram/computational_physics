program moldyn
IMPLICIT NONE 
real*8,dimension(:),allocatable::POS(:),V(:),A(:)
REAL*8:: Kb ,si,t,rn(3),af,ds
Integer::l,i,j,n,nn
print*,"Enter l"
read*, l
t = 1
af = sqrt(t/12.d0)
n = int(l**3 * 0.4)
nn = 3*n
allocate(pos(nn))
allocate(v(nn))
allocate(a(nn))

do i = 1,n
	pos(3*i - 2) = 3*i
	pos(3*i - 1) = 3*i
	pos(3*i) = 3*i
	call random_number(rn)
	v(3*i - 2) = af*(rn(1) - 0.5d0)
	v(3*i - 1) = af*(rn(2) - 0.5d0)
	v(3*i ) = af*(rn(3) - 0.5d0)
end do



do j = 1,n
	do k = 1,n
		if(k.ne.j)
		dx = pos(3*k-2)-pos(3*j-2)
		dy = pos(3*k-1)-pos(3*j-1) 
		dz = pos(3*k)-pos(3*j)
		if(abs(dx).gt.l/2.d0) dx = l - abs(dx)
		if(abs(dy).gt.l/2.d0) dy = l - abs(dy)
		if(abs(dz).gt.l/2.d0) dz = l - abs(dz)
		ds = sqrt(dx**2 + dy**2 + dz**2)
		if (ds.gt.2.5) then
		f = 0.d0
		else
		f = 
end program moldyn
