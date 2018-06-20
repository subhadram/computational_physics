PROGRAM isi
IMPLICIT NONE
Real*8 ,dimension(:,:),allocatable::ss
INTEGER*8,DIMENSION(:,:),ALLOCATABLE::SPIN,E,F
REAL*8 :: x,y,rnlt,av,w,l,avv,T,avm,avg,cv,chi,q,pop
Integer::i,j,p,ngbr(4),sa,sb,ra,rb,s,r,de,sums,suma,d,g,h,u,v,z,summ,sumn,po,n

T = 2.d0
sums = 0
summ = 0
z= 1
Print*, "Give lattice size"
Read*,n
allocate(SS(n,n))
allocate(Spin(n,n))
allocate(e(n,n))
allocate(f(n,n))
Call RANDOM_NUMBER(ss)
do i = 1,n
	do j = 1,n
		
		if (ss(i,j).gt.0.5)then
			spin(i,j) = int(1)
		else 
			spin(i,j) = int(-1)
		end if
	end do
end do
do s = 1,n
		do r = 1,n	
		sa = MOD(s-1,n) 
		if (sa == 0) sa = n 
		sb = MOD(s+1,n)
		if (sb == 0) sb = n
		ra = MOD(r-1,n)
                if (ra == 0) ra = n 
		rb = Mod(r+1,n)
                if (rb == 0) rb = n 
		ngbr(1) = spin(s,ra)
		ngbr(2) = spin(sa,r)
		ngbr(3) = spin(s,rb)
		ngbr(4) = spin(sb,r)
		e(r,s) = spin(r,s)*sum(ngbr)
		!print* , ngbr
		end do 
	end do
suma = sum(e)
po = sum(spin)
av = suma/dfloat(2*n*n)
pop = po/dfloat(n*n)
do g=1,n
	do h =1,n
		sums = sums + e(g,h)*e(g,h)
	end do
end do
avv = (sums)/dfloat(2*n*n) 
print*,av,pop,po

do i= 1,50000
	call random_seed(size = z)
	do d = 1,n*n
		Call random_number(x)
		call random_seed(size = z)
		Call random_number(y)
		x = x*dfloat(n)
		y = y*dfloat(n)
		u = int(x) + 1
		v = int(y) + 1
		!print*,u,v
		f(u,v) = -e(u,v)
		de = f(u,v)-e(u,v)
		if(de.lt.0)then
			spin(u,v) = -spin(u,v)
			else
			call random_seed(size = z)
			call random_number(w)
			l = exp(-de/T)
			if (w.le.l) then
				spin(u,v) = -spin(u,v)
			end if
		end if
	end do
	do s = 1,n
	do r = 1,n	
			sa = MOD(s-1,n) 
			if (sa == 0) sa = n 
			sb = MOD(s+1,n)
			if (sb == 0) sb = n
			ra = MOD(r-1,n)
                	if (ra == 0) ra = n 
			rb = Mod(r+1,n)
                	if (rb == 0) rb = n 
			ngbr(1) = spin(s,ra)
			ngbr(2) = spin(sa,r)
			ngbr(3) = spin(s,rb)
			ngbr(4) = spin(sb,r)
			e(r,s) = spin(r,s)*sum(ngbr)
			!print* , ngbr
		end do 
	end do
write(555,*) e
suma = sum(e)
sumn = sum(spin)
av = suma/dfloat(2*n*n)
AVg = sumn/dfloat(n*n)
do g=1,n
	do h =1,n
		sums = sums + e(g,h)*e(g,h)
		summ = summ + spin(g,h)*spin(g,h)
	end do
end do
avv = (sums)/dfloat(2*n*n)
avm = (summ)/dfloat(n*n)
 cv = (avv - (av*av))/(T*T)
 CHI = (avm - (avg*avg))/T
sums = 0
summ = 0
write(1101,*) i,av,cv,chi
end do
deallocate(SS)
deallocate(Spin)
deallocate(e)
deallocate(f)
END PROGRAM isi



!function rnlt(spin)
!Implicit none
!Real*8 :: x,y
!Integer :: u,v

