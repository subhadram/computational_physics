PROGRAM isi
IMPLICIT NONE
REAL*8 :: ss(10,10),x,y,rnlt,av,w,l
Integer::i,j,p,spin(10,10),ngbr(4),sa,sb,ra,rb,s,r, e(10,10),f(10,10),de
Call RANDOM_NUMBER(ss)

do i = 1,10
	do j = 1,10
		if (ss(i,j).gt.0.5)then
			spin(i,j) = int(1)
		else 
			spin(i,j) = int(-1)
		end if
	end do
end do


do i= 1,1000
	Call random_number(x)
	Call random_number(y)
	x = x*10.0
	y = y*10.0
	u = int(x) + 1
	v = int(y) + 1
	f(u,v) = -e(u,v)
	de = f(u,v)-e(u,v)
	if (de.lt.0)then
	spin(u,v) = -spin(u,v)
	else
	call random_number(w)
	l = exp(-de/2.d0)
	if (w.le.l) spin(u,v)
	end if
		do s = 1,10
		do r = 1,10	
		sa = MOD(s-1,10) 
		if (sa == 0) sa = 10  
		sb = MOD(s+1,10)
		if (sb == 0) sb = 10 
		ra = MOD(r-1,10)
                if (ra == 0) ra = 10 
		rb = Mod(r+1,10)
                if (rb == 0) rb = 10 
		ngbr(1) = spin(s,ra)
		ngbr(2) = spin(sa,r)
		ngbr(3) = spin(s,rb)
		ngbr(4) = spin(sb,r)
		e(r,s) = spin(r,s)*sum(ngbr)
		!print* , ngbr
		end do 
	end do
       av = sum(e)/dfloat(2*100)
       print*,av 		
       print*,sum(spin)/dfloat(2*100)


!!do p = 1,100
 !!   l = rnlt(spin)
END PROGRAM isi



!function rnlt(spin)
!Implicit none
!Real*8 :: x,y
!Integer :: u,v
function neg()result(suma,sumaa)
Implicit none
INTEGER ::s,r,sa,sb,ra,rb,ngbr(4)
	do s = 1,10
		do r = 1,10	
		sa = MOD(s-1,10) 
		if (sa == 0) sa = 10  
		sb = MOD(s+1,10)
		if (sb == 0) sb = 10 
		ra = MOD(r-1,10)
                if (ra == 0) ra = 10 
		rb = Mod(r+1,10)
                if (rb == 0) rb = 10 
		ngbr(1) = spin(s,ra)
		ngbr(2) = spin(sa,r)
		ngbr(3) = spin(s,rb)
		ngbr(4) = spin(sb,r)
		e(r,s) = spin(r,s)*sum(ngbr)
		!print* , ngbr
		end do 
	end do
suma = sum(e)

END function neg

