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
av = sum(e)/dfloat(100)
print*,av 		
print*,sum(spin)/dfloat(100)


!!do p = 1,100
 !!   l = rnlt(spin)
END PROGRAM isi

