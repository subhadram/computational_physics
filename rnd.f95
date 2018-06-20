PROGRAM rnd
IMPLICIT NONE
INTEGER :: i,j,k,l,m,n,s,q,w
REAL*8  ::  r(1000) ,cnt(100) ,ran,h,c(500),sm,mn,si,e2,e3,ei,p(500),eik(500)
h = .01
 cnt = 0
 sm = 0 
 ei = 0
 eik = 0
 p = 0
CALL  RANDOM_NUMBER(r)  
do i=1,1000
write(23,*)  r(i)
enddo  
do j = 1,1000
  	ran=r(j)
	do k = 0,99
		if( h*k.lt.ran .and. (k+1)*h.gt.ran) then
			cnt(k) = cnt(k)+1
                endif
		
	end do
end do
do l = 0 ,99
write(24,*) h*l,cnt(l)
end do
do m = 1,1000
	sm = sm +r(m)
end do
mn = sm/1000.0
print*,mn
e2 = 0
do n = 1,1000
        e2 = e2 + (r(n)*r(n))
	write(25,*) r(n),r(n+1)
end do
e3 = e2/1000.0
si = (SQRT((e3) - (mn*mn)))
 print*, si

do s = 0 , 500
	do q= 0 ,(999-s)
		ei = ei + r(q)*r(q+s)
	end do
	eik(s) = ei/(real(1000-s)) 
	p(s) = ((eik(s) - (mn*mn))/(e2 - (mn*mn)))
	c(s) = c(s) + p(s)
	Write(26,*) r(s),c(s)
end do		
end  program
