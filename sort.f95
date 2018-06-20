PROGRAM sort
IMPLICIT NONE
REAL,DIMENSION(:),ALLOCATABLE::asc
INTEGER::n,i,h,v,l,p
PRINT*,"Enter size of array"
READ* ,n
ALLOCATE(asc(n))
DO i = 1,n
          PRINT*,"Enter an element"
          READ*,h
          asc(i) = h
END DO
PRINT*,"Original array"
PRINT*,asc

DO i = 1,n
	v = asc(i)
	DO  l = i-1, 0 , -1
		IF(asc(l)>v) THEN
		asc(l+1) = asc(l)
		p = l 
		asc(p) = v
		END IF
	END DO

END DO
PRINT*,"Sorted array"
PRINT*,asc
DEALLOCATE(asc)
END PROGRAM sort
