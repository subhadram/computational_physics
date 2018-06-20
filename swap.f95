PROGRAM swap 
IMPLICIT NONE
REAL:: x,y,z
READ*, x,y
z = x
x = y
y = z
PRINT* ,  x,  y
END PROGRAM swap


