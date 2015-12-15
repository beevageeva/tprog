PROGRAM PRACTICA
  IMPLICIT NONE
  INTEGER :: lado, i, j, t=0
  REAL :: min,max,diff
  REAL, DIMENSION( : , : ), ALLOCATABLE :: datu
  READ*, lado
  ALLOCATE(datu(0:lado+1,0:lado+1))
  datu = 0.0
  DO i=1,lado
     READ*, datu(i,1:lado)
  END DO
  min=MINVAL(datu(1:lado,1:lado))
  max=MAXVAL(datu(1:lado,1:lado))
  diff=max-min
  PRINT*, "t = ",t, "diff = ",diff
  DO WHILE (diff .GE. 1)
     t=t+1
     datu(1:lado,1:lado) = 0.99*datu(1:lado,1:lado) + 0.01*((datu(0:lado-1,1:lado) + & 
 datu(2:lado+1,1:lado) + datu(1:lado,0:lado-1) + datu(1:lado,2:lado+1))/ 4)
     min=MINVAL(datu(1:lado,1:lado))
     max=MAXVAL(datu(1:lado,1:lado))
     diff=max-min
     IF (MOD(t,1000) .EQ. 0) PRINT*, "t = ",t, "diff = ",diff
  END DO
  PRINT*, "Final t is: ", t
END PROGRAM PRACTICA

