program hello
implicit none
integer :: start, fin,  i, ierr
integer ::  start4

print *,"Introduce comienzo y final"
read* , start, fin
print*, "Los años bisiestos entre ",start," y ",fin, " son:"

start4 = (start / 4) * 4

if (start4 < start) then
  start4 = start4 + 4
end if



do i=start4,fin,4
     if (mod(i,100) .NE.0  .OR. mod(i,400) == 0 )  then
      print*,i
     end if 
end do

end program hello

