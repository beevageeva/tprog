program hello
implicit none
integer, dimension(:), allocatable :: digitsNumber
integer :: i, ierr, ndigits
real :: NN



print *,"Introduce N"
read* , NN


ndigits = INT(LOG10(NN)) + 1

print*, "number of digits ", ndigits

allocate(digitsNumber(ndigits), STAT=ierr)

do i = ndigits,0,-1
  print* , i
end do


deallocate digitsNumber

end program hello

