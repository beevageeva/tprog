program hello
implicit none
integer, dimension(:), allocatable :: digitsNumber
integer :: i, ierr, ndigits
integer :: NN, lastNN, p10
logical :: isPal


print *,"Introduce N"
read* , NN


ndigits = INT(LOG10(REAL(NN))) + 1

print*, "number of digits ", ndigits

allocate(digitsNumber(ndigits), STAT=ierr)

do i = ndigits-1,1,-1
  p10 = 10**i
  digitsNumber(ndigits-i)=NN/p10
  NN=NN-p10*digitsNumber(ndigits-i) 
end do
digitsNumber(ndigits) = NN

isPal = .TRUE.

print*, digitsNumber

do i = 1,ndigits/2
  if(digitsNumber(i) .NE. digitsNumber(ndigits-i+1)) then
    isPal = .FALSE.
    exit
  end if
  
end do

print*, "is pal is ", isPal


deallocate(digitsNumber)

end program hello

