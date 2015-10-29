program hello
implicit none
integer, dimension(:), allocatable :: numbers
integer :: NN, i, ierr

print *,"Introduce N"
read* , NN
allocate(numbers(NN), STAT=ierr)
if (ierr/=0) print*, "allocation failed"
print *,"Introduce array de ", NN, " elementos"
read*, numbers

print*, "Reversed"

print*, numbers(NN:1:-1)

deallocate(numbers)

end program hello

