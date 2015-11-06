program hello
implicit none
real, dimension(:), allocatable :: a
integer :: n, i, ierr


     interface
         subroutine mergesort(arr1,m1,n1)
           real, dimension(:), allocatable, intent(inout) :: arr1
           integer, intent(in) :: m1,n1 
         end subroutine mergesort
      end interface




print *,"Introduce n"
read* , n
allocate(a(n), STAT=ierr)
if (ierr/=0) print*, "allocation failed"
print *,"Introduce array de ", n, " elementos"
read*, a



print*, "Ordered"
call mergesort(a,0,n)
print*, a

deallocate(a)

end program hello




recursive subroutine mergesort(arr, i1, i2)
  implicit none
  real, dimension(:), allocatable, intent(inout) :: arr
  integer, intent(in) :: i1,i2
  integer ::  middle
  real, dimension(:), allocatable  :: temp  
  integer :: j,j1,j2

  logical :: fin1, fin2

  print*, "i1=", i1, ", i2=", i2

  if(i2-i1>1) then
    middle=(i2-i1)/2
    print*,"MIDDLE=", middle
    print*, "array before MS"
    print*, arr
    print*, "EA"
    call mergesort(arr,i1,middle)
    call mergesort(arr,middle+1,i2)
    print*, "array after MS"
    print*, arr
    print*, "EA"
    allocate(temp(i2-i1))
    do j=i1,i2
      temp(j+1-i1) = arr(j)
    end do

    fin1 = .FALSE.
    fin2 = .FALSE.
    j = i1
    j1 = i1
    j2 = middle+1  
    do while (j<=i2)
      if(j1==i1+1) then
        fin1 = .TRUE.
      end if
      if(j2==i2+1) then
        fin2 = .TRUE.
      end if

      if((temp(j1)>temp(j2) .AND. .NOT. fin2) .OR.  fin1 ) then
        arr(j) = temp(j2-i1+1)
        j2=j2+1  
      else
        arr(j) = temp(j1-i1+1)
        j1=j1+1
      end if
      j = j+1
    end do   
    deallocate(temp)
 
    
  endif
  
  
 
end subroutine


