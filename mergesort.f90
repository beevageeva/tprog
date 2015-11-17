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
call mergesort(a,1,n)
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


  print*, "i1=", i1, ", i2=", i2

  if(i2-i1>0) then
    middle=(i1+i2-1)/2
    print*,"MIDDLE=", middle
!    print*, "array before MS"
!    print*, arr
!    print*, "EA"
    call mergesort(arr,i1,middle)
    call mergesort(arr,middle+1,i2)
!    print*, "array after MS"
!    print*, arr
!    print*, "EA"
    allocate(temp(i2-i1+1))
!    do j=i1,i2
!      temp(j+1-i1) = arr(j)
!    end do
    temp = arr(i1:i2)


    j = i1
    j1 = i1
    j2 = middle+1  
    print*, "back to i1=", i1, ", i2=", i2, ",middle=",middle
    print*, "temp"
    print* ,temp
    !do while (.NOT. fin1 .OR. .NOT. fin2)
    do while (j1<=middle .AND. j2<=i2)


      if(temp(j1-i1+1)<temp(j2-i1+1)) then
        arr(j) = temp(j1-i1+1)
        print*, "1 arr(",j,")=temp(",(j1-i1+1),")" 
        j1=j1+1  
	      if(j1==middle+1) then !fin1 = true
	        do while(j2<=i2)
	          arr(j2) = temp(j2-i1+1)
            print*, "fin1=true, arr(",j2,")=temp(",(j2-i1+1),")" 
	          j2 = j2+1
	        end do
	        exit !or cycle
	       end if 
      else 
        arr(j) = temp(j2-i1+1)
        print*, "2 arr(",j,")=temp(",(j2-i1+1),")" 
        j2=j2+1
	      if(j2==i2+1) then !fin2 = true
	        do while(j1<=middle)
	          arr(j1+i2-middle) = temp(j1-i1+1)
            print*, "fin2= true arr(",j1+i2-middle,")=temp(",(j1-i1+1),")" 
	          j1 = j1+1
	        end do
	        exit !or cycle
	      end if
      end if
      j = j+1

        
    end do   
    deallocate(temp)
    
  endif
  
  
 
end subroutine


