program test
  implicit none
  integer :: fib
  
  integer :: i
  
  
  !print*, fib(6)
 do i=0,20
    print*, fib(i)
 end do

end program test

recursive function fib(n) result (res)
  implicit none
  integer :: res
  integer, intent(in) :: n

  res = 1
  if(n == 0) then
    res = 0
  else if(n==1) then
    res = 1
  else 
    res = fib(n-1) + fib(n-2)
  endif

end function fib
