program test
  implicit none
  integer :: fact
  print*, fact(3)

end program test

recursive function fact(n) result (tmp)
  implicit none
  integer :: tmp
  integer, intent(in) :: n

  tmp = 1
  if(n == 1) then
    tmp = 1
  else
    tmp = n*fact(n-1)
  endif

end function fact
