program test
  integer :: stuff

  write(*, *) "called stuff ", stuff(1), " times"

end program test

recursive function stuff(n) result (tmp)
  integer :: tmp
  integer :: n

  tmp = 1
  if(n < 5) then
    tmp = tmp+stuff(n+1)
  endif

end function stuff
