program hanoi
   implicit none
   call move (3,1,3)

contains

  ! move 1..n from from to to
   recursive subroutine move (n, from , to)
    integer, intent(in) :: n, from , to
    integer :: other
    if(n==1)then
      print*, n, "(" , from, "->", to, ")" 
    else  
      ! other = {1,2,3} - {from , to}
      if(from == 1 .AND. to == 3 .OR.from == 3 .AND. to == 1 )then
        other = 2
      else if(from == 1 .AND. to == 2 .OR.from == 2 .AND. to == 1 )then
        other = 3
      else if(from == 2 .AND. to == 3 .OR.from == 3 .AND. to == 2 )then
        other = 1
      end if
      call move(n-1, from, other)
      print*, n, "(" , from, "->", to, ")" 
      call move(n-1, other, to)
      
    end if  
   end subroutine move


end program hanoi
