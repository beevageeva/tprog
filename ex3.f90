program hello
implicit none
integer, dimension(:,:), allocatable :: A,B,C
integer :: m,n,p, i,j,k, ierr


print *,"Introduce m n y p"
read* , m,n,p

allocate(A(m,n), STAT=ierr)
if (ierr/=0) print*, "A allocation failed"

allocate(B(n,p), STAT=ierr)
if (ierr/=0) print*, "B allocation failed"

allocate(C(m,p), STAT=ierr)
if (ierr/=0) print*, "C allocation failed"

print *,"Introduce matriz de ", m, "x",n,  " elementos"
do i=1,m
  print* , "Fila ", i
  read*, A(i,:)
end do

print *,"Introduce matriz de ", n, "x",p,  " elementos"
do i=1,n
  print* , "Fila ", i
  read*, B(i,:)
end do

do i=1,m
  do j=1,p
!    c(i,j) = 0
!    do k=1,n
!      c(i,j)= c(i,j) + a(i,k)*b(k,j)  
!    end do
      c(i,j)=  sum(a(i,:)*b(:,j))  
  end do
end do


print*, "C"

do i=1,m
!  do j=1,p
!    print*, c(i,j), " "
!  end do
  print*, c(i,:) 
end do


deallocate(A)
deallocate(B)
deallocate(C)


end program hello

