program main

  implicit none

  real :: x(3), y(3)
  integer :: d(2), col ,row
  integer, dimension(2), parameter :: L = [20,16]
  integer :: k, p(2,2)
  integer :: i, j, tile_sizex, tile_sizey
  integer, allocatable :: s(:,:)[:],s2(:,:)[:,:], sglobal(:,:)[:] , indices(:,:)
  integer :: i1(2), j1(2)

  
  !call random_init(.false.,.true.)
  !call random_number(x)
  !print*, this_image(), x
  d = [4,1]
  tile_sizex = L(1)/d(1)
  tile_sizey = L(2)/d(2)

  print*, any(L/d /= 0)
  
  allocate(s(1:tile_sizex,1:tile_sizey)[*])
  allocate(s2(1:tile_sizex,1:tile_sizey)[d(1),*])
  allocate(sglobal(L(1),L(2))[*])
  allocate(indices(2,2))

  k = 0
  do i = 1, d(1)
     do j = 1, d(2)
        p(i,j) = 2*(j-1) + i
     end do
  end do

  if(mod(this_image(),d(1)) == 0) then
     row = d(1)
  else
     row = mod(this_image(),d(1))
  end if

  if(mod(this_image(),d(1)*d(2)) /= 0)then
     col = (mod(this_image(),d(1)*d(2)) - row)/d(1)+1
  else
     col = (d(1)*d(2) - row)/d(1)+1
  end if
  sync all
  print'("image = ",i1,x, "(", i1, ",", i1, ") ", 2(i2,x, i2))', This_image(), row, col, tile_indices(L(1),row,d(1))&
       , tile_indices(L(2),col,d(2))
  i1 = tile_indices(L(1),row,d(1))
  j1 = tile_indices(L(2),col,d(2))

  s = this_image()
  
  
  sglobal(i1(1):i1(2),j1(1):j1(2))[1] = s
  sync all
  if (this_image() == 1) then
     do i = 1, L(1)
        print'(*(i1,x))', sglobal(i,:)
     end do
  end if

contains

  function tile_indices(L,im,n)
    implicit none
    integer, intent(in)  :: L, im, n
    integer, dimension(2):: tile_indices

    tile_indices(1) = (L * (im - 1) ) / n + 1 
    tile_indices(2) = (L * im ) / n
    
  end function tile_indices

end program main
