program main

  implicit none

  real :: x(3), y(3)
  integer :: d(2), col ,row
  integer, parameter :: L = 16
  integer :: k, p(2,2)
  integer :: i, j, tile_sizex, tile_sizey
  integer, allocatable :: s(:,:)[:], sglobal(:,:)[:] , indices(:,:)

  
  !call random_init(.false.,.true.)
  !call random_number(x)
  !print*, this_image(), x


  
  d = [2,2]
  tile_sizex = L/d(1)
  tile_sizey = L/d(2)

  allocate(s(1:tile_sizex,1:tile_sizey)[*])
  allocate(sglobal(L,L)[*])
  allocate(indices(2,2))

  k = 0
  do i = 1, d(1)
     do j = 1, d(2)
        p(i,j) = 2*(j-1) + i
     end do
  end do

  if( mod(this_image(),d(1)) == 0)then
     col = this_image()/d(1)
  else
     col = this_image()/d(1) + 1
  end if

  
  if( mod(this_image(),d(2)) == 0)then
     row = d(2)
  else
     row = mod(this_image(),d(2))
  end if

  !print*, This_image(), col, row
  indices(1,:) = tile_indices(L,col,d(1))
  indices(2,:) = tile_indices(L,row,d(2))


  !print*, indices(1,:), indices(2,:)
  
  s(:,:) = this_image()
  
  sglobal(indices(1,1):indices(1,2),indices(2,1):indices(2,2))[1] = s(:,:)
  sync all
  if (this_image() == 1) then
     do i = 1, L
        print*, sglobal(i,:)
     end do
  end if
  
contains

  pure function tile_indices(L,im,n)    
    integer, intent(in)  :: L, im, n
    integer, dimension(2):: tile_indices

    tile_indices(1) = (L * (im - 1) ) / n + 1 
    tile_indices(2) = (L * im ) / n
  end function tile_indices

end program main
