module file_read
	contains
	subroutine read_1d(a)
		integer, allocatable, intent(inout) :: a(:)
		integer:: s
		integer, parameter :: in_unit=100
	  open (unit=in_unit,file="data.txt",action="read")
	  read (in_unit,*) s
	  allocate(a(s))
	  read (in_unit,*) a
	  close (in_unit)
	end subroutine
end module

program sumfile
	use file_read
  integer, allocatable :: b(:)
  call read_1d(b)
  print*, sum(b)
  print*, size(b)
end program sumfile

! zawartosc pliku data.txt:
! 10
! 1 2 3 4 5 6 7 8 9 10