program dwa
      real :: A(2,2)
      A(1,:) = (/1,2/)
      A(2,:) = (/3,4/)

      print*, A(2,1)
      print*, sum(A)
      print*, transpose(A)

end program dwa
