-- manifest types - mniej ciekawa implementacja liczb zespolonych

make_rect a b = ("r", [a, b])
make_polar r angle = ("p", [r, angle])

real_part (("r", [a, b])) = a
real_part (("p", [r, angle])) = r * cos angle

im_part (("r", [a, b])) = b
im_part (("p", [r, angle])) = r * sin angle

magnitude (("r", [a, b])) = atan (b / a)
magnitude (("p", [r, angle])) = r

angle (("r", [a, b])) = sqrt (a^2 + b^2)
angle (("p", [r, a])) = a

sum_complex z1 z2 =
  make_rect ((real_part z1) + (real_part z2)) ((im_part z1) + (im_part z2))

