-- ciekawsza implementacja liczb zespolonych niż manifest types

make_rect a b =
  \ m ->
    case m of
      "real_part" -> a
      "im_part" -> b
      "magnitude" -> sqrt (a^2 + b^2)
      "angle" -> atan (b / a)

make_polar r angle =
  \ m ->
    case m of
      "real_part" -> r * cos angle
      "im_part" -> r * sin angle
      "magnitude" -> r
      "angle" -> angle

real_part z = z "real_part"
im_part z = z "im_part"
magnitude z = z "magnitude"
angle z = z "angle"

sum_complex z1 z2 =
  make_rect ((real_part z1) + (real_part z2)) ((im_part z1) + (im_part z2))

