SUBROUTINE sb_resample(in, in_n, out, out_n)
  INTEGER, INTENT(IN), VALUE :: in_n, out_n
  REAL, DIMENSION(in_n), INTENT(IN) :: in
  REAL, DIMENSION(out_n), INTENT(OUT) :: out
  out = (/ (in(1 + (i - 1) * in_n / out_n), i = 1, out_n) /)
END SUBROUTINE
