SUBROUTINE sb_plot(signal, signal_n, file)
  INTEGER, INTENT(IN), VALUE :: signal_n
  REAL, DIMENSION(signal_n), INTENT(IN) :: signal
  CHARACTER(*), INTENT(IN) :: file

  INTEGER :: file_unit

  OPEN (NEWUNIT=file_unit, FILE=file)

  DO i = 1, size(signal)
    WRITE (UNIT=file_unit, FMT=*) i, signal(i)
  END DO

  CLOSE (UNIT=file_unit)
END SUBROUTINE
