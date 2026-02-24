! io.f90 — Formatted I/O and simple file I/O in Fortran.
!
! Demonstrates: PRINT with format strings, WRITE to a file,
!               OPEN / CLOSE / READ back from a file.
!
! Compile:  gfortran -Wall io.f90 -o io
! Run:      ./io
!           (creates and reads back a file called "data.txt")

PROGRAM io

  IMPLICIT NONE

  INTEGER, PARAMETER :: FILE_UNIT = 10
  INTEGER :: i, ios, read_i
  REAL    :: x, read_x
  CHARACTER(LEN=40) :: line

  ! ------------------------------------------------------------------
  ! 1. Formatted PRINT to stdout
  ! ------------------------------------------------------------------
  PRINT *, '--- Formatted console output ---'

  ! Integer format: I<width>
  PRINT '(A,I6)',    ' Formatted integer  : ', 42
  ! Real formats: F<width>.<decimals>, E<width>.<decimals>
  PRINT '(A,F10.4)', ' Fixed-point real   : ', 3.14159
  PRINT '(A,E12.4)', ' Scientific notation: ', 0.000123456
  ! Multiple values on one line
  DO i = 1, 5
    x = REAL(i) * 1.1
    PRINT '(A,I2,A,F6.2)', '  i=', i, '  x=', x
  END DO

  ! ------------------------------------------------------------------
  ! 2. Write data to a text file
  ! ------------------------------------------------------------------
  PRINT *, ''
  PRINT *, '--- Writing data.txt ---'
  OPEN(UNIT=FILE_UNIT, FILE='data.txt', STATUS='REPLACE', &
       ACTION='WRITE', IOSTAT=ios)
  IF (ios /= 0) THEN
    PRINT *, 'Error opening file for writing, IOSTAT=', ios
    STOP
  END IF

  DO i = 1, 5
    x = REAL(i) ** 2
    WRITE(FILE_UNIT, '(I4,F10.3)') i, x
  END DO

  CLOSE(FILE_UNIT)
  PRINT *, 'Wrote 5 lines to data.txt.'

  ! ------------------------------------------------------------------
  ! 3. Read the file back and print it
  ! ------------------------------------------------------------------
  PRINT *, ''
  PRINT *, '--- Reading data.txt back ---'
  OPEN(UNIT=FILE_UNIT, FILE='data.txt', STATUS='OLD', &
       ACTION='READ', IOSTAT=ios)
  IF (ios /= 0) THEN
    PRINT *, 'Error opening file for reading, IOSTAT=', ios
    STOP
  END IF

  DO
    READ(FILE_UNIT, '(I4,F10.3)', IOSTAT=ios) read_i, read_x
    IF (ios /= 0) EXIT    ! end of file or error
    PRINT '(A,I4,A,F10.3)', '  i=', read_i, '  x^2=', read_x
  END DO

  CLOSE(FILE_UNIT)

END PROGRAM io

