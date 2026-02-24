! variables.f90 — Fortran's basic data types.
!
! Demonstrates: INTEGER, REAL, DOUBLE PRECISION, CHARACTER, LOGICAL,
!               variable declaration, assignment, and PRINT output.
!
! Compile:  gfortran -Wall variables.f90 -o variables
! Run:      ./variables

PROGRAM variables

  IMPLICIT NONE

  ! All declarations must appear before any executable statements.
  INTEGER          :: count, year
  REAL             :: temperature, pi_approx
  DOUBLE PRECISION :: precise_pi
  CHARACTER(LEN=20):: greeting
  LOGICAL          :: is_even, is_hot

  ! Integer — whole numbers
  count = 42
  year  = 2026

  ! Real — single-precision floating-point (~7 significant digits)
  temperature = 98.6
  pi_approx   = 3.14159

  ! Double precision — ~15 significant digits
  precise_pi = 3.14159265358979D0

  ! Character — fixed-length text
  greeting = 'Hello, Fortran!'

  ! Logical — boolean (.TRUE. or .FALSE.)
  is_even = MOD(count, 2) == 0
  is_hot  = temperature > 98.0

  ! Print results
  PRINT *, 'INTEGER  count      =', count
  PRINT *, 'INTEGER  year       =', year
  PRINT *, 'REAL     temperature=', temperature
  PRINT *, 'REAL     pi_approx  =', pi_approx
  PRINT '(A,F20.15)', ' DBLE  precise_pi  =', precise_pi
  PRINT *, 'CHARACTER greeting  =', TRIM(greeting)
  PRINT *, 'LOGICAL  is_even    =', is_even
  PRINT *, 'LOGICAL  is_hot     =', is_hot

END PROGRAM variables

