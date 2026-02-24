! subroutines.f90 — Procedures: SUBROUTINE and FUNCTION.
!
! Demonstrates: SUBROUTINE (side effects via arguments),
!               FUNCTION (returns a value), INTENT, CALL.
!
! Compile:  gfortran -Wall subroutines.f90 -o subroutines
! Run:      ./subroutines

PROGRAM subroutines

  IMPLICIT NONE

  REAL    :: a, b, result
  INTEGER :: n
  REAL    :: factorial_result

  ! ------------------------------------------------------------------
  ! 1. Call a SUBROUTINE that swaps two values
  ! ------------------------------------------------------------------
  a = 3.0
  b = 7.0
  PRINT '(A,F4.1,A,F4.1)', ' Before swap: a=', a, '  b=', b
  CALL swap(a, b)
  PRINT '(A,F4.1,A,F4.1)', ' After swap:  a=', a, '  b=', b

  ! ------------------------------------------------------------------
  ! 2. Call a FUNCTION that computes the hypotenuse
  ! ------------------------------------------------------------------
  a = 3.0
  b = 4.0
  result = hypotenuse(a, b)
  PRINT '(A,F4.1,A,F4.1,A,F6.3)', &
        ' hypotenuse(', a, ',', b, ') = ', result

  ! ------------------------------------------------------------------
  ! 3. Recursive FUNCTION — factorial
  ! ------------------------------------------------------------------
  DO n = 0, 7
    PRINT '(A,I1,A,F10.0)', ' ', n, '! = ', factorial(n)
  END DO

CONTAINS

  ! ------------------------------------------------------------------
  SUBROUTINE swap(x, y)
    REAL, INTENT(INOUT) :: x, y
    REAL :: tmp
    tmp = x
    x   = y
    y   = tmp
  END SUBROUTINE swap

  ! ------------------------------------------------------------------
  REAL FUNCTION hypotenuse(x, y)
    REAL, INTENT(IN) :: x, y
    hypotenuse = SQRT(x*x + y*y)
  END FUNCTION hypotenuse

  ! ------------------------------------------------------------------
  RECURSIVE REAL FUNCTION factorial(n) RESULT(res)
    INTEGER, INTENT(IN) :: n
    IF (n <= 1) THEN
      res = 1.0
    ELSE
      res = n * factorial(n - 1)
    END IF
  END FUNCTION factorial

END PROGRAM subroutines

