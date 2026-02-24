! arrays.f90 — 1-D and 2-D arrays in Fortran.
!
! Demonstrates: array declaration, array constructor, whole-array
!               arithmetic, SUM/MAXVAL/MINVAL intrinsics, DO loops
!               over arrays, and a simple 2-D (matrix) example.
!
! Compile:  gfortran -Wall arrays.f90 -o arrays
! Run:      ./arrays

PROGRAM arrays

  IMPLICIT NONE

  INTEGER, PARAMETER :: N = 5, ROWS = 3, COLS = 4

  ! All declarations before any executable statements.
  REAL    :: v(N)
  REAL    :: mat(ROWS, COLS)
  INTEGER :: i, j

  ! Array constructor — fill all elements at once
  v = (/ 1.0, 4.0, 9.0, 16.0, 25.0 /)

  PRINT *, '--- 1-D array v ---'
  PRINT *, v

  ! Whole-array arithmetic (no loop needed!)
  PRINT *, 'SQRT of each element:', SQRT(v)
  PRINT '(A,F6.1)', ' SUM    = ', SUM(v)
  PRINT '(A,F6.1)', ' MAXVAL = ', MAXVAL(v)
  PRINT '(A,F6.1)', ' MINVAL = ', MINVAL(v)

  ! Element-by-element with a DO loop
  PRINT *, '--- Squares via DO loop ---'
  DO i = 1, N
    v(i) = REAL(i) ** 2
    PRINT '(A,I1,A,F6.1)', '  v(', i, ') = ', v(i)
  END DO

  ! ------------------------------------------------------------------
  ! 2. Two-dimensional array (matrix)
  ! ------------------------------------------------------------------

  ! Fill: mat(i,j) = i * j
  DO i = 1, ROWS
    DO j = 1, COLS
      mat(i, j) = REAL(i * j)
    END DO
  END DO

  PRINT *, ''
  PRINT *, '--- 2-D array mat (multiplication table) ---'
  DO i = 1, ROWS
    PRINT '(4F6.1)', mat(i, :)   ! print entire row at once
  END DO

  PRINT '(A,F6.1)', ' Total sum of matrix = ', SUM(mat)

END PROGRAM arrays

