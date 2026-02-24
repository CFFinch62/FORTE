! loops.f90 — Three styles of loop in Fortran.
!
! Demonstrates: counted DO loop, DO WHILE, DO with EXIT,
!               CYCLE (skip iteration), and implied DO in PRINT.
!
! Compile:  gfortran -Wall loops.f90 -o loops
! Run:      ./loops

PROGRAM loops

  IMPLICIT NONE

  INTEGER :: i, n, total
  REAL    :: x

  ! ------------------------------------------------------------------
  ! 1. Counted DO loop  (like a for-loop in C/Python)
  ! ------------------------------------------------------------------
  PRINT *, '--- Counted DO loop (1..5) ---'
  DO i = 1, 5
    PRINT *, '  i =', i
  END DO

  ! DO with a step value
  PRINT *, '--- DO loop, step 2 (1..9) ---'
  DO i = 1, 9, 2
    PRINT *, '  i =', i
  END DO

  ! ------------------------------------------------------------------
  ! 2. DO WHILE — loop while a condition is true
  ! ------------------------------------------------------------------
  PRINT *, '--- DO WHILE (halving x until x < 0.01) ---'
  x = 1.0
  DO WHILE (x >= 0.01)
    PRINT '(A,F8.4)', '  x = ', x
    x = x / 2.0
  END DO

  ! ------------------------------------------------------------------
  ! 3. Infinite DO with EXIT and CYCLE
  ! ------------------------------------------------------------------
  PRINT *, '--- DO / EXIT / CYCLE (print odd numbers 1..9) ---'
  i = 0
  DO
    i = i + 1
    IF (i > 10) EXIT          ! leave the loop
    IF (MOD(i, 2) == 0) CYCLE ! skip even numbers
    PRINT *, '  odd i =', i
  END DO

  ! ------------------------------------------------------------------
  ! 4. Summation with DO
  ! ------------------------------------------------------------------
  n     = 100
  total = 0
  DO i = 1, n
    total = total + i
  END DO
  PRINT '(A,I4,A,I6)', ' Sum 1..',  n, ' = ', total

END PROGRAM loops

