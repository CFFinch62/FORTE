! conditionals.f90 — Branching and selection in Fortran.
!
! Demonstrates: IF / ELSE IF / ELSE, logical operators,
!               SELECT CASE for integers and character ranges.
!
! Compile:  gfortran -Wall conditionals.f90 -o conditionals
! Run:      ./conditionals

PROGRAM conditionals

  IMPLICIT NONE

  INTEGER           :: score, day_num
  REAL              :: temp
  CHARACTER(LEN=1)  :: grade

  ! ------------------------------------------------------------------
  ! 1. Simple IF / ELSE IF / ELSE
  ! ------------------------------------------------------------------
  temp = 37.5   ! body temperature in Celsius

  IF (temp < 36.0) THEN
    PRINT *, 'Hypothermia risk — temperature too low.'
  ELSE IF (temp <= 37.5) THEN
    PRINT *, 'Normal body temperature.'
  ELSE IF (temp <= 38.5) THEN
    PRINT *, 'Mild fever.'
  ELSE
    PRINT *, 'High fever — see a doctor!'
  END IF

  ! ------------------------------------------------------------------
  ! 2. Logical operators: .AND. .OR. .NOT.
  ! ------------------------------------------------------------------
  score = 78

  IF (score >= 90) THEN
    grade = 'A'
  ELSE IF (score >= 80) THEN
    grade = 'B'
  ELSE IF (score >= 70) THEN
    grade = 'C'
  ELSE IF (score >= 60) THEN
    grade = 'D'
  ELSE
    grade = 'F'
  END IF

  PRINT '(A,I3,A,A)', ' Score: ', score, '  Grade: ', grade

  IF (score >= 60 .AND. score < 80) THEN
    PRINT *, 'Passing but room to improve.'
  END IF

  ! ------------------------------------------------------------------
  ! 3. SELECT CASE — clean multi-way branch on an integer
  ! ------------------------------------------------------------------
  PRINT *, ''
  PRINT *, '--- Day of the week ---'
  DO day_num = 1, 7
    SELECT CASE (day_num)
    CASE (1)
      PRINT *, 'Monday'
    CASE (2)
      PRINT *, 'Tuesday'
    CASE (3)
      PRINT *, 'Wednesday'
    CASE (4)
      PRINT *, 'Thursday'
    CASE (5)
      PRINT *, 'Friday'
    CASE (6, 7)
      PRINT *, 'Weekend!'
    CASE DEFAULT
      PRINT *, 'Unknown day'
    END SELECT
  END DO

END PROGRAM conditionals

