! modules.f90 — The MODULE / USE pattern in Fortran.
!
! Modules are the primary way to share constants, types, and
! procedures between program units — Fortran's equivalent of
! a Python module or a C header + source pair.
!
! Compile:  gfortran -Wall modules.f90 -o modules
! Run:      ./modules

! ------------------------------------------------------------------
! MODULE definition — put this before any PROGRAM that uses it
! ------------------------------------------------------------------
MODULE math_constants

  IMPLICIT NONE

  ! Named constants available to any program that does USE math_constants
  REAL, PARAMETER :: PI      = 3.14159265358979
  REAL, PARAMETER :: E_EULER = 2.71828182845905
  REAL, PARAMETER :: PHI     = 1.61803398874989   ! golden ratio

CONTAINS

  ! A pure function inside the module
  PURE REAL FUNCTION circle_area(radius)
    REAL, INTENT(IN) :: radius
    circle_area = PI * radius ** 2
  END FUNCTION circle_area

  PURE REAL FUNCTION deg_to_rad(degrees)
    REAL, INTENT(IN) :: degrees
    deg_to_rad = degrees * PI / 180.0
  END FUNCTION deg_to_rad

END MODULE math_constants


! ------------------------------------------------------------------
! MODULE with a derived type (struct-like)
! ------------------------------------------------------------------
MODULE geometry

  IMPLICIT NONE

  TYPE :: Point2D
    REAL :: x, y
  END TYPE Point2D

CONTAINS

  REAL FUNCTION distance(p1, p2)
    TYPE(Point2D), INTENT(IN) :: p1, p2
    distance = SQRT((p2%x - p1%x)**2 + (p2%y - p1%y)**2)
  END FUNCTION distance

END MODULE geometry


! ------------------------------------------------------------------
! Main program — uses both modules
! ------------------------------------------------------------------
PROGRAM modules

  USE math_constants   ! bring in PI, E_EULER, circle_area, deg_to_rad
  USE geometry         ! bring in Point2D, distance

  IMPLICIT NONE

  REAL        :: r, angle_deg
  TYPE(Point2D) :: origin, pt

  ! Use module constants
  PRINT '(A,F18.14)', ' PI      = ', PI
  PRINT '(A,F18.14)', ' e       = ', E_EULER
  PRINT '(A,F18.14)', ' phi     = ', PHI

  ! Use module functions
  r = 5.0
  PRINT '(A,F5.1,A,F12.6)', ' Area of circle r=', r, ' : ', circle_area(r)

  angle_deg = 45.0
  PRINT '(A,F5.1,A,F12.8)', ' 45 degrees in radians : ', deg_to_rad(angle_deg)

  ! Use derived type and module function
  origin = Point2D(0.0, 0.0)
  pt     = Point2D(3.0, 4.0)
  PRINT '(A,F6.3)', ' Distance from origin to (3,4) = ', distance(origin, pt)

END PROGRAM modules

