# Fortran Tutorial — FORTE IDE

This tutorial teaches modern Fortran (Fortran 90/95) using the FORTE IDE. By the end you will understand Fortran's key strengths — array computing, numerical precision, and modular code — and be able to write, compile, and run scientific programs.

---

## Getting Started

Open FORTE (`./run.sh`). Press **`Ctrl+N`** to create a new file (`.f90`), or open any file in the `examples/` folder from the file browser. Press **`Ctrl+R`** to compile and run.

---

## Lesson 1 — Hello, World!

Create `hello.f90`:

```fortran
program Hello
  implicit none
  print *, "Hello, World!"
end program Hello
```

Press `Ctrl+R`.

**What you learned:**
- `program Name` / `end program Name` bracket the main code
- `implicit none` — required in modern Fortran; forces explicit declarations
- `print *` prints to stdout with default formatting

---

## Lesson 2 — Variables and Types

```fortran
program Variables
  implicit none

  integer            :: count = 0
  real               :: pi    = 3.14159
  double precision   :: e     = 2.718281828459045d0
  character(len=20)  :: name  = "Ada Lovelace"
  logical            :: flag  = .true.

  print *, "Count:  ", count
  print *, "Pi:     ", pi
  print *, "e:      ", e
  print *, "Name:   ", name
  print *, "Flag:   ", flag
end program Variables
```

**Type keywords:**
`integer` `real` `double precision` `complex` `character` `logical`

**Kind parameters** (for portability):
```fortran
use iso_fortran_env
integer, parameter :: dp = real64
real(dp) :: x = 1.0_dp
```

---

## Lesson 3 — Reading Input

```fortran
program Greet
  implicit none
  character(len=50) :: name
  integer :: age

  write(*, "(A)", advance="no") "Enter your name: "
  read *, name
  write(*, "(A)", advance="no") "Enter your age:  "
  read *, age

  print *, "Hello, ", trim(name), "! You are", age, "years old."
end program Greet
```

---

## Lesson 4 — Arithmetic and Operators

```fortran
program Arithmetic
  implicit none
  real :: a = 10.0, b = 3.0, result

  result = a + b;  print *, "Add:        ", result
  result = a - b;  print *, "Subtract:   ", result
  result = a * b;  print *, "Multiply:   ", result
  result = a / b;  print *, "Divide:     ", result
  result = a ** b; print *, "Power (a^b):", result

  print *, "Integer div:", 10 / 3     ! truncates: 3
  print *, "Modulo:     ", mod(10, 3) ! remainder: 1
end program Arithmetic
```

---

## Lesson 5 — Conditionals

```fortran
program Conditionals
  implicit none
  integer :: score
  print *, "Enter score:"
  read *, score

  if (score >= 90) then
    print *, "A"
  else if (score >= 80) then
    print *, "B"
  else if (score >= 70) then
    print *, "C"
  else
    print *, "F"
  end if

  ! SELECT CASE
  select case (score / 10)
    case (10, 9)
      print *, "Excellent"
    case (8)
      print *, "Good"
    case default
      print *, "Needs improvement"
  end select
end program Conditionals
```

---

## Lesson 6 — Loops

```fortran
program Loops
  implicit none
  integer :: i, total

  ! Counted DO loop
  do i = 1, 5
    print *, "i =", i
  end do

  ! Step by 2
  do i = 1, 10, 2
    print *, "odd:", i
  end do

  ! DO WHILE equivalent
  total = 0
  do while (total < 50)
    total = total + 10
  end do
  print *, "Total:", total

  ! Loop control
  do i = 1, 10
    if (i == 4) cycle    ! skip
    if (i == 7) exit     ! break
    print *, i
  end do
end program Loops
```

---

## Lesson 7 — Arrays

Fortran's array operations are first-class:

```fortran
program Arrays
  implicit none
  integer :: a(5) = [1, 2, 3, 4, 5]
  real    :: b(3,3)
  integer :: i, j

  print *, "Sum:    ", sum(a)
  print *, "Max:    ", maxval(a)
  print *, "Min:    ", minval(a)
  print *, "a*2:    ", a * 2          ! element-wise multiply

  ! Slices
  print *, "a(2:4): ", a(2:4)

  ! 2-D array
  do i = 1, 3
    do j = 1, 3
      b(i,j) = real(i*j)
    end do
  end do
  print *, "Trace:  ", b(1,1) + b(2,2) + b(3,3)
end program Arrays
```

> **Fortran arrays are 1-indexed by default.** Column-major storage: `b(row, col)`.

---

## Lesson 8 — Subroutines and Functions

```fortran
module MathUtil
  implicit none
contains

  function square(n) result(sq)
    integer, intent(in) :: n
    integer :: sq
    sq = n * n
  end function

  recursive function factorial(n) result(f)
    integer, intent(in) :: n
    integer :: f
    if (n <= 1) then
      f = 1
    else
      f = n * factorial(n-1)
    end if
  end function

  subroutine swap(a, b)
    integer, intent(inout) :: a, b
    integer :: temp
    temp = a; a = b; b = temp
  end subroutine

end module MathUtil

program Main
  use MathUtil
  implicit none
  integer :: x = 3, y = 7
  print *, square(4), factorial(6)
  call swap(x, y)
  print *, x, y     ! 7 3
end program Main
```

---

## Lesson 9 — File I/O

```fortran
program FileIO
  implicit none
  integer :: unit_no = 10, i, val
  logical :: exists

  ! Write
  open(unit_no, file="numbers.txt", status="replace")
  do i = 1, 5
    write(unit_no, *) i*i
  end do
  close(unit_no)

  ! Read
  open(unit_no, file="numbers.txt", status="old")
  do i = 1, 5
    read(unit_no, *) val
    print *, "Read:", val
  end do
  close(unit_no)
end program FileIO
```

---

## Next Steps

- Explore the `modules.f90` and `arrays.f90` examples in the `examples/` folder.
- Read the [Fortran Language Reference](LANGUAGE-REFERENCE.md).
- Learn about `allocatable` arrays, derived types, and `use iso_fortran_env` for portable numeric kinds.
