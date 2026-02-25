# Fortran Language Reference — FORTE IDE

A concise reference for modern Fortran (Fortran 90/95/2003) as used in the FORTE IDE with gfortran.

---

## Program Structure

```fortran
program ProgramName
  use ModuleName            ! Import module
  implicit none             ! Mandatory in modern Fortran
  ! Declarations
  ! Executable statements
contains
  ! Internal procedures
end program ProgramName
```

---

## Data Types

### Intrinsic Types
| Type             | Keyword            | Literal examples    |
| ---------------- | ------------------ | ------------------- |
| Integer          | `integer`          | `42`, `-7`, `0`     |
| Real             | `real`             | `3.14`, `1.0e-3`    |
| Double precision | `double precision` | `3.14d0`, `1.0d-10` |
| Complex          | `complex`          | `(1.0, 2.5)`        |
| Logical          | `logical`          | `.true.`, `.false.` |
| Character        | `character`        | `"text"`, `'hello'` |

### Kind Parameters (portable)
```fortran
use iso_fortran_env
integer, parameter :: i32 = int32, i64 = int64
integer, parameter :: sp  = real32, dp = real64
real(dp)   :: x = 1.0_dp
integer(i64) :: bignum
```

### Type Declarations
```fortran
integer             :: n, m = 10
real                :: x, y = 0.0
character(len=50)   :: name
character(len=*)    :: msg      ! deferred-length (dummy argument)
logical             :: flag = .false.
```

---

## Variable Attributes

```fortran
integer, parameter :: MAX = 100     ! constant
integer, save      :: count         ! retains value between calls
integer, intent(in)  :: n           ! read-only argument
integer, intent(out) :: result      ! write-only argument
integer, intent(inout) :: x         ! read-write argument
integer, allocatable :: arr(:)      ! dynamic array
integer, pointer   :: ptr           ! pointer
integer, target    :: data          ! can be pointed to
```

---

## Arrays

```fortran
! Static
real :: a(10)               ! 1-indexed by default
real :: b(0:9)              ! 0-indexed
real :: c(3,3)              ! 2-D (column-major storage)
integer :: d(2:5)           ! custom bounds

! Array constructor
a = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
a = [(real(i), i = 1, 10)]  ! implied-do constructor

! Allocatable
real, allocatable :: v(:)
allocate(v(n))
deallocate(v)

! Slices
a(2:5)            ! elements 2 through 5
a(1:10:2)         ! every other element (stride 2)
c(1,:)            ! first row
c(:,2)            ! second column
```

### Array Intrinsics
| Function                  | Description                       |
| ------------------------- | --------------------------------- |
| `size(a)` / `size(a,dim)` | Total elements / size along dim   |
| `shape(a)`                | Shape as integer array            |
| `sum(a)`                  | Sum of all elements               |
| `product(a)`              | Product of all elements           |
| `maxval(a)` / `minval(a)` | Max / min element                 |
| `maxloc(a)` / `minloc(a)` | Location of max / min             |
| `transpose(a)`            | Transpose a 2-D array             |
| `matmul(a,b)`             | Matrix multiplication             |
| `dot_product(a,b)`        | Dot product                       |
| `spread(a,dim,n)`         | Replicate array along dimension   |
| `pack(a,mask)`            | Pack array elements matching mask |
| `reshape(a,shape)`        | Reshape array                     |

---

## Operators

### Arithmetic: `+` `-` `*` `/` `**`
### Comparison: `==` `/=` `<` `>` `<=` `>=`
### Logical: `.and.` `.or.` `.not.` `.eqv.` `.neqv.`
### String: `//` (concatenation)

---

## Control Flow

```fortran
! IF
if (x > 0) then
  print *, "positive"
else if (x < 0) then
  print *, "negative"
else
  print *, "zero"
end if

! One-liner
if (x < 0) x = -x

! SELECT CASE
select case (n)
  case (1)
    print *, "one"
  case (2:5)
    print *, "two to five"
  case default
    print *, "other"
end select

! DO loop
do i = 1, n
  ...
end do

do i = n, 1, -1    ! countdown
  ...
end do

! DO WHILE (legacy, prefer DO with EXIT)
do while (condition)
  ...
end do

! Infinite loop with exit
do
  if (done) exit
  ...
  if (skip) cycle
end do
```

---

## Procedures

```fortran
! Subroutine
subroutine swap(a, b)
  integer, intent(inout) :: a, b
  integer :: temp
  temp = a; a = b; b = temp
end subroutine

! Function
function area(r) result(a)
  real, intent(in) :: r
  real :: a
  a = 3.14159 * r * r
end function

! Recursive
recursive function fact(n) result(f)
  integer, intent(in) :: n
  integer :: f
  if (n <= 1) then; f = 1; else; f = n * fact(n-1); end if
end function

! Calling
call swap(x, y)
y = area(5.0)
```

---

## Modules

```fortran
module Constants
  implicit none
  real, parameter :: PI = 3.14159265358979d0
  real, parameter :: E  = 2.71828182845905d0
contains
  function deg_to_rad(d) result(r)
    real, intent(in) :: d
    real :: r
    r = d * PI / 180.0
  end function
end module Constants

program Main
  use Constants
  implicit none
  print *, deg_to_rad(90.0)
end program Main
```

---

## I/O

```fortran
! Print to stdout
print *, x, y            ! default format
write(*, *) x, y         ! same
write(*, "(F8.2)") x     ! formatted

! Read from stdin
read *, x
read(*, "(I4)") n

! File I/O
open(10, file="out.txt", status="replace")
write(10, *) "Hello"
close(10)

open(10, file="in.txt", status="old", iostat=ierr)
if (ierr /= 0) stop "Cannot open file"
read(10, *, iostat=ierr) val
close(10)
```

### Common Format Descriptors
| Descriptor | Meaning                          |
| ---------- | -------------------------------- |
| `I4`       | Integer, 4-wide                  |
| `F8.2`     | Float, width 8, 2 decimal places |
| `E12.4`    | Scientific notation              |
| `A20`      | Character, 20-wide               |
| `L2`       | Logical                          |
| `X`        | Skip one space                   |
| `/`        | Newline                          |
| `3(I4)`    | Repeat 3 times                   |

---

## Key Intrinsic Functions

### Math
`abs` `sqrt` `exp` `log` `log10` `sin` `cos` `tan` `asin` `acos` `atan` `atan2` `sinh` `cosh` `tanh` `floor` `ceiling` `nint` `int` `real` `dble` `mod` `modulo` `sign` `min` `max`

### Character
| Function       | Description                    |
| -------------- | ------------------------------ |
| `len(s)`       | Length of string               |
| `len_trim(s)`  | Length without trailing blanks |
| `trim(s)`      | Remove trailing blanks         |
| `adjustl(s)`   | Left-justify string            |
| `adjustr(s)`   | Right-justify string           |
| `index(s,sub)` | Find sub in s                  |
| `char(n)`      | Character from ASCII code      |
| `ichar(c)`     | ASCII code of character        |

---

## Derived Types

```fortran
type :: Point2D
  real :: x, y
end type Point2D

type(Point2D) :: p = Point2D(3.0, 4.0)
print *, p%x, p%y
```

---

*GNU Fortran documentation: [gcc.gnu.org/onlinedocs/gfortran](https://gcc.gnu.org/onlinedocs/gfortran/)*
*Fortran-lang: [fortran-lang.org](https://fortran-lang.org/)*
