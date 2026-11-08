!*****************************************************************************************
!> author: Jacob Williams
!  license: MIT
!
!  A hash table for looking up names.
!
!  The table doesn't store the names themselves: it stores indices into an array
!  of names that is owned by the caller (and passed to each call), so that nothing
!  is copied. It uses open addressing with linear probing, and the FNV-1a hash.

module fparse_hash

use, intrinsic :: iso_fortran_env, only: int64
use fparse_common

implicit none (type, external)

private

type, public :: name_table
    !! A hash table of indices into an array of names.
    integer, dimension(:), allocatable :: slots !! the indices (0 for an empty slot).
                                                !! The size is a power of two.
    integer :: n = 0 !! number of indices in the table
contains
    procedure, public :: init => table_init
    procedure, public :: insert => table_insert
    procedure, public :: find => table_find
end type name_table

contains
!*****************************************************************************************

!*****************************************************************************************
!>
!  Initialize an empty table, with room for `n` names without being resized.

pure subroutine table_init(me, n)

    class(name_table), intent(inout) :: me !! the table
    integer, intent(in) :: n !! the expected number of names

    integer :: cap

    cap = 16
    do while (cap < 2 * n)
        cap = 2 * cap
    end do
    if (allocated(me%slots)) deallocate(me%slots)
    allocate(me%slots(cap), source=0)
    me%n = 0

end subroutine table_init
!*****************************************************************************************

!*****************************************************************************************
!>
!  Add the index of a name to the table. The name must not already be in the table.

pure subroutine table_insert(me, names, idx)

    class(name_table), intent(inout) :: me !! the table
    type(string_t), dimension(:), intent(in) :: names !! the names
    integer, intent(in) :: idx !! the index of the name to add

    integer, dimension(:), allocatable :: old
    integer :: k

    if (.not. allocated(me%slots)) call me%init(1)
    if (2 * (me%n + 1) > size(me%slots)) then
        ! resize (keep the load factor at most 1/2):
        call move_alloc(me%slots, old)
        allocate(me%slots(2 * size(old)), source=0)
        do k = 1, size(old)
            if (old(k) /= 0) call add_index(me%slots, names, old(k))
        end do
    end if
    call add_index(me%slots, names, idx)
    me%n = me%n + 1

end subroutine table_insert
!*****************************************************************************************

!*****************************************************************************************
!>
!  Put the index of a name in the first empty slot for its hash.

pure subroutine add_index(slots, names, i)

    integer, dimension(:), intent(inout) :: slots !! the slots of the table
    type(string_t), dimension(:), intent(in) :: names !! the names
    integer, intent(in) :: i !! the index of the name

    integer :: s, mask

    mask = size(slots) - 1
    s = iand(hash(names(i)%str), mask)
    do while (slots(s + 1) /= 0)
        s = iand(s + 1, mask)
    end do
    slots(s + 1) = i

end subroutine add_index
!*****************************************************************************************

!*****************************************************************************************
!>
!  Find a name. Returns its index in `names`, or 0 if it is not in the table.

pure integer function table_find(me, names, name) result(idx)

    class(name_table), intent(in) :: me !! the table
    type(string_t), dimension(:), intent(in) :: names !! the names
    character(len=*), intent(in) :: name !! the name to find

    integer :: s, mask

    idx = 0
    if (.not. allocated(me%slots)) return
    mask = size(me%slots) - 1
    s = iand(hash(name), mask)
    do while (me%slots(s + 1) /= 0)
        if (len(names(me%slots(s + 1))%str) == len(name)) then
            if (names(me%slots(s + 1))%str == name) then
                idx = me%slots(s + 1)
                return
            end if
        end if
        s = iand(s + 1, mask)
    end do

end function table_find
!*****************************************************************************************

!*****************************************************************************************
!>
!  The FNV-1a hash of a string (32 bits).

pure integer function hash(str) result(h)

    character(len=*), intent(in) :: str !! the string

    integer(int64), parameter :: offset_basis = 2166136261_int64
    integer(int64), parameter :: prime = 16777619_int64
    integer(int64), parameter :: mask32 = 4294967295_int64
    integer(int64) :: h64
    integer :: i

    h64 = offset_basis
    do i = 1, len(str)
        h64 = ieor(h64, int(iachar(str(i:i)), int64))
        h64 = iand(h64 * prime, mask32)
    end do
    h = int(iand(h64, 2147483647_int64)) ! (non-negative)

end function hash
!*****************************************************************************************

!*****************************************************************************************
end module fparse_hash
!*****************************************************************************************
