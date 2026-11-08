!*****************************************************************************************
!> author: Jacob Williams
!  license: MIT
!
!  Lexical analysis: converts an expression string into a list of tokens.
!
!### Supported tokens
!
!  * numbers: `1`, `1.0`, `.5`, `1e-3`, `2.5d0`, `1.5E+2`
!  * identifiers: a letter followed by letters, digits or underscores (case insensitive)
!  * arithmetic operators: `+`, `-`, `*`, `/`, `^` and `**`
!  * relational operators: `==`, `/=`, `!=`, `<`, `<=`, `>`, `>=`,
!    and the Fortran forms `.eq.`, `.ne.`, `.lt.`, `.le.`, `.gt.`, `.ge.`
!  * logical operators: `&&`, `||`, `!` and the Fortran forms `.and.`, `.or.`, `.not.`
!  * punctuation: `(`, `)`, `[`, `]`, `,`

module fparse_lexer

use, intrinsic :: iso_fortran_env, only: int64
use fparse_common

implicit none (type, external)

private

! token kinds:
integer, parameter, public :: tok_end    = 0  !! end of the expression
integer, parameter, public :: tok_number = 1  !! a numeric literal
integer, parameter, public :: tok_ident  = 2  !! an identifier
integer, parameter, public :: tok_plus   = 3  !! `+`
integer, parameter, public :: tok_minus  = 4  !! `-`
integer, parameter, public :: tok_mul    = 5  !! `*`
integer, parameter, public :: tok_div    = 6  !! `/`
integer, parameter, public :: tok_pow    = 7  !! `^` or `**`
integer, parameter, public :: tok_lparen = 8  !! `(`
integer, parameter, public :: tok_rparen = 9  !! `)`
integer, parameter, public :: tok_lbrack = 10 !! `[`
integer, parameter, public :: tok_rbrack = 11 !! `]`
integer, parameter, public :: tok_comma  = 12 !! `,`
integer, parameter, public :: tok_eq     = 13 !! `==` or `.eq.`
integer, parameter, public :: tok_ne     = 14 !! `/=`, `!=` or `.ne.`
integer, parameter, public :: tok_lt     = 15 !! `<` or `.lt.`
integer, parameter, public :: tok_le     = 16 !! `<=` or `.le.`
integer, parameter, public :: tok_gt     = 17 !! `>` or `.gt.`
integer, parameter, public :: tok_ge     = 18 !! `>=` or `.ge.`
integer, parameter, public :: tok_and    = 19 !! `&&` or `.and.`
integer, parameter, public :: tok_or     = 20 !! `||` or `.or.`
integer, parameter, public :: tok_not    = 21 !! `!` or `.not.`

type, public :: token_t
    !! A token from an expression string.
    integer :: kind = tok_end !! the token kind (`tok_*` parameters)
    real(wp) :: value = 0.0_wp !! the value (for numbers)
    integer :: pos = 0 !! position of the start of the token in the string
    integer :: last = -1 !! position of the end of the token in the string
                         !! (the token text is `str(pos:last)`)
end type token_t

type :: dot_op_t
    !! a Fortran-style dot operator
    character(len=5) :: text
    integer :: kind
end type dot_op_t

type(dot_op_t), dimension(*), parameter :: dot_ops = [ &
    dot_op_t('.and.', tok_and), dot_op_t('.or. ', tok_or), dot_op_t('.not.', tok_not), &
    dot_op_t('.eq. ', tok_eq),  dot_op_t('.ne. ', tok_ne), dot_op_t('.lt. ', tok_lt),  &
    dot_op_t('.le. ', tok_le),  dot_op_t('.gt. ', tok_gt), dot_op_t('.ge. ', tok_ge) ]

public :: tokenize

contains
!*****************************************************************************************

!*****************************************************************************************
!>
!  Convert an expression string into an array of tokens.
!  The last token is always of kind `tok_end` (the array may be larger
!  than the number of tokens).

subroutine tokenize(str, tokens, err)

    character(len=*), intent(in) :: str !! the expression string
    type(token_t), dimension(:), allocatable, intent(out) :: tokens !! the tokens
    type(error_t), intent(inout) :: err !! error status

    integer :: i, j, n, ntok, kind
    character(len=1) :: c, c2
    real(wp) :: val
    logical :: ok

    n = len_trim(str)
    allocate(tokens(n+1))
    ntok = 0
    i = 1

    do while (i <= n)

        c = str(i:i)
        if (c == ' ' .or. c == achar(9)) then
            i = i + 1
            cycle
        end if
        c2 = ' '
        if (i < n) c2 = str(i+1:i+1)
        j = i      ! end of token
        val = 0.0_wp

        if (is_digit(c) .or. (c == '.' .and. is_digit(c2))) then
            call lex_number(str(1:n), i, j, val, ok)
            if (.not. ok) then
                call err%set('invalid number "'//str(i:j)//'"', i)
                return
            end if
            kind = tok_number
        else if (is_letter(c)) then
            do while (j < n)
                if (.not. is_ident_char(str(j+1:j+1))) exit
                j = j + 1
            end do
            kind = tok_ident
        else if (c == '.') then
            call match_dot_op(str(1:n), i, kind, j)
            if (kind == 0) then
                call err%set('invalid character "."', i)
                return
            end if
        else
            select case (c)
            case ('+'); kind = tok_plus
            case ('-'); kind = tok_minus
            case ('^'); kind = tok_pow
            case ('('); kind = tok_lparen
            case (')'); kind = tok_rparen
            case ('['); kind = tok_lbrack
            case (']'); kind = tok_rbrack
            case (','); kind = tok_comma
            case ('*')
                if (c2 == '*') then
                    kind = tok_pow; j = i + 1
                else
                    kind = tok_mul
                end if
            case ('/')
                if (c2 == '=') then
                    kind = tok_ne; j = i + 1
                else
                    kind = tok_div
                end if
            case ('=')
                if (c2 == '=') then
                    kind = tok_eq; j = i + 1
                else
                    call err%set('unexpected "=" (use "==" for comparison)', i)
                    return
                end if
            case ('<')
                if (c2 == '=') then
                    kind = tok_le; j = i + 1
                else
                    kind = tok_lt
                end if
            case ('>')
                if (c2 == '=') then
                    kind = tok_ge; j = i + 1
                else
                    kind = tok_gt
                end if
            case ('!')
                if (c2 == '=') then
                    kind = tok_ne; j = i + 1
                else
                    kind = tok_not
                end if
            case ('&')
                if (c2 == '&') then
                    kind = tok_and; j = i + 1
                else
                    call err%set('unexpected "&" (use "&&" for logical and)', i)
                    return
                end if
            case ('|')
                if (c2 == '|') then
                    kind = tok_or; j = i + 1
                else
                    call err%set('unexpected "|" (use "||" for logical or)', i)
                    return
                end if
            case default
                call err%set('invalid character "'//c//'"', i)
                return
            end select
        end if

        ntok = ntok + 1
        tokens(ntok)%kind = kind
        tokens(ntok)%pos = i
        tokens(ntok)%last = j
        tokens(ntok)%value = val
        i = j + 1

    end do

    ntok = ntok + 1
    tokens(ntok)%kind = tok_end
    tokens(ntok)%pos = n + 1
    tokens(ntok)%last = n

end subroutine tokenize
!*****************************************************************************************

!*****************************************************************************************
!>
!  Lex a numeric literal starting at position `i`.
!  On output, `j` is the position of the last character of the number.

subroutine lex_number(str, i, j, val, ok)

    character(len=*), intent(in) :: str !! the expression string
    integer, intent(in) :: i !! start position
    integer, intent(out) :: j !! end position
    real(wp), intent(out) :: val !! the numeric value
    logical, intent(out) :: ok !! true if the number is valid

    integer :: k, n, kind, kend, ios
    character(len=:), allocatable :: numstr
    integer(int64) :: m
    integer :: nsig, nfrac, expo, esign, p10
    logical :: fast, seen_dot

    ! exact powers of ten (all are exactly representable):
    real(wp), dimension(0:22), parameter :: pow10 = [1.0e0_wp, 1.0e1_wp, 1.0e2_wp, 1.0e3_wp, 1.0e4_wp, &
                                                     1.0e5_wp, 1.0e6_wp, 1.0e7_wp, 1.0e8_wp, 1.0e9_wp, 1.0e10_wp, 1.0e11_wp, &
                                                     1.0e12_wp, 1.0e13_wp, &
                                                     1.0e14_wp, 1.0e15_wp, 1.0e16_wp, 1.0e17_wp, 1.0e18_wp, 1.0e19_wp, 1.0e20_wp, &
                                                     1.0e21_wp, 1.0e22_wp]

    n = len(str)
    k = i                ! next character to examine
    call skip_digits()
    if (k <= n) then
        if (str(k:k) == '.') then
            call match_dot_op(str, k, kind, kend)
            if (kind == 0) then      ! not something like `1.and.`
                k = k + 1
                call skip_digits()
            end if
        end if
    end if
    if (k <= n) then           ! exponent
        if (index('eEdD', str(k:k)) > 0) then
            kend = k + 1
            if (kend <= n) then
                if (str(kend:kend) == '+' .or. str(kend:kend) == '-') kend = kend + 1
            end if
            if (kend <= n) then
                if (is_digit(str(kend:kend))) then
                    k = kend
                    call skip_digits()
                end if
            end if
        end if
    end if
    j = k - 1

    ! Fast path, for numbers with at most 15 significant digits and a power of
    ! ten of magnitude at most 22: the digits (an integer < 2^53) and the power
    ! of ten are exact, so a single multiplication or division gives the
    ! correctly rounded value (the same as `read`).
    m = 0
    nsig = 0
    nfrac = 0
    expo = 0
    fast = .true.
    seen_dot = .false.
    k = i
    do while (k <= j)
        if (is_digit(str(k:k))) then
            if (seen_dot) nfrac = nfrac + 1
            if (nsig > 0 .or. str(k:k) /= '0') nsig = nsig + 1
            if (nsig > 15) then
                fast = .false.
                exit
            end if
            m = 10 * m + (iachar(str(k:k)) - iachar('0'))
        else if (str(k:k) == '.') then
            seen_dot = .true.
        else ! exponent
            k = k + 1
            esign = 1
            if (str(k:k) == '+' .or. str(k:k) == '-') then
                if (str(k:k) == '-') esign = -1
                k = k + 1
            end if
            do while (k <= j)
                expo = 10 * expo + (iachar(str(k:k)) - iachar('0'))
                if (expo > 1000) then
                    fast = .false.
                    exit
                end if
                k = k + 1
            end do
            expo = esign * expo
            exit
        end if
        k = k + 1
    end do
    p10 = expo - nfrac
    if (fast .and. abs(p10) <= 22) then
        if (p10 >= 0) then
            val = real(m, wp) * pow10(p10)
        else
            val = real(m, wp) / pow10(-p10)
        end if
        ok = .true.
        return
    end if

    ! otherwise, use the Fortran conversion:
    numstr = str(i:j)
    do k = 1, len(numstr)
        if (numstr(k:k) == 'd' .or. numstr(k:k) == 'D') numstr(k:k) = 'e'
    end do
    read(numstr, *, iostat=ios) val
    ok = ios == 0

contains

    subroutine skip_digits()
        do while (k <= n)
            if (.not. is_digit(str(k:k))) exit
            k = k + 1
        end do
    end subroutine skip_digits

end subroutine lex_number
!*****************************************************************************************

!*****************************************************************************************
!>
!  Check if a Fortran dot-operator (e.g. `.and.`) starts at position `i`.
!  Returns `kind=0` if there is no match.

pure subroutine match_dot_op(str, i, kind, j)

    character(len=*), intent(in) :: str !! the expression string
    integer, intent(in) :: i !! start position (of the `.`)
    integer, intent(out) :: kind !! the token kind (0 if not matched)
    integer, intent(out) :: j !! end position of the operator

    integer :: k, l

    kind = 0
    j = i
    do k = 1, size(dot_ops)
        l = len_trim(dot_ops(k)%text)
        if (i + l - 1 <= len(str)) then
            if (to_lower(str(i:i+l-1)) == dot_ops(k)%text(1:l)) then
                kind = dot_ops(k)%kind
                j = i + l - 1
                return
            end if
        end if
    end do

end subroutine match_dot_op
!*****************************************************************************************

pure logical function is_digit(c)
    character(len=1), intent(in) :: c !! the character
    is_digit = c >= '0' .and. c <= '9'
end function is_digit

pure logical function is_letter(c)
    character(len=1), intent(in) :: c !! the character
    is_letter = (c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z')
end function is_letter

pure logical function is_ident_char(c)
    character(len=1), intent(in) :: c !! the character
    is_ident_char = is_letter(c) .or. is_digit(c) .or. c == '_'
end function is_ident_char

!*****************************************************************************************
end module fparse_lexer
!*****************************************************************************************
