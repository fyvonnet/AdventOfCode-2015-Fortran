program day08
    implicit none

    character(len=80) :: code
    integer :: ios
    integer :: total1 = 0, total2 = 0
    integer :: code_len

    open(99, file='inputs/day08', action='read', position='rewind')

    do
        read(99, '(A)', iostat=ios) code
        if (ios /= 0) exit
        code_len = len_trim(code)
        total1 = total1 + code_len - count_string(code)
        total2 = total2 + len_trim(encode(code)) - code_len
    end do

    close(99)

    print *, total1
    print *, total2

contains

    pure function count_string(code) result(strlen)
        integer :: strlen
        character(len=*), intent(in) :: code
        integer :: i

        i = 2; strlen = 0

        do 
            select case (code(i:i))
            case ('\')
                select case (code(i+1:i+1))
                case ('x')
                    i = i + 3
                case default
                    i = i + 1
                end select
            case ('"')
                exit
            end select
            strlen = strlen + 1
            i = i + 1
        end do
    end function count_string


    pure function encode(code) result(newcode)
        character(len=*), intent(in) :: code
        character(len=80) :: newcode
        integer :: i, j

        newcode = '"'
        j = 2

        do i = 1, len_trim(code)
            select case (code(i:i))
            case ('\')
                newcode(j:j+1) = '\\'
                j = j + 2
            case ('"')
                newcode(j:j+1) = '\"'
                j = j + 2
            case default
                newcode(j:j) = code(i:i)
                j = j + 1
            end select
        end do

        newcode(j:j) = '"'
    end function encode


end program day08
