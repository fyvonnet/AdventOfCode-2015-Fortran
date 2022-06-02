program day11
    implicit none

    character(len=8) :: input = 'hepxcrrq'
    integer :: string(8)
    integer :: i, found = 0

    do i = 1, 8
        string(i) = iachar(input(i:i)) - 97
    end do

    do
        call increase(string, 8)
        if ((has_two_pairs(string)) .and. (has_straight_increase(string))) then
            do i = 1, 8
                input(i:i) = achar(string(i) + 97)
            end do
            print *, input
            found = found + 1
            if (found == 2) exit
        end if
    end do

contains

    recursive subroutine increase(A, n)
        integer, intent(inout) :: A(8)
        integer, intent(in) :: n

        if (A(n) == 25) then
            A(n) = 0
            call increase(A, n - 1)
        else if ((A(n) == 7) .or. (A(n) == 13) .or. (A(n) == 10)) then
            A(n) = A(n) + 2
        else
            A(n) = A(n) + 1
        end if
    end subroutine increase


    pure logical function has_straight_increase(str)
        implicit none
        integer, intent(in) :: str(8)
        integer :: i

        has_straight_increase = .false.

        do i = 1, 6
            if ((str(i + 1) == str(i) + 1) .and. (str(i + 2) == str(i + 1) + 1)) then
                has_straight_increase = .true.
                exit
            end if
        end do
    end function has_straight_increase


    pure logical function has_two_pairs(str)
        implicit none
        integer, intent(in) :: str(8)
        integer :: i, n

        n = 0
        i = 1

        do 
            if (i > 7) then
                has_two_pairs = .false.
                exit
            end if

            if (str(i) == str(i + 1)) then
                i = i + 1
                n = n + 1
                if (n == 2) then
                    has_two_pairs = .true.
                    exit
                end if
            end if
            i = i + 1
        end do
    end function has_two_pairs

end program day11

