program day05
    implicit none

    character(len=16) :: str
    integer :: ios
    integer :: counter1 = 0
    integer :: counter2 = 0

    open(unit=99, file='inputs/day05', action='read', position='rewind')

    do
        read(99,*,iostat=ios) str
        if (ios /= 0) exit
        if (has_three_vowels(str) .and. has_pair(str) .and. (.not.(has_forbidden_string(str)))) counter1 = counter1 + 1
        if (has_double_pair(str) .and. has_repeat(str)) counter2 = counter2 + 1
    end do

    close(99)

    print *, counter1
    print *, counter2

contains

    pure logical function is_vowel(c)
        implicit none
        character :: c
        character(len=5), parameter :: vowels='aeiou'
        integer :: i

        is_vowel = .false.

        do i = 1, 5
            if (c == vowels(i:i)) then
                is_vowel = .true.
                exit
            end if
        end do

    end function is_vowel


    pure logical function has_three_vowels(str)
        implicit none
        character(len=16) :: str
        integer :: counter = 0
        integer :: i

        has_three_vowels = .false.
        counter = 0

        do i = 1, 16
            if (is_vowel(str(i:i))) then
                counter = counter + 1
                if (counter == 3) then
                    has_three_vowels = .true.
                    exit
                end if
            end if
        end do
    end function


    pure logical function has_pair(str)
        implicit none
        character(len=16) :: str
        integer :: i

        has_pair = .false.

        do i = 1, 15
            if (str(i:i) == str(i+1:i+1)) then
                has_pair = .true.
                exit
            end if
        end do
    end function has_pair


    pure logical function is_forbidden_string(str)
        implicit none
        character(len=2) :: str
        integer :: i
        character(len=2) :: forbidden_strings(4) = ['ab', 'cd', 'pq', 'xy']
        is_forbidden_string = .false.

        do i = 1, 4
            if (str == forbidden_strings(i)) then
                is_forbidden_string = .true.
                exit
            end if
        end do
    end function is_forbidden_string



    pure logical function has_forbidden_string(str)
        implicit none
        character(len=16) :: str
        integer :: i
        has_forbidden_string = .false.

        do i = 1, 15
            if (is_forbidden_string(str(i:i+1))) then
                has_forbidden_string = .true.
                exit
            end if
        end do
    end function has_forbidden_string

    pure logical function has_double_pair(str)
        implicit none
        character(len=16) :: str
        integer :: i
        has_double_pair = .false.

        do i = 1, 13
            if (index(str(i+2:16), str(i:i+1)) /= 0) then
                has_double_pair = .true.
                exit
            end if
        end do
    end function has_double_pair

    pure logical function has_repeat(str)
        implicit none
        character(len=16) :: str
        integer :: i
        has_repeat = .false.

        do i = 1, 14
            if (str(i:i) == str(i+2:i+2)) then
                has_repeat = .true.
                exit
            end if
        end do
    end function has_repeat

end program day05

