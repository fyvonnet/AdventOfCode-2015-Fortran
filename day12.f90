program day12
    implicit none

    integer :: ios, sum1, sum2
    character :: c
    character(len=:), allocatable :: input
    integer :: input_length = 0
    integer :: i

    open(unit = 99, file = 'inputs/day12', action = 'read', position = 'rewind')

    do
        read(99, '(A)', advance = 'no', iostat = ios) c
        if (ios /= 0) exit
        input_length = input_length + 1
    end do

    allocate(character(len=input_length) :: input)
    rewind(99)
    read(99, *) input
    close(99)

    i    = 1
    sum1 = 0
    sum2 = compute_sums()

    print *, sum1
    print *, sum2

contains

    recursive integer function compute_sums() result(res)
        logical :: red, is_object
        integer :: subtotal, value, multiplier

        red = .false.
        subtotal = 0

        if (input(i:i) == '{') then
            is_object = .true.
        else
            is_object = .false.
        end if

        i = i + 1
        res = 0

        do
            if ((input(i:i) == '}') .or.  (input(i:i) == ']')) then
                i = i + 1
                exit
            else if ((input(i:i) == '{') .or.  (input(i:i) == '[')) then
                subtotal = subtotal + compute_sums()
            else if (input(i:i + 4) == '"red"') then
                red = .true.
                i = i + 5
            else if (is_numeric(input(i:i))) then
                value = 0
                multiplier = 1
                do while (is_numeric(input(i:i)))
                    if (input(i:i) == '-') then
                        multiplier = -1
                    else
                        value = (value * 10) + (iachar(input(i:i)) - 48)
                    end if
                    i = i + 1
                end do
                value = value * multiplier
                subtotal = subtotal + value
                sum1 = sum1 + value
            else
                i = i + 1
            end if
        end do

        if (red .and. is_object) then
            res = 0
        else
            res = subtotal
        end if
    end function compute_sums


    pure logical function is_numeric(c)
        character, intent(in) :: c
        is_numeric = ((c == '-') .or. ((iachar(c) >= 48) .and. (iachar(c) <= 57)))
    end function is_numeric

end program

