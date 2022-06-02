program day07
    implicit none

    type input
        integer :: val
        logical :: has_signal
    end type input

    type gate
        integer     :: type
        integer     :: signal
        logical     :: two_inputs
        type(input) :: input1, input2
        integer     :: output
    end type gate

    character (len=30)     :: str, a, b, c, d, e
    integer                :: ios
    integer                :: input_length = 0
    type(gate),allocatable :: gates(:)
    integer                :: i
    type(input)            :: in
    integer                :: signal_a
    integer, parameter     :: UNKNOWN  = -1
    integer, parameter     :: NONE     =  0
    integer, parameter     :: NOT      =  1
    integer, parameter     :: AND      =  2
    integer, parameter     :: OR       =  3
    integer, parameter     :: LSHIFT   =  4
    integer, parameter     :: RSHIFT   =  5

    open(unit=99, file='inputs/day07', action='read', position = 'rewind')

    do
        read(99, '(A)', iostat=ios)
        if (ios /= 0) exit
        input_length = input_length + 1
    end do

    rewind(99)
    allocate(gates(input_length))

    do i = 1, input_length
        read(99, '(A)') str
        read(str, *) a, b, c
        if (a == 'NOT') then
            read(str, *) a, b, c, d
            gates(i)%type = NOT
            gates(i)%input1 = decode_input(b)
            gates(i)%two_inputs = .false.
            gates(i)%output = djb2(d)
        else if (b == '->') then
            gates(i)%type = NONE
            gates(i)%input1 = decode_input(a)
            gates(i)%two_inputs = .false.
            gates(i)%output = djb2(c)
        else
            read(str, *) a, b, c, d, e
            gates(i)%input1 = decode_input(a)
            gates(i)%input2 = decode_input(c)
            gates(i)%output = djb2(e)
            gates(i)%two_inputs = .true.
            select case (b)
            case ('AND')
                gates(i)%type = AND
            case ('OR')
                gates(i)%type = OR
            case ('LSHIFT')
                gates(i)%type = LSHIFT
            case ('RSHIFT')
                gates(i)%type = RSHIFT
            end select
        end if
        gates(i)%signal = UNKNOWN
    end do

    close(99)
    call quicksort(gates, 1, input_length)

    signal_a = execute_gate(djb2('a'))
    print *, signal_a

    do i = 1, input_length
        if (gates(i)%output == djb2('b')) then
            gates(i)%signal = signal_a
        else
            gates(i)%signal = UNKNOWN
        end if
    end do

    print *, execute_gate(djb2('a'))


contains


    recursive function execute_gate(output) result(signal)
        implicit none
        integer, intent(in) :: output
        integer :: signal
        integer :: i, j, a, b
        integer :: low, high

        high = input_length
        low  = 1

        do
            i = low + (high - low) / 2
            if (output < gates(i)%output) then
                high = i - 1
            else if (output > gates(i)%output) then
                low = i + 1
            else
                exit
            end if
        end do

        if (gates(i)%signal /= UNKNOWN) then
            signal = gates(i)%signal
        else
            a = evaluate_input(gates(i)%input1)
            if (gates(i)%two_inputs) b = evaluate_input(gates(i)%input2)

            select case (gates(i)%type)
            case(NONE)
                signal = a
            case(NOT)
                signal = logic_not(a)
            case(AND)
                signal = iand(a, b)
            case(OR)
                signal = ior(a, b)
            case(LSHIFT)
                signal = a
                do j = 1, b
                    signal = signal * 2
                end do
            case(RSHIFT)
                signal = a
                do j = 1, b
                    signal = signal / 2
                end do
            end select
            gates(i)%signal = signal
        end if
    end function execute_gate


    recursive function evaluate_input(in) result(signal)
        implicit none
        type(input), intent(in) :: in
        integer :: signal

        if (in%has_signal) then
            signal = in%val
        else
            signal = execute_gate(in%val)
        end if
    end function evaluate_input


    pure function decode_input(str)
        implicit none
        character(len=*), intent(in) :: str
        type(input) :: decode_input

        if ((str(1:1) .ge. '0').and.(str(1:1).le.'9')) then
            decode_input%has_signal = .true.
            read(str, *) decode_input%val
        else
            decode_input%has_signal = .false.
            decode_input%val = djb2(str)
        end if
    end function decode_input


    pure function logic_not(n)
        implicit none
        integer, intent(in) :: n
        integer :: m
        integer :: logic_not, i
        logical :: a(16)

        a = .true.
        logic_not = 0
        m = n

        do i = 1, 16
            if (m == 0) exit
            a(i) = (modulo(m, 2) == 0)
            m = m / 2
        end do

        do i = 16, 1, -1
            logic_not = logic_not * 2
            if (a(i)) logic_not = logic_not + 1
        end do
    end function logic_not


    pure function djb2(str)
        implicit none
        character(len=*), intent(in) :: str
        integer :: djb2
        integer :: i

        djb2 = 5381

        do i = 1, len_trim(str)
            djb2 = djb2 * 33 + iachar(str(i:i))
        end do
    end function


    subroutine swap(A, b, c)
        type(gate), intent(inout) :: A(*)
        integer, intent(in) :: b, c
        type(gate) :: tmp

        tmp = A(b)
        A(b) = A(c)
        A(c) = tmp
    end subroutine swap


    subroutine partition(A, p, q, r)
        type(gate), intent(inout) :: A(*)
        integer, intent(in) :: p, r
        integer, intent(inout) :: q
        type(gate) :: x, tmp
        integer :: i, j

        x = A(r)
        i = p - 1
        do j = p, r - 1
            if (A(j)%output .le. x%output) then
                i = i + 1
                call swap(A, i, j)
            end if
        end do
        call swap(A, i + 1, r)
        q = i + 1
    end subroutine partition


    recursive subroutine quicksort(A, p, r)
        type(gate), intent(inout) :: A(*)
        integer, intent(in) :: p, r
        integer :: q
        if (p .lt. r) then
            call partition(A, p, q, r)
            call quicksort(A, p, q - 1)
            call quicksort(A, q + 1, r)
        end if
    end subroutine quicksort

end program day07

