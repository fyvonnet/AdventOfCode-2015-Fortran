program day15
    implicit none
    integer :: i, j, k, input_length, ios
    character(len=80) :: line
    integer, allocatable :: input(:,:), spoons(:)
    integer :: maximum1, maximum2, properties(4), score

    open(unit=99, file='inputs/day15', action='read', position='rewind')

    input_length = 0
    do
        read(99, '(A)', iostat=ios)
        if (ios /= 0) exit
        input_length = input_length + 1
    end do

    rewind(99)
    allocate(input(input_length, 5))
    allocate(spoons(input_length))

    do i = 1, input_length
        read(99, '(A)', iostat=ios) line
        j = len_trim(line) + 1; line(j:j) = ','
        j = 1
        do while (line(j:j) /= ' ')
            j = j + 1
        end do
        j = j + 1

        do k = 1, 5
            do while (line(j:j) /= ' ')
                j = j + 1
            end do
            j = j + 1
            call read_int(line, j, input(i, k))
        end do
    end do

    maximum1 = 0
    maximum2 = 0

    do i = 0, (100 ** input_length) - 1
        k = i
        do j = 1, input_length
            spoons(j) = mod(k, 100)
            k = k / 100
        end do
        if (sum(spoons) == 100) then
            do j = 1, 4
                properties(j) = max(0, sum(input(:,j) * spoons))
            end do
            score = product(properties)
            maximum1 = max(maximum1, score)
            if (sum(spoons * input(:,5)) == 500) maximum2 = max(maximum2, score)
        end if
    end do

    print *, maximum1
    print *, maximum2

contains

    subroutine read_int(line, i, n)
        implicit none
        character(len=80), intent(in) :: line
        integer, intent(inout) :: i
        integer, intent(inout) :: n
        integer :: j, multiplier

        if (line(i:i) == '-') then
            multiplier = -1
            i = i + 1
        else
            multiplier = 1
        end if

        n = 0; j = 1

        do
            if (line(i:i) == ',') exit
            n = n * 10 + iachar(line(i:i)) - 48
            i = i + 1; j = j + 1
        end do

        n = n * multiplier
        i = i + 2 ! skip space after comma
    end subroutine read_int


end program day15

