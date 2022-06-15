program day15
    !use subset

    implicit none
    !save
    integer :: i, j, k, input_length, ios
    character(len=80) :: line
    integer, allocatable :: input(:,:), spoons(:)
    integer :: maximum1, maximum2, properties(4), score
    logical :: more = .false.
    integer :: t, h

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

    close(99)

    maximum1 = 0
    maximum2 = 0

    do
        call nexcom(100, input_length, spoons, more, t, h)
        do j = 1, 4
            properties(j) = max(0, sum(input(:,j) * spoons))
        end do
        score = product(properties)
        maximum1 = max(maximum1, score)
        if (sum(spoons * input(:,5)) == 500) maximum2 = max(maximum2, score)
        if (.not. more) exit
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

    ! adapted from "Combinatorial Algorithms second edition"
    subroutine nexcom (n, k, r, mtc, t, h)
        implicit none
        integer, intent(in) :: n, k
        integer, intent(inout) :: r(:)
        logical, intent(inout) :: mtc
        integer, intent(inout) :: t, h
        integer :: i

        10 if (mtc) goto 20
        r(1) = n
        t = n
        h = 0
        if (k .eq. 1) goto 15
        do i = 2, k
            r(i) = 0
        end do
        15 mtc = r(k) .ne. n
        return
        20 if (t .gt. 1) h = 0
        30 h = h + 1
        t = r(h)
        r(h) = 0
        r(1) = t - 1
        r(h + 1) = r(h + 1) + 1
        goto 15
    end subroutine nexcom

end program day15

