program day06
    implicit none
    integer :: ios
    character(len=40) :: str
    logical :: lamps1(0:999,0:999) = .false.
    integer :: lamps2(0:999,0:999) = 0
    integer :: x_start, x_end, y_start, y_end
    integer :: x, y
    logical :: switch, toggle
    integer :: start, l2
    character(len=7) :: through
    integer :: counter1 = 0
    integer :: counter2 = 0

    open(unit=99, file='inputs/day06', action='read', position='rewind')

    do
        read(99,'(A)',iostat=ios) str
        if (ios /= 0) exit
        select case (str(1:4))
        case ('turn')
            toggle = .false.
            select case (str(6:7))
            case ('on')
                start = 9
                switch = .true.
            case ('of')
                start = 10
                switch = .false.
            end select
        case ('togg')
            toggle = .true.
            start = 8
        end select

        read (str(start:), *) x_start, y_start, through, x_end, y_end

        do x = x_start, x_end
            do y = y_start, y_end
                l2 = lamps2(x,y)
                if (toggle) then
                    lamps1(x,y) = .not.lamps1(x,y)
                    lamps2(x,y) = l2 + 2
                else
                    lamps1(x,y) = switch
                    if (switch) then
                        lamps2(x,y) = l2 + 1
                    else
                        if (l2 > 0) lamps2(x,y) = l2 - 1
                    end if
                end if
            end do
        end do
    end do

    close(99)

    do x = 0, 999
        do y = 0, 999
            if (lamps1(x, y)) counter1 = counter1 + 1
            counter2 = counter2 + lamps2(x, y)
        end do
    end do

    print *, counter1
    print *, counter2

end program day06

