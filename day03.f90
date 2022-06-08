program day03
    implicit none
    integer :: xmin = 0
    integer :: xmax = 0
    integer :: ymin = 0
    integer :: ymax = 0
    integer :: counter1 = 1
    integer :: counter2 = 1
    integer :: x = 0, y = 0
    integer, target :: sx = 0, sy = 0
    integer, target :: rx = 0, ry = 0
    logical, allocatable :: houses(:,:,:)
    integer :: ios
    character :: c
    integer, pointer :: px, py
    logical :: santa = .true.

    open(unit=99, file='inputs/day03', action = 'read', position = 'rewind')

    do
        read(99, '(A)', advance='no', iostat=ios) c
        if (ios /= 0) exit
        select case (c)
        case ('<')
            x = x - 1
            if (x < xmin) xmin = x
        case ('>')
            x = x + 1
            if (x > xmax) xmax = x
        case ('^')
            y = y + 1
            if (y > ymax) ymax = y
        case ('v')
            y = y - 1
            if (y < ymin) ymin = y
        case default
            print *, 'invalid char: ' // c
        end select
    end do

    rewind(99)

    allocate(houses(xmin*2:xmax*2, ymin*2:ymax*2, 2))
    x = 0; y = 0
    houses = .true.
    houses(0,0,:) = .false.

    do
        read(99, '(A)', advance='no', iostat=ios) c
        if (ios /= 0) exit
        if (santa) then
            px => sx
            py => sy
            santa = .false.
        else
            px => rx
            py => ry
            santa = .true.
        end if
        select case (c)
        case ('<')
            px = px - 1
            x = x - 1
        case ('>')
            px = px + 1
            x = x + 1
        case ('^')
            py = py + 1
            y = y + 1
        case ('v')
            py = py - 1
            y = y - 1
        case default
            print *, 'invalid char: ' // c
        end select
        if (houses(x,y,1)) then
            houses(x,y,1) = .false.
            counter1 = counter1 + 1
        end if
        if (houses(px,py,2)) then
            houses(px,py,2) = .false.
            counter2 = counter2 + 1
        end if
    end do

    print *, counter1
    print *, counter2

end program

