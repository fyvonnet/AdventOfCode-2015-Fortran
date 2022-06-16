program day18
    implicit none
    integer :: grid(100,100), grid_new(100,100)
    character(len=100) :: line
    integer :: c, x, y, xx, yy
    integer :: count

    open(unit=99, file='inputs/day18', action='read', position='rewind')

    do y = 1, 100
        read(99, *) line
        do x = 1, 100
            select case (line(x:x))
            case('.')
                grid(x,y) = 0
            case('#')
                grid(x,y) = 1
            case default
                print *, 'Unknown character: ' // line(x:x)
                stop 1
            end select
        end do
    end do

    do c = 1, 100
        do y = 1, 100
            do x = 1, 100
                count = 0
                do yy = y - 1, y + 1
                    do xx = x - 1, x + 1
                        !if ((xx >= 1) .and. (yy >= 1) .and. (xx <= 100) .and. (yy <= 100) .and. (yy /= y) .and. (xx /= x)) then
                        if ((xx >= 1) .and. (yy >= 1) .and. (xx <= 100) .and. (yy <= 100)) then
                            count = count + grid(xx, yy)
                        end if
                    end do
                end do
                count = count - grid(x, y)
                select case (grid(x, y))
                case(1)
                    if ((count == 2) .or. (count == 3)) then
                        grid_new(x, y) = 1
                    else
                        grid_new(x, y) = 0
                    end if
                case(0)
                    if (count == 3) then
                        grid_new(x, y) = 1
                    else
                        grid_new(x, y) = 0
                    end if
                end select
            end do
        end do
        grid = grid_new
    end do

    print *, sum(grid)

end program day18

