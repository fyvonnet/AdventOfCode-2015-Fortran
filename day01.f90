program day01
    implicit none

    character :: buffer
    integer :: floor = 0
    integer :: char = 0
    integer :: ans2 = 0

    open(unit=99, file='inputs/day01', status="old", action="read")
    do
        read(99, "(A)", advance='no', end=1) buffer
        char = char + 1
        if (buffer == '(') then
            floor = floor + 1
        else
            floor = floor - 1
            if ((floor == -1).and.(ans2 == 0)) then
                ans2 = char
            end if
        end if
    end do

    1 close(99)

    print *, floor
    print *, ans2

end program day01

