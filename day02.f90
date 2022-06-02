program day02
    implicit none
    integer :: ios
    character(len=20) :: buf
    integer :: x_pos(2)
    integer :: i, j
    integer :: l, w, h
    integer :: a, b, c
    integer :: paper = 0, ribbon = 0

    open(unit=99, file='inputs/day02', action='read', position='rewind')

    do
        read(99, *, iostat=ios) buf
        if (ios /= 0) exit
        i = 1
        j = 1
        do
            if (buf(j:j) == "x") then
                x_pos(i) = j
                i = i + 1
            end if
            if (i == 3) exit
            j = j + 1
        end do
        read(buf(1:(x_pos(1) - 1)), *) l
        read(buf((x_pos(1) + 1):(x_pos(2) - 1)), *) w
        read(buf((x_pos(2) + 1):), *) h
        a = l * w
        b = w * h
        c = h * l
        paper = paper + a + a + b + b + c + c + min(a, b, c)
        if ((l >= w).and.(l >= h)) then
            a = w
            b = h
        else if ((w >= l).and.(w >= h)) then
            a = l
            b = h
        else
            a = l
            b = w
        end if
        ribbon = ribbon + a + a + b + b + l * w * h
    end do

    close(99)

    print *,paper
    print *,ribbon

end program

