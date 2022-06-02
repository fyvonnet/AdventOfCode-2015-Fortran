program day10
    implicit none

    character :: c
    integer :: n = 0
    integer :: last_i = -1
    integer :: t, i, j, ios, seq_len
    integer, allocatable :: sequence(:), new_sequence(:)

    open(unit=99, file='inputs/day10', action='read', position='rewind')

    seq_len = 0

    do
        read(99, '(a)', advance='no', iostat=ios) c
        if (ios /= 0) exit
        seq_len = seq_len + 1
    end do

    allocate(sequence(seq_len))
    rewind(99)

    do i = 1, seq_len
        read(99, '(a)', advance='no', iostat=ios) c
        sequence(i) = iachar(c) - 48
    end do

    close(99)

    do t = 1, 50
        i = 2
        j = 1
        n = 1
        last_i = sequence(1)
        allocate(new_sequence(seq_len * 2))
        do i = 2, seq_len
            if (last_i == sequence(i)) then
                n = n + 1
            else
                new_sequence(j) = n
                new_sequence(j + 1) = last_i
                j = j + 2
                last_i = sequence(i)
                n = 1
            end if
        end do
        new_sequence(j) = n
        new_sequence(j + 1) = last_i
        seq_len = j + 1
        deallocate(sequence)
        allocate(sequence(seq_len))
        sequence = new_sequence(:seq_len)
        deallocate(new_sequence)
        if (t == 40) print *, seq_len
    end do

    print *, seq_len

end program day10

