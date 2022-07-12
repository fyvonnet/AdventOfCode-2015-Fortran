program day20
    implicit none
    integer, parameter :: input = 34000000
    integer, parameter :: input_sqrt = floor(sqrt(1.0 * input))
    integer :: i, j, count
    logical, dimension(2:input_sqrt) :: is_prime = .true.
    integer, allocatable :: primes(:)

    i = 2
    count = 0
    do !while (i < input_sqrt)
        do while (.not.(is_prime(i)))
            i = i + 1
            if (i == input_sqrt) goto 10
        end do
        do j = i * 2, input_sqrt, i
            is_prime(j) = .false.
        end do
        i = i + 1
        count = count + 1
    end do

    10 allocate(primes(count))

    j = 1
    do i = 2, input_sqrt
        if (is_prime(i)) then
            primes(j) = i
            j = j + 1
        end if
    end do

    i = 1
    do while (sumDivisors(i) * 10 < input)
        i = i + 1
    end do

    print *, i

contains

    integer function sumDivisors(n)
        implicit none
        integer, intent(in) :: n
        integer :: i

        sumDivisors = 0

        if (n == 1) then
            sumDivisors = 1
            return
        end if

        do i = 2, floor(sqrt(1.0 * n))
            if (modulo(n, i) == 0) then
                if (i == n / i) then
                    sumDivisors = sumDivisors + i
                else
                    sumDivisors = sumDivisors + (i + n / i)
                end if
            end if
        end do

        sumDivisors = sumDivisors + 1 + n

    end function sumDivisors

end program day20






