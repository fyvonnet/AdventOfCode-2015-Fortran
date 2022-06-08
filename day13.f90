program day13
    implicit none

    integer, parameter :: heap_length = 50000

    type input
        character(len=10) :: name1, name2
        integer :: units
    end type input

    type heap_elm
        integer :: distance
        integer :: remain
        integer :: node
        integer :: first_node
        logical, allocatable :: not_visited(:)
    end type heap_elm

    type heap
        integer :: heap_size ! 0
        type(heap_elm),allocatable :: data(:)
    end type

    integer :: input_length, ios, i, j, k, a, b, multiplier
    character(len=70), allocatable :: line
    integer :: nnodes
    integer, allocatable :: matrix(:,:)
    character(len=10), allocatable :: names(:)
    character(len=10) :: name
    type(input), allocatable :: guests(:)
    type(heap) :: h

    input_length = 0
    open(unit=99, file='inputs/day13', action='read', position='rewind')
    !open(unit=99, file='inputs/day13-test', action='read', position='rewind')
    do
        read(99, *, iostat=ios)
        if (ios /= 0) exit
        input_length = input_length + 1
    end do

    rewind(99)

    !nnodes = ceiling((sqrt(4.0 * input_length + 1.0) + 1) / 2)
    allocate(guests(input_length))

    do i = 1, input_length
        line = ''
        read(99, '(A)', iostat=ios) line
        j = 1
        name = ''
        guests(i)%name1 = ''
        guests(i)%name2 = ''
        do
            if (line(j:j) == ' ') exit
            guests(i)%name1(j:j) = line(j:j)
            j = j + 1
        end do

        j = j + 7
        if (line(j:j) == 'g') then
            multiplier = 1
        else
            multiplier = -1
        end if

        j = j + 5
        guests(i)%units = 0
        a = iachar(line(j:j))
        do while ((a >= 48) .and. (a <= 57))
            guests(i)%units = guests(i)%units * 10 + a - 48
            j = j + 1
            a = iachar(line(j:j))
        end do
        guests(i)%units = guests(i)%units * multiplier

        j = j + 36
        k = 1
        do while (line(j:j) /= '.')
            guests(i)%name2(k:k) = line(j:j)
            j = j + 1; k = k + 1
        end do
    end do

    close(99)

    nnodes = 2
    do i = 2, input_length
        if (guests(i)%name1 /= guests(1)%name1) exit
        nnodes = nnodes + 1
    end do

    allocate(matrix(nnodes, nnodes))
    allocate(names(nnodes))

    names = ''

    j = 1
    do i = 1, nnodes
        names(i) = guests(j)%name1
        j = j + (nnodes - 1)
    end do

    matrix = 0

    do i = 1, input_length
        a = find_name_id(guests(i)%name1, names)
        b = find_name_id(guests(i)%name2, names)

        matrix(a, b) = matrix(a, b) + guests(i)%units
        matrix(b, a) = matrix(b, a) + guests(i)%units

        !matrix(find_name_id(guests(i)%name1, names), find_name_id(guests(i)%name2, names)) = guests(i)%units
    end do

    allocate(h%data(heap_length))

    call search_route()

contains

    pure integer function find_name_id(name, array) result(i)
        character(len=10), intent(in) :: name
        character(len=10), intent(in) :: array(:)

        do i = 1, size(array)
            if (array(i) == name) exit
        end do

    end function find_name_id


    subroutine search_route()
        implicit none
        type(heap_elm) :: new_elm, elm

        allocate(new_elm%not_visited(nnodes))

        h%heap_size = 0
        do i= 1, nnodes
            new_elm%distance = 0
            new_elm%node = i
            new_elm%remain = nnodes - 1
            new_elm%first_node = i
            new_elm%not_visited = .true.
            new_elm%not_visited(i) = .false.
            call max_heap_insert(h, new_elm)
        end do

        do
            elm = heap_extract_max(h)
            if (elm%remain == -1) then
                exit
            else if (elm%remain == 0) then 
                new_elm%distance = elm%distance + matrix(elm%node, elm%first_node)
                new_elm%remain = -1
                call max_heap_insert(h, new_elm)
            else

                do i = 1, nnodes
                    if (elm%not_visited(i)) then
                        new_elm%distance = elm%distance + matrix(i, elm%node)
                        new_elm%node = i
                        new_elm%first_node = elm%first_node
                        new_elm%not_visited = elm%not_visited
                        new_elm%not_visited(i) = .false.
                        new_elm%remain = elm%remain - 1
                        call max_heap_insert(h, new_elm)
                    end if
                end do
            end if
        end do

        print *, elm%distance
    end subroutine search_route


    pure function parent(i)
        implicit none
        integer :: parent
        integer,intent(in) :: i
        parent = floor(i / 2.0)
    end function parent

    pure function left(i)
        implicit none
        integer :: left
        integer,intent(in) :: i
        left = 2 * i
    end function left

    pure function right(i)
        implicit none
        integer :: right
        integer,intent(in) :: i
        right = 2 * i + 1
    end function right

    subroutine swap_heap_elm(a, b)
        implicit none
        type(heap_elm), intent(inout) :: a, b
        type(heap_elm) :: tmp
        tmp = a; a = b; b = tmp
    end subroutine swap_heap_elm

    logical function greater(a, b)
        implicit none
        type(heap_elm), intent(in) :: a, b
        if (a%remain /= b%remain) then
            greater = a%remain > b%remain
        else
            greater = a%distance > b%distance
        end if
    end function greater

    recursive subroutine max_heapify(A, i)
        implicit none
        integer, intent(in) :: i
        type(heap), intent(inout) :: A
        integer :: l, r
        integer :: largest


        l = left(i); r = right(i)
        largest = i

        if (l <= A%heap_size) then
            if (greater (A%data(l), A%data(i))) largest = l
        end if

        if (r <= A%heap_size) then
            if (greater(A%data(r), A%data(largest))) largest = r
        end if

        if (largest /= i) then
            call swap_heap_elm(A%data(i), A%data(largest))
            call max_heapify(A, largest)
        end if
    end subroutine max_heapify


    function heap_extract_max(A) result(max)
        implicit none
        type(heap), intent(inout) :: A
        type(heap_elm) :: max

        if (A%heap_size < 1) stop "heap underflow"

        max = A%data(1)
        A%data(1) = A%data(A%heap_size)
        A%heap_size = A%heap_size - 1
        call max_heapify(A, 1)
    end function heap_extract_max


    subroutine max_heap_insert(A, key)
        implicit none
        type(heap), intent(inout) :: A
        type(heap_elm), intent(in) :: key

        if (A%heap_size == size(A%data)) stop "heap overflow"

        A%heap_size = A%heap_size + 1
        A%data(A%heap_size)%distance = -999
        call heap_increase_key(A, A%heap_size, key)
    end subroutine max_heap_insert


    subroutine heap_increase_key(A, i, key)
        implicit none
        type(heap), intent(inout) :: A
        integer, intent(in) :: i
        type(heap_elm), intent(in) :: key
        integer :: j

        j = i

        if (key%distance < A%data(i)%distance) stop "new key is smaller than current key"

        A%data(i) = key

        do
            if (j <= 1) exit
            if (greater(A%data(parent(j)), A%data(j))) exit
            call swap_heap_elm(A%data(parent(j)), A%data(j))
            j = parent(j)
        end do
    end subroutine heap_increase_key


end program day13

