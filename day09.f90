program day09
    use day09_mheap

    implicit none

    integer, parameter :: heap_length = 5000

    type distance 
        character(len=15) :: from, to
        integer :: distance
    end type distance

    type heap_elm
        integer :: distance
        integer :: remain
        integer :: node
        logical, allocatable :: not_visited(:)
    end type heap_elm

    type heap
        integer :: heap_size ! 0
        type(heap_elm),allocatable :: data(:)
    end type

    character(len=50) :: str
    character(len=15) :: a, b, c, d
    character(len=15), allocatable :: node_names(:)
    integer :: e
    integer :: ios, input_length = 0
    integer :: nnodes
    integer :: i, j, x, y
    type(distance), allocatable :: input(:)
    type(integer), allocatable :: dist_matrix(:,:)
    type(heap) :: h
    type(heap_elm) :: new_elm, elm

    logical :: part2 = .false.

    open(99, file='inputs/day09', action='read', position='rewind')

    do
        read(99, '(A)', iostat=ios)
        if (ios /= 0) exit
        input_length = input_length + 1
    end do

    nnodes = ceiling(sqrt(2.0 * input_length))

    allocate(input(input_length))
    allocate(dist_matrix(nnodes, nnodes))
    allocate(node_names(nnodes))
    dist_matrix = 0

    rewind(99)

    do i = 1, input_length
        read(99, '(A)') str
        read(str, *) a, b, c, d, e
        input(i) = distance(a, c, e)
    end do

    close(99)

    i = 1
    do j = 1, nnodes - 1
        node_names(j) = input(i)%from
        i = i + nnodes - j
    end do
    node_names(nnodes) = input(input_length)%to

    do i = 1, input_length
        x = get_node_num(node_names, input(i)%from)
        y = get_node_num(node_names, input(i)%to)
        dist_matrix(x, y) = input(i)%distance
        dist_matrix(y, x) = input(i)%distance
    end do

    allocate(h%data(heap_length))
    allocate(new_elm%not_visited(nnodes))

    part2 = .false.
    call search_route()

    part2 = .true.
    call search_route()

contains

    subroutine search_route()
        implicit none
        h%heap_size = 0
        do i= 1, nnodes
            new_elm%distance = 0
            new_elm%node = i
            new_elm%remain = 7
            new_elm%not_visited = .true.
            new_elm%not_visited(i) = .false.
            call max_heap_insert(h, new_elm)
        end do

        do
            elm = heap_extract_max(h)
            if (elm % remain == 0) exit

            do i = 1, nnodes
                if (elm%not_visited(i)) then
                    new_elm%distance = elm%distance + dist_matrix(i, elm%node)
                    new_elm%node = i
                    new_elm%not_visited = elm%not_visited
                    new_elm%not_visited(i) = .false.
                    new_elm%remain = elm%remain - 1
                    call max_heap_insert(h, new_elm)
                end if
            end do
        end do

        print *, elm%distance
    end subroutine search_route


    pure function get_node_num(node_names, name) result(i)
        implicit none
        integer :: i
        character(len=*), intent(in) :: node_names(:)
        character(len=*), intent(in) :: name

        do i = 1, size(node_names)
            if (node_names(i) == name) exit
        end do
    end function get_node_num


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
        logical :: x
        x = a%distance < b%distance
        greater = xor(part2, x) ! XOR in controlled NOT
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
        A%data(A%heap_size)%distance = 0
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


end program day09

