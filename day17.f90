program day17
    implicit none

    type queue_elem
        integer :: liters, i, ncontainers
        type(queue_elem), pointer :: next
    end type queue_elem

    integer :: input_length, ios, i
    integer, allocatable :: input(:)
    type(queue_elem), pointer :: queue_head, queue_tail
    type(queue_elem), pointer :: list_head, list_tail
    integer :: count, mincont

    type(queue_elem) :: elem

    nullify(queue_head)
    nullify(queue_tail)
    nullify(list_head)
    nullify(list_tail)

    open(unit=99, file='inputs/day17', action='read', position='rewind')

    input_length = 0

    do 
        read(99, *, iostat=ios)
        if (ios /= 0) exit
        input_length = input_length + 1
    end do

    rewind(99)
    allocate(input(input_length))

    do i = 1, input_length
        read(99, *) input(i)
        call push(queue_head, queue_tail, input(i), i, 1)
    end do

    close(99)

    count = 0
    mincont = input_length
    do while (associated(queue_head))
        elem = pop(queue_head)
        if (elem%liters == 150) then
            mincont = min(mincont, elem%ncontainers)
            count = count + 1
            call push(list_head, list_tail, 0, 0, elem%ncontainers)
        else if (elem%liters < 150) then
            do i = elem%i + 1, input_length
                call push(queue_head, queue_tail, elem%liters + input(i), i, elem%ncontainers + 1)
            end do
        end if
    end do

    print *, count

    count = 0
    do while (associated(list_head))
        elem = pop(list_head)
        if (elem%ncontainers == mincont) count = count + 1
    end do

    print *, count

contains

    subroutine push(head, tail, l, i, n)
        type(queue_elem), pointer, intent(inout) :: head, tail
        integer, intent(in) :: l, i, n

        if (associated(head)) then
            allocate(tail%next)
            tail => tail%next
            tail%liters = l
            tail%i = i
            tail%ncontainers = n
            nullify(tail%next)
        else
            allocate(head)
            head%liters = l
            head%i = i
            head%ncontainers = n
            nullify(head%next)
            tail => head
        end if
    end subroutine push

    type(queue_elem) function pop(head)
        type(queue_elem), pointer, intent(inout) :: head
        type(queue_elem), pointer :: ptr

        pop = head
        ptr => head%next
        deallocate(head)
        head => ptr
    end function pop

end program day17

