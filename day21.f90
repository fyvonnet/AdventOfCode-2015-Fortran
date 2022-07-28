program day21
    implicit none

    type item
        integer :: c, d, a
    end type item

    character(len=10) :: desc
    integer :: cost, damage, armor
    integer :: i, j, k
    integer :: cost_min = 99999, cost_max = 0
    type(item) :: weapons(5), armors(6), rings(7)
    type(item) :: rings_combs(22)

    open(unit=99, file='equipment.dat', status='old', action='read')
    read(99, '(A)')

    do i = 1, 5
        read(99, *) desc, cost, damage, armor
        weapons(i)%c = cost
        weapons(i)%d = damage
        weapons(i)%a = armor
    end do

    read(99, '(A)')
    read(99, '(A)')

    armors(1)%c = 0
    armors(1)%d = 0
    armors(1)%a = 0

    do i = 2, 6
        read(99, *) desc, cost, damage, armor
        armors(i)%c = cost
        armors(i)%d = damage
        armors(i)%a = armor
    end do

    read(99, '(A)')
    read(99, '(A)')

    rings(1)%c = 0
    rings(1)%d = 0
    rings(1)%a = 0

    do i = 2, 7
        read(99, *) desc, desc, cost, damage, armor
        rings(i)%c = cost
        rings(i)%d = damage
        rings(i)%a = armor
    end do

    close(99)

    rings_combs(1)%c = 0
    rings_combs(1)%d = 0
    rings_combs(1)%a = 0

    i = 2
    do j = 1, 7
        do k = j + 1, 7
            if (j /= k) then
                rings_combs(i)%c = rings(j)%c + rings(k)%c
                rings_combs(i)%d = rings(j)%d + rings(k)%d
                rings_combs(i)%a = rings(j)%a + rings(k)%a
                i = i + 1
            end if
        end do
    end do

    do i = 1, 5
        do j = 1, 6
            do k = 1, 22
                cost   = weapons(i)%c + armors(j)%c + rings_combs(k)%c
                damage = weapons(i)%d + armors(j)%d + rings_combs(k)%d
                armor  = weapons(i)%a + armors(j)%a + rings_combs(k)%a

                if (player_wins(damage, armor)) then 
                    cost_min = min(cost_min, cost)
                else
                    cost_max = max(cost_max, cost)
                end if
            end do
        end do
    end do

    print *, cost_min
    print *, cost_max

contains

    logical function player_wins(player_dam, player_arm)
        implicit none
        integer :: player_dam, player_arm, player_hit
        integer :: boss_dam, boss_arm, boss_hit

        player_hit = 100
        boss_hit = 100
        boss_dam = 8
        boss_arm = 2

        do
            boss_hit = boss_hit - max(1, (player_dam - boss_arm))
            if (boss_hit <= 0) then
                player_wins = .true.
                return
            end if

            player_hit = player_hit - max(1, (boss_dam - player_arm))
            if (player_hit <= 0) then
                player_wins = .false.
                return
            end if
        end do
    end function player_wins

end program

