program day04
    use sdf_md5, only:sdf_md5_generate
    implicit none

    character(len=8), parameter :: input = "bgvyzdsv"
    character(len=32) :: hash
    character(len=15) :: str
    integer :: n = 1

    write(str, '(i0)') 609043
    
    do
        write(str, '(i0)') n
        hash = sdf_md5_generate(input // trim(str))
        !if (hash(:5) == "00000") exit
        if (hash(:6) == "000000") exit
        n = n + 1
    end do

    print *, n
        
end program
