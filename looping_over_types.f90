program looping_over_types

    implicit none

    integer, parameter :: dp  = kind(1.d0)
    integer, parameter :: sp  = kind(1.0)
    integer, parameter :: wp = dp           ! working precision

    type test_class_arrays
        integer :: nx, ny, n
        real(wp), allocatable :: var1(:,:,:)
        real(wp), allocatable :: var2(:,:,:)
        real(wp), allocatable :: var3(:,:,:)
        real(wp), allocatable :: var4(:,:,:)
        real(wp), allocatable :: var5(:,:,:)
        real(wp), allocatable :: var6(:,:,:)
    end type

    type test_class_cols
        integer :: n
        real(wp), allocatable :: var1(:)
        real(wp), allocatable :: var2(:)
        real(wp), allocatable :: var3(:)
        real(wp), allocatable :: var4(:)
        real(wp), allocatable :: var5(:)
        real(wp), allocatable :: var6(:)
    end type

    type test_class_pts
        real(wp) :: var1
        real(wp) :: var2
        real(wp) :: var3
        real(wp) :: var4
        real(wp) :: var5
        real(wp) :: var6
    end type

    integer :: i, j, k, q

    integer, parameter :: nx   = 36
    integer, parameter :: ny   = 72
    integer, parameter :: nmax = 100

    integer, parameter :: niter = 200

    type(test_class_arrays) :: tta
    type(test_class_cols), allocatable :: ttc2D(:,:)
    type(test_class_pts),  allocatable :: ttp3D(:,:,:)
    
    real(8) :: start_time
    real(8) :: end_time
    real(8) :: dtime_tta
    real(8) :: dtime_ttc
    real(8) :: dtime_ttp

    ! Allocate test derived type (arrays)
    call test_class_arrays_alloc(tta,nx,ny,nmax)

    ! Allocate test derived type (2D array of derived types)
    allocate(ttc2D(nx,ny))
    do j = 1, ny 
    do i = 1, nx 
        call test_class_cols_alloc(ttc2D(i,j),nmax)
    end do
    end do 

    ! Allocate test derived type (3D array of derived types)
    allocate(ttp3D(nx,ny,nmax))

    ! ==== Now perform some computation tests ============================

    ! Test using standard arrays for loops 

    call cpu_time(start_time)

    do q = 1, niter
        call test_class_arrays_update(tta)
    end do 

    call cpu_time(end_time)
    dtime_tta = (end_time - start_time)

    ! Test using derived type columns

    call cpu_time(start_time)

    do q = 1, niter

        do j = 1, ny 
        do i = 1, nx
            call test_class_cols_update(ttc2D(i,j))
        end do
        end do 

    end do

    call cpu_time(end_time)
    dtime_ttc = (end_time - start_time)
    
    ! Test using derived type points

    call cpu_time(start_time)

    do q = 1, niter

        do k = 1, nmax
        do j = 1, ny 
        do i = 1, nx
            call test_class_pts_update(ttp3D(i,j,k))
        end do
        end do 
        end do 

    end do

    call cpu_time(end_time)
    dtime_ttp = (end_time - start_time)

    write(*,"(a,a8,a1,g14.3,a2)") "Time using: ", "arrays",  ":", dtime_tta, "s"
    write(*,"(a,a8,a1,g14.3,a2)") "Time using: ", "columns", ":", dtime_ttc, "s"
    write(*,"(a,a8,a1,g14.3,a2)") "Time using: ", "points",  ":", dtime_ttp, "s"

contains

    subroutine compute_something(var1,var2,var3,var4,var5,var6)

        implicit none

        real(wp), intent(INOUT) :: var1
        real(wp), intent(INOUT) :: var2
        real(wp), intent(INOUT) :: var3
        real(wp), intent(INOUT) :: var4
        real(wp), intent(INOUT) :: var5
        real(wp), intent(INOUT) :: var6
        
        call random_number(var1)
        call random_number(var2)
        call random_number(var3)
        var4 = var2/var1 + var3
        var5 = var4/var3 + var1
        var6 = var5-var4+var3-var2+var1

        return

    end subroutine compute_something

    subroutine test_class_arrays_update(tt)
        
        implicit none

        type(test_class_arrays), intent(INOUT) :: tt
        
        ! Local variables
        integer :: i, j, k 

        do k = 1, tt%n
        do j = 1, tt%ny
        do i = 1, tt%nx 
            call compute_something(tt%var1(i,j,k),tt%var2(i,j,k),tt%var3(i,j,k), &
                                        tt%var4(i,j,k),tt%var5(i,j,k),tt%var6(i,j,k))
        end do 
        end do
        end do

        return

    end subroutine test_class_arrays_update

    subroutine test_class_cols_update(tt)
        
        implicit none

        type(test_class_cols), intent(INOUT) :: tt

        ! Local variables
        integer :: k 

        do k = 1, tt%n 
            call compute_something(tt%var1(k),tt%var2(k),tt%var3(k), &
                                        tt%var4(k),tt%var5(k),tt%var6(k))
        end do

        return

    end subroutine test_class_cols_update

    subroutine test_class_pts_update(tt)
        
        implicit none

        type(test_class_pts), intent(INOUT) :: tt

 
        call compute_something(tt%var1,tt%var2,tt%var3, &
                                    tt%var4,tt%var5,tt%var6)

        return

    end subroutine test_class_pts_update

    subroutine test_class_arrays_alloc(tt,nx,ny,n)
        
        implicit none

        type(test_class_arrays), intent(INOUT) :: tt 
        integer, intent(IN) :: nx
        integer, intent(IN) :: ny
        integer, intent(IN) :: n 

        if (allocated(tt%var1)) deallocate(tt%var1)
        if (allocated(tt%var2)) deallocate(tt%var2)
        if (allocated(tt%var3)) deallocate(tt%var3)
        if (allocated(tt%var4)) deallocate(tt%var4)
        if (allocated(tt%var5)) deallocate(tt%var5)
        if (allocated(tt%var6)) deallocate(tt%var6)
        
        allocate(tt%var1(nx,ny,n))
        allocate(tt%var2(nx,ny,n))
        allocate(tt%var3(nx,ny,n))
        allocate(tt%var4(nx,ny,n))
        allocate(tt%var5(nx,ny,n))
        allocate(tt%var6(nx,ny,n))
        
        tt%nx = nx
        tt%ny = ny
        tt%n = n 

        return

    end subroutine test_class_arrays_alloc


    subroutine test_class_cols_alloc(tt,n)
        
        implicit none

        type(test_class_cols), intent(INOUT) :: tt 
        integer, intent(IN) :: n 

        if (allocated(tt%var1)) deallocate(tt%var1)
        if (allocated(tt%var2)) deallocate(tt%var2)
        if (allocated(tt%var3)) deallocate(tt%var3)
        if (allocated(tt%var4)) deallocate(tt%var4)
        if (allocated(tt%var5)) deallocate(tt%var5)
        if (allocated(tt%var6)) deallocate(tt%var6)
        
        allocate(tt%var1(n))
        allocate(tt%var2(n))
        allocate(tt%var3(n))
        allocate(tt%var4(n))
        allocate(tt%var5(n))
        allocate(tt%var6(n))
        
        tt%n = n 

        return

    end subroutine test_class_cols_alloc

end program looping_over_types

    