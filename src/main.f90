  module BModule

    implicit none
    private
    public :: BType
  
    type :: BType
      integer(kind=4), pointer :: ipos => null()
      integer(kind=4), pointer :: nval2 => null()
      real(kind=8), dimension(:), pointer :: dval1 => null()
    contains
      procedure :: init
      procedure :: dealloc
    end type BType

    contains

      subroutine init(this, ipos, nval2)
        class(BType) :: this
        integer(kind=4), intent(in) :: ipos
        integer(kind=4), intent(in) :: nval2
        !local
        integer(kind=4) :: i
        write(*,*) 'in BType_init ', ipos
        !
        ! -- allocate scalars and arrays
        allocate(this%ipos)
        allocate(this%nval2)
        allocate(this%dval1(nval2))
        !
        ! -- initialize scalars and arrays
        this%ipos = ipos
        this%nval2 = nval2
        do i = 1, nval2
          this%dval1(i) = 0.0d0
        end do
      end subroutine init
  
      subroutine dealloc(this)
        class(BType) :: this
        write(*,*) 'in BType_da ', this%ipos
        !
        ! -- deallocate
        deallocate(this%ipos)
        deallocate(this%nval2)
        deallocate(this%dval1)
        !
        ! -- return
        return
      end subroutine dealloc
  end module BModule
  
  module AModule
  
    use BModule

    implicit none

    type bcontainer
      class(BType), pointer :: obj
    end type bcontainer

    private
    public :: AType

    type:: AType

      type(bcontainer), pointer, dimension(:) :: elements => null()
      type(BType), pointer  :: bobj => null()
      !
      ! -- AType data
      integer(kind=4), pointer :: nval1 => null()
      integer(kind=4), pointer :: nval2 => null()
      integer(kind=4), dimension(:), pointer :: dval1 => null()

      contains
  
        procedure :: a_allocate_arrays
        procedure :: a_ar
        procedure :: a_da
        !
        ! -- methods specific for AType
        procedure, private :: a_allocate_scalars
        procedure, private :: a_rd

    end type AType

    contains

      subroutine a_ar(this)
        ! -- dummy
        class(AType), intent(inout) :: this
        ! -- local
        integer(kind=4) :: i
        type(BType), pointer :: bobj     
        !
        ! -- Allocate scalars
        call this%a_allocate_scalars()
        !
        ! -- initialize nval1
        this%nval1 = 10
        this%nval2 = 20
        !
        ! -- Allocate arrays in package superclass
        call this%a_allocate_arrays()
        !
        ! -- Allocate objects
        allocate(this%elements(this%nval1))
        do i = 1, this%nval1
          allocate(bobj)
          this%elements(i)%obj => bobj
        enddo
        !
        ! -- Initialize each object
        do i = 1, this%nval1
            this%bobj => this%elements(i)%obj
            call this%bobj%init(i,this%nval2)
        end do
        !
        ! -- call rd
        call this%a_rd()
        !
        ! -- return
        return
      end subroutine a_ar

      subroutine a_allocate_scalars(this)
        ! -- dummy
        class(AType) :: this

        write(*,*) 'in a_allocate_scalars'
        !
        ! -- allocate scalars
        allocate(this%nval1)
        allocate(this%nval2)
        !
        ! -- initialize scalars
        this%nval1 = 0
        this%nval2 = 0
        !
        ! -- return
        return
      end subroutine a_allocate_scalars

      subroutine a_allocate_arrays(this)
        ! -- dummy
        class(AType),   intent(inout) :: this
        ! -- local
        integer (kind=4) :: i

        write(*,*) 'in a_allocate_arrays'
        !
        ! -- allocate arrays
        allocate(this%dval1(this%nval1))
        !
        ! -- initialize arrays
        do i = 1, this%nval1
          this%dval1(i) = 0.0d0
        end do
        !
        ! -- return
        return
      end subroutine a_allocate_arrays

      subroutine a_rd(this)
        ! -- dummy
        class(AType) :: this
        ! -- local
        integer(kind=4) :: n
        integer(kind=4), dimension(:), allocatable :: rowmaxnnz
        integer(kind=4), dimension(:), allocatable :: nboundchk

        write(*,*) 'in a_rd'
        !
        ! -- allocate space for node counter and initilize
        allocate(rowmaxnnz(12))
        do n = 1, 12
          rowmaxnnz(n) = 0
        end do
        !
        ! -- allocate space for local variables
        allocate(nboundchk(this%nval1))
        do n = 1, this%nval1
          nboundchk(n) = 0
        end do
        !
        ! -- deallocate local variables
        deallocate(rowmaxnnz)
        deallocate(nboundchk)
        !
        ! -- return
        return
      end subroutine a_rd

      subroutine a_da(this)
        ! -- dummy
        class(AType) :: this
        ! -- locals
        integer (kind=4) :: i
        ! -- format

        write(*,*) 'in a_da'
        !
        ! -- deallocate uzf objects
        do i = 1, this%nval1+1
            this%bobj => this%elements(i)%obj
            call this%bobj%dealloc()
            nullify(this%elements(i)%obj)
        end do
        nullify(this%bobj)
        deallocate(this%elements)
        !
        ! -- deallocate scalars
        deallocate(this%nval1)
        deallocate(this%nval2)
        !
        ! -- deallocate arrays
        deallocate(this%dval1)
        !
        ! -- Return
        return
      end subroutine a_da

  end module AModule


  program main
    use AModule
    implicit none
    type(AType), pointer :: a => null()

    write(*,*) 'allocating a'
    allocate(a)

    write(*,*) 'calling a_ar()'
    call a%a_ar()

    write(*,*) 'calling a_da()'
    call a%a_da()

    write(*,*) 'deallocating a'
    deallocate(a)

    write(*,*) 'Normal termination.'

  end program main

