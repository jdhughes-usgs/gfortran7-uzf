! -- Uzf module
module UzfModule
!
  use UzfKinematicModule

  implicit none

  type uzfcontainer
    class(UzfKinematicType), pointer :: obj
  end type uzfcontainer

  private
  public :: UzfType

  type:: UzfType
    ! output integers
    !integer(kind=4), pointer :: iprwcont => null()
    !integer(kind=4), pointer :: iwcontout => null()
    !integer(kind=4), pointer :: ibudgetout => null()

    type(uzfcontainer), pointer, dimension(:) :: elements    => null() !array of all the kinematic uzf objects
    type(UzfKinematicType), pointer           :: uzfobj      => null() !uzf kinematic object

    
    !
    ! -- uzf data
    integer(kind=4), pointer                       :: nodes        => null() !cdl--(this should probably be maxbound)
    integer(kind=4), pointer                       :: nwav         => null()
    integer(kind=4), dimension(:), pointer         :: mfcellid     => null()
    !real(kind=8), dimension(:), pointer             :: appliedinf   => null()

  contains

    procedure :: uzf_allocate_arrays
    procedure :: uzf_ar
    procedure :: uzf_da
    !
    ! -- methods specific for uzf
    procedure, private :: uzf_allocate_scalars
    procedure, private :: pakdata_rd

  end type UzfType

contains

  subroutine uzf_ar(this)
! ******************************************************************************
! uzf_ar -- Allocate and Read
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules
    ! -- dummy
    class(UzfType), intent(inout) :: this
    ! -- local
    integer(kind=4) :: i
    type(UzfKinematicType), pointer :: uzfobj     
! ------------------------------------------------------------------------------
    !
    ! -- Allocate scalars
    call this%uzf_allocate_scalars()
    !
    ! -- initialize nodes
    this%nodes = 10
    this%nwav = 20
    !
    ! -- Allocate arrays in package superclass
    call this%uzf_allocate_arrays()
    !
    ! -- Allocate UZF objects plus one extra for work array
    allocate(this%elements(this%nodes+1))
    do i = 1, this%nodes + 1
      allocate(uzfobj)
      this%elements(i)%obj => uzfobj
    enddo
    !
    ! -- Initialize each UZF object
    do i = 1, this%nodes+1
        this%uzfobj => this%elements(i)%obj
        call this%uzfobj%init(i,this%nwav)
    end do
!
!   --Read uzf cell properties and set values
    call this%pakdata_rd()
    !
    ! -- return
    return
  end subroutine uzf_ar

  subroutine uzf_allocate_arrays(this)
! ******************************************************************************
! allocate_arrays -- allocate arrays used for mover
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(UzfType),   intent(inout) :: this
    ! -- local
    integer (kind=4) :: i
! ------------------------------------------------------------------------------
    !
    ! -- allocate uzf specific arrays
    allocate(this%mfcellid(this%nodes))
    !allocate(this%appliedinf(this%nodes))
    !!
    !! -- initialize arrays
    !do i = 1, this%nodes
    !  this%mfcellid(i) = 0
    !  this%appliedinf(i) = 0.0d0
    !end do
    !
    ! -- return
    return
    end subroutine uzf_allocate_arrays

   subroutine pakdata_rd(this)
! ******************************************************************************
! pakdata_rd -- Read UZF cell properties and set them for UzfKinematic type.
! ******************************************************************************
! ------------------------------------------------------------------------------
    ! -- dummy
    class(UzfType) :: this
    ! -- local
    integer(kind=4) :: n
    integer(kind=4), dimension(:), allocatable :: rowmaxnnz
    integer(kind=4), dimension(:), allocatable :: nboundchk
! ------------------------------------------------------------------------------
!
    !
    ! -- allocate space for node counter and initilize
    allocate(rowmaxnnz(12))
    do n = 1, 12
      rowmaxnnz(n) = 0
    end do
    !
    ! -- allocate space for local variables
    allocate(nboundchk(this%nodes))
    do n = 1, this%nodes
      nboundchk(n) = 0
    end do
    !
    ! -- deallocate local variables
    deallocate(rowmaxnnz)
    deallocate(nboundchk)
    !
    ! -- return
    return
  end subroutine pakdata_rd

  subroutine uzf_allocate_scalars(this)
! ******************************************************************************
! allocate_scalars -- allocate scalar members
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules

    ! -- dummy
    class(UzfType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- allocate uzf specific scalars
    !allocate(this%iprwcont)
    !allocate(this%iwcontout)
    !allocate(this%ibudgetout)
    allocate(this%nodes)
    allocate(this%nwav)
    !
    ! -- initialize scalars
    !this%iprwcont = 0
    !this%iwcontout = 0
    !this%ibudgetout = 0
    this%nodes = 0
    this%nwav = 0
    !
    ! -- return
    return
  end subroutine uzf_allocate_scalars
!
  subroutine uzf_da(this)
! ******************************************************************************
! uzf_da -- Deallocate objects
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- dummy
    class(UzfType) :: this
    ! -- locals
    integer (kind=4) :: i
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- deallocate uzf objects
    do i = 1, this%nodes+1
        this%uzfobj => this%elements(i)%obj
        call this%uzfobj%dealloc()
        nullify(this%elements(i)%obj)
    end do
    nullify(this%uzfobj)
    deallocate(this%elements)
    !
    ! -- deallocate scalars
    !deallocate(this%iprwcont)
    !deallocate(this%iwcontout)
    !deallocate(this%ibudgetout)
    deallocate(this%nodes)
    deallocate(this%nwav)
    !!
    ! -- deallocate arrays
    deallocate(this%mfcellid)
    !deallocate(this%appliedinf)
    !
    ! -- Return
    return
  end subroutine uzf_da

end module UzfModule
