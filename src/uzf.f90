! -- Uzf module
module UzfModule
!
  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DEM6, DEM4, DEM2, DEM1, DHALF,              &
                             DONE, DHUNDRED,                                    &
                             LINELENGTH, LENFTYPE, LENPACKAGENAME,              &
                             LENBOUNDNAME, LENBUDTXT, DNODATA,                  &
                             NAMEDBOUNDFLAG, MAXCHARLEN,                        &
                             DHNOFLO, DHDRY
  use MemoryTypeModule, only: MemoryTSType
  use MemoryManagerModule, only: mem_allocate, mem_reallocate, mem_setptr,      &
                                 mem_deallocate

  implicit none

  private
  public :: UzfType

  type:: UzfType
    ! output integers
    integer(I4B), pointer :: iprwcont => null()
    integer(I4B), pointer :: iwcontout => null()
    integer(I4B), pointer :: ibudgetout => null()
    !
    ! -- uzf data
    integer(I4B), pointer                       :: nodes        => null() !cdl--(this should probably be maxbound)
    integer(I4B), dimension(:), pointer         :: mfcellid     => null()
    real(DP), dimension(:), pointer             :: appliedinf   => null()

  contains

    procedure :: uzf_allocate_arrays
    procedure :: uzf_ar
    procedure :: uzf_da
    !
    ! -- methods specific for uzf
    procedure, private :: uzf_allocate_scalars
    procedure, private :: pakdata_rd
    procedure, private :: pakdata_pr

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
    integer(I4B) :: i, n
! ------------------------------------------------------------------------------
    !
    ! -- Allocate scalars
    call this%uzf_allocate_scalars()
    !
    ! -- initialize nodes
    this%nodes = 10
    !
    ! -- Allocate arrays in package superclass
    call this%uzf_allocate_arrays()
!
!   --Read uzf cell properties and set values
    call this%pakdata_rd()
    !
    ! -- print cell data
    call this%pakdata_pr()
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
    ! -- modules
    !use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(UzfType),   intent(inout) :: this
    ! -- local
    integer (I4B) :: i
    integer (I4B) :: j
    integer (I4B) :: ipos
! ------------------------------------------------------------------------------
    !
    ! -- allocate uzf specific arrays
    call mem_allocate(this%mfcellid, this%nodes, 'MFCELLID', 'apple')
    call mem_allocate(this%appliedinf, this%nodes, 'APPLIEDINF', 'apple')
    !
    ! -- initialize arrays
    do i = 1, this%nodes
      this%mfcellid(i) = 0
      this%appliedinf(i) = DZERO
    end do
    !
    ! -- return
    return
    end subroutine uzf_allocate_arrays

   subroutine pakdata_rd(this)
! ******************************************************************************
! pakdata_rd -- Read UZF cell properties and set them for UzfKinematic type.
! ******************************************************************************
    use InputOutputModule, only: urword
    use SimModule, only: ustop, store_error, count_errors
! ------------------------------------------------------------------------------
    ! -- dummy
    class(UzfType) :: this
    ! -- local
    character(len=LINELENGTH) :: line, errmsg, cellid
    integer(I4B) :: ierr, i, n, landflag, ivertcon
    integer(I4B) :: j
    integer(I4B) :: ic
    logical :: isfound, endOfBlock
    real(DP) :: surfdep,vks,thtr,thts,thti,eps,hgwf
    integer(I4B), dimension(:), allocatable :: rowmaxnnz
    integer(I4B), dimension(:), allocatable :: nboundchk
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
    ! -- initialize variables
    surfdep = DZERO
    vks = DZERO
    thtr = DZERO
    thts = DZERO
    thti = DZERO
    eps = DZERO
    landflag = 0
    ivertcon = 0
    !
    ! -- deallocate local variables
    deallocate(rowmaxnnz)
    deallocate(nboundchk)
    !
    ! -- return
    return
  end subroutine pakdata_rd

  subroutine pakdata_pr(this)
! ******************************************************************************
! pakdata_pr -- Read UZF cell properties and set them for UzfKinematic type.
! ******************************************************************************
! ------------------------------------------------------------------------------
    ! -- dummy
    class(UzfType) :: this
    ! -- local
    character (len=20) :: cellids, cellid
    character(len=LINELENGTH) :: line, linesep
    character(len=16) :: text
    integer(I4B) :: i
    integer(I4B) :: n
    integer(I4B) :: node
    integer(I4B) :: iloc
    real(DP) :: q
! ------------------------------------------------------------------------------
!
    !
    ! -- return
    return
  end subroutine pakdata_pr

  subroutine uzf_allocate_scalars(this)
! ******************************************************************************
! allocate_scalars -- allocate scalar members
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
    ! -- modules

    use MemoryManagerModule, only: mem_allocate
    ! -- dummy
    class(UzfType) :: this
! ------------------------------------------------------------------------------
    !
    ! -- allocate uzf specific scalars
    call mem_allocate(this%iprwcont, 'IPRWCONT', 'orange')
    call mem_allocate(this%iwcontout, 'IWCONTOUT', 'orange')
    call mem_allocate(this%ibudgetout, 'IBUDGETOUT', 'orange')
    call mem_allocate(this%nodes, 'NODES', 'orange')
    !
    ! -- initialize scalars
    this%iprwcont = 0
    this%iwcontout = 0
    this%ibudgetout = 0
    this%nodes = 0
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
    use MemoryManagerModule, only: mem_deallocate
    ! -- dummy
    class(UzfType) :: this
    ! -- locals
    integer (I4B) :: i
    ! -- format
! ------------------------------------------------------------------------------
    !
    ! -- deallocate scalars
    call mem_deallocate(this%iprwcont)
    call mem_deallocate(this%iwcontout)
    call mem_deallocate(this%ibudgetout)
    call mem_deallocate(this%nodes)
    !
    ! -- deallocate arrays
    call mem_deallocate(this%mfcellid)
    call mem_deallocate(this%appliedinf)
    !
    ! -- Return
    return
  end subroutine uzf_da

end module UzfModule
