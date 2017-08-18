module UzfKinematicModule

  implicit none
  private
  public :: UzfKinematicType
  
  type :: UzfKinematicType
      integer(kind=4), pointer :: ipos                  => null()
      integer(kind=4), pointer :: nwav                  => null()
      real(kind=8), dimension(:), pointer :: uzdpst      => null()
!      real(kind=8), dimension(:), pointer :: uzthst      => null()
!      real(kind=8), dimension(:), pointer :: uzflst      => null()
!      real(kind=8), dimension(:), pointer :: uzspst      => null()
  contains
      procedure :: init
      procedure :: dealloc
    end type UzfKinematicType
!  
    contains
!
! ------------------------------------------------------------------------------
   
  subroutine init(this, ipos, nwav)
! ******************************************************************************
! init -- allocate and set uzf object variables
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
   !modules
   !arguments
   class(UzfKinematicType) :: this
   integer(kind=4), intent(in) :: ipos
   integer(kind=4), intent(in) :: nwav
   !local
   integer(kind=4) :: i
! ------------------------------------------------------------------------------
    allocate(this%ipos)
    allocate(this%nwav)
    allocate(this%uzdpst(nwav))
!    allocate(this%uzthst(nwav))
!    allocate(this%uzflst(nwav))
!    allocate(this%uzspst(nwav))

    this%ipos = ipos
    this%nwav = nwav
    do i = 1, nwav
      this%uzdpst(i) = 0.0d0
!      this%uzthst(i) = 0.0d0
!      this%uzflst(i) = 0.0d0
!      this%uzspst(i) = 0.0d0
    end do
  end subroutine init
  !
  !
  subroutine dealloc(this)
! ******************************************************************************
! dealloc -- deallocate uzf object variables
!
! ******************************************************************************
!
!    SPECIFICATIONS:
! ------------------------------------------------------------------------------
   ! -- modules
   ! -- dummy
   class(UzfKinematicType) :: this
   ! -- locals
! ------------------------------------------------------------------------------
    deallocate(this%ipos)
    deallocate(this%nwav)
    deallocate(this%uzdpst)
!    deallocate(this%uzthst)
!    deallocate(this%uzflst)
!    deallocate(this%uzspst)
    !
    ! -- return
    return
  end subroutine dealloc
end module UzfKinematicModule