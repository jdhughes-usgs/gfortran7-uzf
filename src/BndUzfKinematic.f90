module UzfKinematicModule
  
  use KindModule, only: DP, I4B

  implicit none
  private
  public :: UzfKinematicType
  
  type :: UzfKinematicType
      integer(I4B), pointer :: ipos                  => null()
      real(DP), pointer :: thtr                      => null()
      real(DP), pointer :: thts                      => null()
      real(DP), pointer :: thti                      => null()
      real(DP), pointer :: eps                       => null()
      real(DP), pointer :: extwc                     => null()
      real(DP), pointer :: ha                        => null()
      real(DP), pointer :: hroot                     => null()
      real(DP), pointer :: rootact                   => null()
      real(DP), pointer :: etact                     => null()
      real(DP), dimension(:), pointer :: uzspst      => null()
      real(DP), dimension(:), pointer :: uzthst      => null()
      real(DP), dimension(:), pointer :: uzflst      => null()
      real(DP), dimension(:), pointer :: uzdpst      => null()
      integer(I4B), pointer :: nwavst                => null()
      real(DP), pointer :: uzolsflx                  => null()
      real(DP), pointer :: uzstor                    => null()
      real(DP), pointer :: delstor                   => null()
      real(DP), pointer :: totflux                   => null()
      real(DP), pointer :: vflow                     => null()
      integer(I4B), pointer :: nwav, ntrail          => null()
      real(DP), pointer :: sinf                      => null()
      real(DP), pointer :: finf                      => null()
      real(DP), pointer :: pet                       => null()
      real(DP), pointer :: petmax                    => null()
      real(DP), pointer :: extdp                     => null()
      real(DP), pointer :: extdpuz                   => null()
      real(DP), pointer :: finf_rej                  => null()
      real(DP), pointer :: gwet                      => null()
      real(DP), pointer :: uzfarea                   => null()
      real(DP), pointer :: cellarea                  => null()
      real(DP), pointer :: celtop                    => null()
      real(DP), pointer :: celbot                    => null()
      real(DP), pointer :: landtop                   => null()
      real(DP), pointer :: cvlm1                     => null()
      real(DP), pointer :: watab                     => null()
      real(DP), pointer :: watabold                  => null()
      real(DP), pointer :: vks                       => null()
      real(DP), pointer :: surfdep                   => null()
      real(DP), pointer :: surflux                   => null()
      real(DP), pointer :: surfluxbelow              => null()
      real(DP), pointer :: surfseep                  => null()
      real(DP), pointer :: gwpet                     => null()
      integer(I4B), pointer :: landflag              => null()
      integer(I4B), pointer :: ivertcon               => null()
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
   integer(I4B), intent(in) :: ipos
   integer(I4B), intent(in) :: nwav
! ------------------------------------------------------------------------------
    allocate(this%uzdpst(nwav))
    allocate(this%uzthst(nwav))
    allocate(this%uzflst(nwav))
    allocate(this%uzspst(nwav))
    allocate(this%nwavst)
    allocate(this%uzolsflx)
    allocate(this%ipos)
    allocate(this%thtr)
    allocate(this%thts)
    allocate(this%thti)
    allocate(this%eps)
    allocate(this%ha)
    allocate(this%hroot)
    allocate(this%rootact)
    allocate(this%extwc)
    allocate(this%etact)
    allocate(this%nwav)
    allocate(this%ntrail)
    allocate(this%uzstor)
    allocate(this%delstor)
    allocate(this%totflux)
    allocate(this%vflow)
    allocate(this%sinf)
    allocate(this%finf)
    allocate(this%finf_rej)
    allocate(this%gwet)
    allocate(this%uzfarea)
    allocate(this%cellarea)
    allocate(this%celtop)
    allocate(this%celbot)
    allocate(this%landtop)
    allocate(this%cvlm1)
    allocate(this%watab)
    allocate(this%watabold)
    allocate(this%surfdep)
    allocate(this%vks)
    allocate(this%surflux)
    allocate(this%surfluxbelow)
    allocate(this%surfseep)
    allocate(this%gwpet)
    allocate(this%pet)
    allocate(this%petmax)
    allocate(this%extdp)
    allocate(this%extdpuz)
    allocate(this%landflag) 
    allocate(this%ivertcon)
    this%ipos = ipos
    this%uzdpst = 0.0d0
    this%uzthst = 0.0d0
    this%uzflst = 0.0d0
    this%uzspst = 0.0d0
    this%nwavst = 1
    this%uzolsflx = 0.0d0
    this%thtr = 0.0d0
    this%thts = 0.0d0
    this%thti = 0.0d0
    this%eps = 0.0d0
    this%ha = 0.0d0
    this%hroot = 0.0d0
    this%rootact = 0.0d0
    this%extwc = 0.0d0
    this%etact = 0.0d0
    this%nwav = nwav
    this%ntrail = 0
    this%uzstor = 0.0d0
    this%delstor = 0.0d0
    this%totflux = 0.0d0
    this%vflow = 0.0d0
    this%sinf = 0.0d0
    this%finf = 0.0d0
    this%finf_rej = 0.0d0
    this%gwet = 0.0d0
    this%uzfarea = 0.0d0
    this%cellarea = 0.0d0
    this%celtop = 0.0d0
    this%celbot = 0.0d0
    this%landtop = 0.0d0
    this%cvlm1 = 0.0d0
    this%watab = 0.0d0
    this%watabold = 0.0d0
    this%surfdep = 0.0d0
    this%vks = 0.0d0
    this%surflux = 0.0d0
    this%surfluxbelow = 0.0d0
    this%surfseep = 0.0d0
    this%gwpet = 0.0d0
    this%pet = 0.0d0
    this%petmax = 0.0d0
    this%extdp = 0.0d0
    this%extdpuz = 0.0d0
    this%landflag = 0
    this%ivertcon = 0
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
    deallocate(this%uzdpst)
    deallocate(this%uzthst)
    deallocate(this%uzflst)
    deallocate(this%uzspst)
    deallocate(this%nwavst)
    deallocate(this%uzolsflx)
    deallocate(this%thtr)
    deallocate(this%thts)
    deallocate(this%thti)
    deallocate(this%eps)
    deallocate(this%ha)
    deallocate(this%hroot)
    deallocate(this%rootact)
    deallocate(this%extwc)
    deallocate(this%etact)
    deallocate(this%nwav)
    deallocate(this%ntrail)
    deallocate(this%uzstor)
    deallocate(this%delstor)
    deallocate(this%totflux)
    deallocate(this%vflow)
    deallocate(this%sinf)
    deallocate(this%finf)
    deallocate(this%finf_rej)
    deallocate(this%gwet)
    deallocate(this%uzfarea)
    deallocate(this%cellarea)
    deallocate(this%celtop)
    deallocate(this%celbot)
    deallocate(this%landtop)
    deallocate(this%cvlm1)
    deallocate(this%watab)
    deallocate(this%watabold)
    deallocate(this%surfdep)
    deallocate(this%vks)
    deallocate(this%surflux)
    deallocate(this%surfluxbelow)
    deallocate(this%surfseep)
    deallocate(this%gwpet)
    deallocate(this%pet)
    deallocate(this%petmax)
    deallocate(this%extdp)
    deallocate(this%extdpuz)
    deallocate(this%landflag) 
    deallocate(this%ivertcon)
    !
    ! -- return
    return
  end subroutine dealloc
!
! ------------------------------------------------------------------------------
!
! ------------------------------------------------------------------------------
! end of BndUzfKinematic object
end module UzfKinematicModule