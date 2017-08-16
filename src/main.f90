program main
  use UzfModule
  implicit none
  integer :: i
  type(UzfType), pointer :: uzf => null()
  
  write(*,*) 'allocating uzf'
  allocate(uzf)
  
  write(*,*) 'calling uzf_ar()'
  call uzf%uzf_ar()
  
  write(*,*) 'calling uzf_da()'
  call uzf%uzf_da()
  
  write(*,*) 'deallocating uzf'
  deallocate(uzf)
  
end program main

