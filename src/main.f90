program main
  use UzfModule
  implicit none
  type(UzfType), pointer :: uzf => null()
  
  write(*,*) 'allocating uzf'
  allocate(uzf)
  
  write(*,*) 'calling uzf_ar()'
  call uzf%uzf_ar()
  
  write(*,*) 'calling uzf_da()'
  call uzf%uzf_da()
  
  write(*,*) 'deallocating uzf'
  deallocate(uzf)
  
  write(*,*) 'Normal termination.'
  
end program main

