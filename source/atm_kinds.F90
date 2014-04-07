!=======================================================================
!
! Defines variable precision for all common data types 
!
module atm_kinds

implicit none
save

integer, parameter :: char_len  = 80, &
                      char_len_long  = 256, &
                      log_kind  = kind(.true.), &
                      int_kind  = selected_int_kind(6), &
                      real_kind = selected_real_kind(6), &
                      dbl_kind  = selected_real_kind(13), &
                      r16_kind  = selected_real_kind(26)

end module atm_kinds
!=======================================================================
