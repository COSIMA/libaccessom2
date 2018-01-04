
!> Replace all occurrences of 'pattern' with 'replace' in string.
! Based on: http://fortranwiki.org/fortran/show/String_Functions
function replace_text(string, pattern, replace)  result(outs)

	character(len=*), intent(in) :: s,text,rep
	character(len(string)) :: outs
	integer             :: i, nt, nr

	outs = string ; nt = len_trim(pattern) ; nr = len_trim(replace)
	do
	   i = index(outs,pattern(:nt)) ; if (i == 0) exit
	   outs = outs(:i-1) // replace(:nr) // outs(i+nt:)
	end do

end function replace_text


