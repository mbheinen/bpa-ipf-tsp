C    @(#)get_user.f	20.3 2/13/96
      subroutine get_user( userid )
      character * 10  userid

c This routine calls a portable "C" routine to get the userid

      common /c_userid/ user
      character * 10  user

      call cgetuser
      userid = user

c Convert "C style" null terminated string to "Fortran style" 
c         blank filled string

      ix = index( userid, char(0) )
      if( ix .gt. 0  .and. ix .le. 10 ) then
         do 100, i = ix, 10
            userid(i:i) = ' '
  100    continue
      endif
      return
      end
