C    @(#)newfrm.f	20.3 2/13/96
        logical function newfrm(frmnum)
 
C               Function to determine if the fiche output has advanced
C               to a new frame so that a new index entry may be made.
 
C               FRMNUM  =  The frame number from the last index entry.
C                          on input is the old frame number.  If current
C                          frame is greater, it is reset.
 
      include 'ipfinc/pageno.inc'
 
        integer frmnum
 
        if ( frmnum .lt. fichpg ) then
           frmnum = fichpg
           newfrm = .true.
        else
           newfrm = .false.
        endif
 
        return
 
        end
