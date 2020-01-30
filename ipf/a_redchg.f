C    @(#)a_redchg.f	20.4 1/4/99
C****************************************************************
C
C   	File: a_redchg.f
C
C   	Purpose: 1. Reduce the following record types:
C                    "A" or "I".
C                2. Convert one of the change records into an
C                   equilalent record.
C                3. Delete all other change records by setting
C                   an error flag.
C
C   	Author: Walt Powell            Date: 13 November 1992
C   	Called by: redchgs.f
C
C****************************************************************
C
      	subroutine a_redchg (nx, nxdx)
        integer nx, nxdx(*)
 
      	include 'ipfinc/parametr.inc'

      	include 'ipfinc/changr.inc'
      	include 'ipfinc/oldchg.inc'
      	include 'ipfinc/prt.inc'

        integer a0_field(2,16), a1_field(2,11), i_field(2,2), get_rule

        data a0_field /
c
c           Type A or A0
c
     &      14, 21, 22, 25, 27, 34, 36, 37, 39, 40, 42, 43, 45, 46, 
     &      48, 49, 51, 52, 54, 55, 57, 58, 60, 61, 63, 64, 73, 76,
     &      77, 80,  0,  0 /

        data a1_field /
c
c           Type A1 through A9
c
     &      36, 37, 39, 40, 42, 43, 45, 46, 48, 49, 51, 52, 54, 55, 
     &      57, 58, 60, 61, 63, 64,  0,  0 /

        data i_field /
c
c           Type I
c
     &      27, 34,  0,  0 /
c
c       Check "A" or "I" changes
c
c       "get_rule" obtains the basic change reduction rule:
c
c       1. Del(ic) + Add(jc) -> Mod(ic)
c       2. Del(ic) + Res(jc) -> NULL
c       3. Add(ic) + Mod(jc) -> Add(ic)
c       4. Add(ic) + Del(jc) -> NULL
c       5. Mod(ic) + Mod(jc) -> Mod(ic)
c       6. Mod(ic) + Del(jc) -> Del
c
        num_rule = get_rule(nx, nxdx, ic, jc)
        do while (num_rule .gt. 0)
           if (num_rule .eq. 1 .or. num_rule .eq. 3 .or. 
     &         num_rule .eq. 5) then
c
c             1. Reduce oldchg(jc) into oldchg(ic)
c             2. Delete oldchg(jc)
c             3. Update change codes if necessary.
c
              if (oldchg(jc)(1:1) .eq. 'A' .and.
     &           (oldchg(jc)(2:2) .eq. '0' .or.
     &            oldchg(jc)(2:2) .eq. ' ')) then
                 i = 1
                 do while (a0_field(1,i) .gt. 0)
                    i1 = a0_field(1,i)
                    i2 = a0_field(2,i)

                    if (oldchg(jc)(i1:i2) .ne. ' ') then
                       oldchg(ic)(i1:i2) = oldchg(jc)(i1:i2)
                    endif
                    i = i + 1
                 enddo
              else if (oldchg(jc)(1:1) .eq. 'A') then
                 i = 1
                 do while (a1_field(1,i) .gt. 0)
                    i1 = a1_field(1,i)
                    i2 = a1_field(2,i)

                    if (oldchg(jc)(i1:i2) .ne. ' ') then
                       oldchg(ic)(i1:i2) = oldchg(jc)(i1:i2)
                    endif
                    i = i + 1
                 enddo
              else 
                 i = 1
                 do while (i_field(1,i) .gt. 0)
                    i1 = i_field(1,i)
                    i2 = i_field(2,i)

                    if (oldchg(jc)(i1:i2) .ne. ' ') then
                       oldchg(ic)(i1:i2) = oldchg(jc)(i1:i2)
                    endif
                    i = i + 1
                 enddo
              endif
c
c             Delete oldchg(jc) by setting error flag
c
              oldchg(jc)(126:126) = 'E'
              if (num_rule .eq. 1) then
                 oldchg(ic)(3:3) = 'M'
              endif
c
c             If oldchg(jc) is a pseudo change, overwrite with
c             code for oldchg(jc).  This assures that explicit changes
c             performed on pseudo changes will emerge as explicit
c             changes.
c
              if (oldchg(ic)(121:121) .eq. 'P') then
                  oldchg(ic)(121:121) = oldchg(jc)(121:121)
              endif

           else if (num_rule .eq. 2 .or. num_rule .eq. 4) then
c
c             1. Delete oldchg(ic) and oldchg(jc) by setting error 
c                flag
c
              oldchg(ic)(126:126) = 'E'
              oldchg(jc)(126:126) = 'E'

           else if (num_rule .eq. 6) then
c
c             1. Delete oldchg(ic) by setting error flag
c
              oldchg(ic)(126:126) = 'E'
c
c             If oldchg(jc) is a pseudo change, overwrite with
c             code for oldchg(jc).  This assures that explicit changes
c             performed on pseudo changes will emerge as explicit
c             changes.
c
              if (oldchg(jc)(121:121) .eq. 'P') then
                  oldchg(jc)(121:121) = oldchg(ic)(121:121)
              endif

           endif
           num_rule = get_rule(nx, nxdx, ic, jc)
        enddo                    
        return
        end
