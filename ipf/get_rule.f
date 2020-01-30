C    @(#)get_rule.f	20.3 2/13/96
C****************************************************************
C
C   	File: get_rule.f
C
C   	Purpose: This interger function returns an integer value and
c                and two integer change indices designating the next
c                two change records which should be reducted. Except
c                for rule "0", the lower numbered rules have precedence.
c
c                Rule Description
c
c                 0   None 
c                 1   Del(ic) + Add(jc) -> Mod(ic)
c                 2   Del(ic) + Res(jc) -> NULL
c                 3   Add(ic) + Mod(jc) -> Add(ic)
c                 4   Add(ic) + Del(jc) -> NULL
c                 5   Mod(ic) + Mod(jc) -> Mod(ic)
c                 6   Mod(ic) + Del(jc) -> Del
C
C       Input Parameters:
C
C          nx      - the size of array nxdx()
c          nxdx()  - the array defining the set of changes
c                    to be reduced
c
C       Output Parameters:
c
c          ic      - the first oldchg() record to be reduced
c          jc      - the second oldchg() record to be reduced
C
C   	Author: Walt Powell            Date: 13 January 1993
C   	Called by: redchgs.f
C
C****************************************************************
C
      	integer function get_rule (nx, nxdx, ic, jc)
        integer nx, nxdx(*), ic, jc
 
      	include 'ipfinc/parametr.inc'

      	include 'ipfinc/changr.inc'
      	include 'ipfinc/oldchg.inc'
      	include 'ipfinc/prt.inc'

        common /scratch/ array(MAXCHG)
        integer array
 
        character c*1, d*1, null*1
        integer rule

        null = char(0)
        get_rule = 0
        ix = 1

        do while (ix .lt. nx)
           i = array(nxdx(ix))
           if (oldchg(i)(126:126) .ne. 'E') then           
              c = oldchg(i)(3:3)
              jx = ix + 1
              d = null
              do while (jx .le. nx .and. d .eq. null)
                 j = array(nxdx(jx))           
                 if (oldchg(j)(126:126) .ne. 'E') then           
                    d = oldchg(j)(3:3)
                    if (c .eq. 'D' .and. d .eq. ' ') then
                       rule = 1
                   
c                   Rule 1: Del(ic) + Add(jc) -> Mod(ic)

                    else if (c .eq. 'D' .and. d .eq. 'R') then
                       rule = 2

c                   Rule 2: Del(ic) + Res(jc) -> NULL

                    else if (c .eq. ' ' .and. d .eq. 'M') then
                       rule = 3

c                   Rule 3: Add(ic) + Mod(jc) -> Add(ic)

                    else if (c .eq. ' ' .and. d .eq. 'D') then
                       rule = 4

c                   Rule 4: Add(ic) + Del(jc) -> NULL

                    else if (c .eq. 'M' .and. d .eq. 'M') then
                       rule = 5

c                   Rule 5: Mod(ic) + Mod(jc) -> Mod(ic)

                    else if (c .eq. 'M' .and. d .eq. 'D') then
                       rule = 6

c                   Rule 6: Mod(ic) + Del(jc) -> Del
                    
                    else
                       write (errbuf(1), 330)
  330                  format (' Incompatible change records :')
                       write (errbuf(2), 320) oldchg(j)(122:125),
     1                                        oldchg(j)(1:80)
  320                  format ('   Change No. ',a4,' (',a80,')')
                       write (errbuf(3), 320) oldchg(i)(122:125),
     1                                        oldchg(i)(1:80)
                       call prterx ('W',3)
                       oldchg(j)(126:126) = 'E'
                       rule = 0
                    endif
                    if (rule .gt. 0) then
                       if (get_rule .eq. 0) then
                          get_rule = rule
                          ic = i
                          jc = j
                       else if (rule .lt. get_rule) then
                          get_rule = rule
                          ic = i
                          jc = j
                       endif
                    endif
                 endif
                 jx = jx + 1
              enddo
           endif
           ix = ix + 1
        enddo
        return
        end
