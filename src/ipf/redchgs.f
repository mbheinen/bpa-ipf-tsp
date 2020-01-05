C    @(#)redchgs.f	20.5 1/4/99
C****************************************************************
C
C   	File: redchgs.f
C
C   	Purpose: Reduce (consolidate) all changes for a single
C                data item in /OLDCHG/ into a single equivalent
C                change record.
C
C   	Author: Walt Powell            Date: 13 November 1992
C   	Called by: savechgs.f
C
C****************************************************************
C
      	subroutine redchgs (first, last)
        integer first, last
 
      	include 'ipfinc/parametr.inc'

      	include 'ipfinc/changr.inc'
      	include 'ipfinc/oldchg.inc'
      	include 'ipfinc/prt.inc'
      	include 'ipfinc/qksrt.inc'

        common /scratch/ array(MAXCHG), xndex(100)
        integer array, xndex
 
        integer kmpoldch, gtchgtyp
      	external kmpoldch, swpoldch, gtchgtyp

      	if (last .eq. 0 .or. last .gt. numold) go to 900
c
c       Set "key" to disable kmpoldch from breaking ties using 
c       change index.
c
        key = 1
c
c       Reduce changes:
c
c       Add + Mod = Add'
c       Add + Del = both cancel
c       Add + Res = Illegal - program error
c       Mod + Mod = Mod'
c       Mod + Del = Del'
c       Mod + Add = Illegal - program error
c       Mod + Res = Illegal - program error
c       Del + Add = Mod'
c       Del + Res = both cancel
c       Del + Mod = Illegal - program error
c
        ix = 1
        do while (ix .le. numold)
           ic = array(ix)
           if (ic .lt. first .or. ic .gt. last) then
              ix = ix + 1
              go to 172
c
c             Exempt non-processed changes and change errors.
c
           else if (oldchg(ic)(126:126) .ne. 'P') then
              ix = ix + 1
              go to 172
           endif

           numc = 1
           xndex(numc) = ix
c
c          Look ahead for next change
c
           komptype = 0
           iy = ix + 1

           do while (iy .le. last .and. komptype .ge. 0)
              jc = array(iy)
              if (oldchg(jc)(126:126) .ne. 'P') then
                iy = iy + 1
                go to 112
              else if (jc .lt. first .or. jc .gt. last) then
                 iy = iy + 1
                 go to 112
              endif
              komptype = kmpoldch (ix, iy)
              if (komptype .lt. 0) then
              else if (komptype .gt. 0) then
                 write (errbuf(1), 100)
  100            format (' REDCHGS sort error - records :')
                 write (errbuf(2), 110) oldchg(ic)(122:125),
     1                                  oldchg(ic)(1:80)
  110            format ('   Change card no. ',a4,' (',a80,')')
                 write (errbuf(3), 110) oldchg(jc)(122:125),
     1                                  oldchg(jc)(1:80)
                 call prterx ('W', 3)
                 iy = iy + 1
              else
                 numc = numc + 1
                 xndex(numc) = iy
                 iy = iy + 1
              endif
  112         continue
           enddo

           if (numc .gt. 1) then

              ic = array(xndex(1))
              ictype = gtchgtyp (oldchg(ic)(1:2))
c
c             gtchgtyp = index ('/.DPZAIB+XQEL$$R$T', oldchg(ic)(1:1))
c
c                      /    .    d    p    z    a    i    b    +    
c                      x    q    e    l   ld   lm    r   rz    t  
              go to (170, 114, 170, 170, 170, 120, 120, 140, 140, 
     1               140, 140, 150, 150, 150, 150, 150, 150, 150) ictype
c
c             Reduce ".#" comments
c
  114         call c_redchg (numc, xndex)
              go to 160
c
c             Reduce "A" through "A9" or "I" changes
c
  120         call a_redchg (numc, xndex)
              go to 160
c
c             Reduce "B", "+", or "X" changes
c
  140         call b_redchg (numc, xndex)
              go to 160
c 
c             Reduce "E", "L", "LD", "LM", "R", "RZ", "T" changes
c
  150         call l_redchg (numc, xndex)

  160         continue
           endif

  170      ix = xndex(numc) + 1
  172      continue
        enddo

  900   continue

        return
        end
