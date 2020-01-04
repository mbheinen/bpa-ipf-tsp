C    @(#)storchgs.f	20.3 2/13/96
C****************************************************************
C
C   File: storchgs.f
C
C   Purpose: Store all changes and comment records in /oldchg/
C
C   Author: Walt Powell            Date: 13 November 1992
C   Called by: change.f
C
C****************************************************************
C
      subroutine storchgs (change, numfil)
      character change *(*) 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/changr.inc'
      include 'ipfinc/oldchg.inc'
      include 'ipfinc/prt.inc'

      if (numold .lt. MAXCHG) then
         numold = numold + 1
         write (oldchg(numold), 100) change, numfil
  100    format (a, t127, i2)
      else
         write (errbuf(1), 110) MAXCHG
  110    format(' More than ',i4, ' change records and comments archived
     &. remaining ones ignored.')
         call prterx ('W', 1)
      endif

      return
      end
