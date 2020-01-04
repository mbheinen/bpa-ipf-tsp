C    @(#)nxt_bufln.f	20.3 2/13/96
c****************************************************************
c       File: nxt_bufln
c       Purpose: Routine to get the next line from a buffer of data
c                lines separated by linefeed and terminated with null
c       Author: Jay Coleman	Date: 16 Feb 1994
c****************************************************************
c
      integer function nxt_bufln(inbuf, ix, text, lc)
      character inbuf * (*), text * (*)
      integer   ix, lc

      include 'ipfinc/errmsg.inc'
      include 'ipfinc/prt.inc'

      character null * 1, linefeed * 1
      integer   i, j, last

      save

c***   The entry point  init_bufln  must be called prior to this
c***   function call.

      if ( last .gt. 0 ) then

         nxt_bufln = 0
         i = ix
         j = index( inbuf(i+1:last), linefeed )
         if ( j .eq. 0 ) then
            nxt_bufln = 1
            j = last - i
            ix = last
         else
            ix = i + j
         endif

         if ( j - 1 .gt. len( text ) ) then
            errbuf(1) = ' nxt_bufln() truncated line :'
            errbuf(2) = ' ' // inbuf(i+1:ix-1)
            call prterx ('W', 2)
         endif

         if ( j .gt. 1 ) then
            text = inbuf(i+1:ix-1)
            lc = lastch(text)
         else
            text = ' '
            lc = 1
         endif

      else
         nxt_bufln = 1
         text = inbuf
         lc = lastch( text )
         errbuf(1) = ' nxt_bufln() got non-zero terminated buffer'
         errbuf(2) = ' ' // text(1:lc)
         call prterx ('W', 2)
      endif

      return

      entry init_bufln( inbuf, ix, text, lc )

      linefeed = char(10)
      null = char(0)

      last = index( inbuf, null )
      if ( last .gt. 0 ) then
         init_bufln = 0
      else
         init_bufln = 1
         text = inbuf
         lc = lastch( text )
         errbuf(1) = ' init_bufln() got non-zero terminated buffer'
         errbuf(2) = ' ' // text(1:lc)
         call prterx ('W', 2)
      endif

      return

      end
