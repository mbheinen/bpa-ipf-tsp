C    @(#)gtstatus.f	20.3 2/13/96
C****************************************************************
C
C       File: gtstatus.f
C       Purpose: Routine to obtain system status
C
C       Author: Walt Powell  Date: 20 February 1992
C                        Modified: 20 February 1992
C       Called by:
C
C****************************************************************
C
        integer function gtstatus (in_buffer, out_buffer)
        character in_buffer *(*), out_buffer *(*)

        include 'ipfinc/parametr.inc'
      	include 'ipfinc/blank.inc'
      	include 'ipfinc/changr.inc'
      	include 'ipfinc/lfiles.inc'
      	include 'ipfinc/delete.inc'

	character null * 1, linefeed * 1, record * 132
        integer o2, apdoutbuf

        maxbuf_out = len( out_buffer ) - 400
        null = char(0)
        linefeed = char(10)

        out_buffer(1:1) = null
        o2 = index (out_buffer,null)
c
c       Program: <n> version <n> date <n> 
c       Program size: max buses <n> max branches
c       Case: <name> status <n> base file <n> 
c             <n> buses <n> branches <n> areas <n> d-c lines
c             <n> changes
c            c comments
c            c comments
c            c comments
c
        gtstatus = 0

c*** remember maxbuf_out is really 400 less than the real buffer size
        if (o2 .gt. maxbuf_out) then
           write (out_buffer(o2:o2+8), 820) linefeed, null
  820      format (a, '*[MORE]', a)
           o2 = o2 + 9
        endif

  900   continue
        return
        end
