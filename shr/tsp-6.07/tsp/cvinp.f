C    %W% %G%
      subroutine cvinp(line)

      implicit none
      
      include 'tspinc/params.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/cv.inc'
      
      character line*80, indat*8, outdat*8
      integer cvoffset, icv, i
      logical inblank, outblank

c     test for first card of new curve
      if (line(3:3).eq.' ' .or. line(3:3).eq.'1') then

c       this is a new curve
        icv   = icv   + 1
        cvtot = cvtot + 1
 
c       assign cvindx value for this curve
        if (icv.eq.1) then
          cvindx(icv) = 0
        else
          cvindx(icv) = cvindx(icv-1) + cvpts(icv-1)
        endif

c       assign cvref
        read(line(5:8),'(i4)') cvref(icv)

c       end of new curve case
      endif

c     assign points from line, keep track of total points stored
      do i = 1, 4

c       assign indat and outdat
        indat  = line(16*i+1:16*i+ 8)
        outdat = line(16*i+9:16*i+16)

c       check for blank fields
        inblank  = (indat .eq.'        ')
        outblank = (outdat.eq.'        ')

        if (inblank .and..not.outblank .or.
     &      outblank.and..not.inblank ) then

c         write error message because one of indat/outdat is blank
          write(errbuf(1), 1000) line(5:8)
1000      format('Blank input or output field in CV #',a4,
     &           ' This is a possible data error.')
          call prterr('W',1)
 
        else if (.not.outblank.and..not.inblank) then

c         store points

          cvpts(icv) = cvpts(icv) + 1
          
          read(indat ,'(f8.0)')  cvin(cvindx(icv)+cvpts(icv))
          read(outdat,'(f8.0)') cvout(cvindx(icv)+cvpts(icv))
        
c         end of inblank/outblank case
        endif

c       end of point assigning loop 
      enddo

      return
      end
