C    @(#)chkbrlnk.f	20.5 11/11/97
        subroutine chkbrlnk (nb, ic, module)
        character module *(*)
c
C       This subroutine verifys that bus nb branches are in
c       proper sort order following processing change record ic
c       from change module.
c
        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/prt.inc'


        common /is_batch / is_batch

        character xbuf*120
        integer p, pold, sect, oldsrt, ipack_4
c
c       Test validity of branch pointers
c
        pold = 0
        p = kbsdta(16,nb)
        oldsrt = 0
        read (chgcrd(ic)(122:125), '(bz, i4)') nc

        do while (p .gt. 0)
           sect = brsect(p)
           if (sect .gt. 0 .and. brnch_ptr(p) .lt. 0) sect = 9 - sect
           newsrt = ipack_4 (inp2alf(ky(p)), ichar(brid(p)), 
     &                       sect, brtype(p))

           if (oldsrt .gt. newsrt) then
              write (errbuf(1), 100) module
  100         format(' (', a, ') Branches not in sort order.') 
              errbuf(2) = ' '   
              call bcdbrn(pold, xbuf)     
              write (errbuf(3), 110) xbuf(1:80)
  110         format(13x,'(',a80,')') 
              call bcdbrn(p, xbuf)             
              write (errbuf(4), 110) xbuf(1:80)
              if (is_batch .eq. 0) then
                 call prterx ('E',4)
              else
                 call prterx ('F',4)
              endif
           else if (oldsrt .eq. newsrt) then
              write (errbuf(1), 120) module           
  120         format(' (', a, ') Duplicate branch records.')
              errbuf(2) = ' '                      
              call bcdbrn(pold, xbuf)              
              write (errbuf(3), 110) xbuf(1:80)    
              call bcdbrn(p, xbuf)                 
              write (errbuf(4), 110) xbuf(1:80)    
              call prterx ('W',4)                 
           endif
           oldsrt = newsrt
           pold = p
           p = brnch_nxt(p)
        enddo
        return
        end
