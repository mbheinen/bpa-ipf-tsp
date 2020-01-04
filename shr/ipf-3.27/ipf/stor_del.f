C    @(#)stor_del.f	20.4 2/13/96
        subroutine stor_del (record, ic)
        character record *(*)
 
c       stores a deleted record in array delete(ndelete)
 
        include 'ipfinc/parametr.inc'
 
        include 'ipfinc/changr.inc'
        include 'ipfinc/delete.inc'
        include 'ipfinc/prt.inc'

        common /is_batch / is_batch

        integer find_del
        logical found

        if (ndelete .ge. MAXDEL) then
           read (chgcrd(ic)(122:125), '(bz, i4)') nc
           write (errbuf(1), 100) MAXDEL
  100      format(' More than ', i5, ' deleted records.')
           write (errbuf(2), 110) record(1:33), nc
  110      format(' Overflow occurred at record ', a, ' No. ', i5)
           if (is_batch .eq. 0) then
              call prterx ('E',2)
           else
              call prterx ('F',2)
           endif
           record(126:126) = 'E'
           ndelete = 1
        else
c
c          Delete any previous entities pertaining to "record"
c
           jc = find_del(record)
           if (jc .gt. 0) then
              delete(jc) = record        ! Supersede previous entity
           else
              ndelete = ndelete + 1
              delete(ndelete) = record
           endif

        endif
        return
        end
