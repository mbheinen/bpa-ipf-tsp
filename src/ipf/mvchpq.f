C    @(#)mvchpq.f	20.5 11/12/98
        subroutine mvchpq (nc, type, ibus, ipq)
        character *(*) type
 
c       moves add/restore pqcurves data from chgcrd(nc) to pqcurves(ix)
 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/changr.inc'
        include 'ipfinc/pqcurves.inc'
        include 'ipfinc/prt.inc'
 
        common /is_batch / is_batch

        character bus1*8

        if (type .eq. 'QP') then
           read (chgcrd(nc), 100, err=900) pqid(ipq), bus1, base1, 
     &          pqnumunit(ipq), (pqpgen(i,ipq), i=-1,15)
  100      format( bz, t4, a2, t7, a8, f4.0, i2, 2f5.1, 15f6.2)
           if (chgcrd(nc)(6:6) .eq. ' ' .or. 
     &         chgcrd(nc)(6:6) .eq. 'A') then
               pqactive(ipq) = .true.
           else
               pqactive(ipq) = .false.
           endif
           pqbusptr(ipq) = ibus
        else if (type .eq. 'QX') then
           read (chgcrd(nc), 101, err=900) bus1, base1, 
     &          (pqqmax(i,ipq), i=-1,15)
  101      format( bz, t7, a8, f4.0, 2x, 2f5.3, 15f6.2)
        else if (type .eq. 'QN') then
           read (chgcrd(nc), 101, err=900) bus1, base1, 
     &          (pqqmin(i,ipq), i=-1,15)
        else                                                           
           write (errbuf(1), 110) chgcrd(nc)(1:80)                
  110      format (' illegal record for MVCHPQ :(',a80,')')     
           if (is_batch .eq. 0) then
              call prterx ('E',1)
           else
              call prterx ('F',1)
           endif
           chgcrd(nc)(126:126) = 'E'
        endif
        go to 920

  900   write (errbuf(1), 910) chgcrd(nc)(1:80)                
  910   format (' illegal data in field :(',a80,')')     
        if (is_batch .eq. 0) then
           call prterx ('E',1)
        else
           call prterx ('F',1)
        endif
        chgcrd(nc)(126:126) = 'E'

  920   continue
        return
        end
