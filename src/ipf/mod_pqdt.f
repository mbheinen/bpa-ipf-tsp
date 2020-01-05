C    @(#)mod_pqdt.f	20.4 11/12/98
        subroutine mod_pqdt (ic, type, ix)
        integer ic, ix
        character *(*) type
c
c       Perform pqcurve modification changes from chgcrd(ic)
c       onto pqcurves(ix).
c       The ONLY legal modifications are a change in p.u. code
c       and/or  activation/deactivation.
c
        include 'ipfinc/parametr.inc'
 
        include 'ipfinc/alpha.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/pqcurves.inc'
        include 'ipfinc/prt.inc'

        character fieldfmt(54)*10, a_data*10, template*80
        integer i_data, fieldptr(4), field(4,54), itype
        real f_data

        data fieldptr / 1, 19, 37, 55 /
        data field /
c
c          Type QP
c
     &      1, 19, 20, -1,   1, 21, 25, -1,   1, 26, 30,  0, 
     &      1, 31, 36,  1,   1, 37, 42,  2,   1, 43, 48,  3, 
     &      1, 49, 54,  4,   1, 55, 66,  4,   1, 61, 66,  6, 
     &      1, 67, 72,  7,   1, 73, 78,  8,   1, 79, 84,  9,
     &      1, 85, 90, 10,   1, 91, 96, 11,   1, 97,102, 12,
     &      1,103,108, 13,   1,109,114, 14,   1,115,120, 15,
c
c          Type QX
c
     &      1, 19, 20, -1,   1, 21, 25, -1,   1, 26, 30,  0, 
     &      1, 31, 36,  1,   1, 37, 42,  2,   1, 43, 48,  3, 
     &      1, 49, 54,  4,   1, 55, 66,  4,   1, 61, 66,  6, 
     &      1, 67, 72,  7,   1, 73, 78,  8,   1, 79, 84,  9,
     &      1, 85, 90, 10,   1, 91, 96, 11,   1, 97,102, 12,
     &      1,103,108, 13,   1,109,114, 14,   1,115,120, 15,
c
c          Type QN
c
     &      1, 19, 20, -1,   1, 21, 25, -1,   1, 26, 30,  0, 
     &      1, 31, 36,  1,   1, 37, 42,  2,   1, 43, 48,  3, 
     &      1, 49, 54,  4,   1, 55, 66,  4,   1, 61, 66,  6, 
     &      1, 67, 72,  7,   1, 73, 78,  8,   1, 79, 84,  9,
     &      1, 85, 90, 10,   1, 91, 96, 11,   1, 97,102, 12,
     &      1,103,108, 13,   1,109,114, 14,   1,115,120, 15 /
c
c       Type QP [1..18]
c       Type QX [19..36]
c       Type QN [37..54]
c
        data fieldfmt /
     &       '(bz, i2.0)', 2*'(bz, f5.1)', 15*'(bz, f6.2)',
     &       '(bz, i2.0)', 2*'(bz, f5.3)', 15*'(bz, f6.2)', 
     &       '(bz, i2.0)', 2*'(bz, f5.3)', 15*'(bz, f6.2)' /

        if (chgcrd(ic)(1:2) .eq. 'QP') then
          itype = 1
        else if (chgcrd(ic)(1:2) .eq. 'QX') then
          itype = 2
        else
          itype = 3
        endif

        if (chgcrd(ic)(6:6) .eq. 'A' .or.
     &      chgcrd(ic)(6:6) .eq. 'a') then
           pqactive(ix) = .true.
        else if (chgcrd(ic)(6:6) .eq. '*')then
           pqactive(ix) = .false.
        endif
c
c       Perform table driven change modifications
c
        do i = fieldptr(itype), fieldptr(itype+1)-1

           i1 = field(1,i)
           i2 = field(2,i)
           i3 = field(3,i)
           i4 = field(4,i)

           if (chgcrd(ic)(i2:i3) .ne. ' ') then
              if (index(fieldfmt(i), 'f') .ne. 0) then
c
c                Floating point decoding
c
                 read (chgcrd(ic)(i2:i3), fieldfmt(i), err=900)
     &              f_data
                 if (itype .eq. 1) then
                    pqpgen(i4,ix) = f_data
                 else if (itype .eq. 2) then
                    pqqmax(i4,ix) = f_data
                 else
                    pqqmin(i4,ix) = f_data
                 endif
              else if (index(fieldfmt(i), 'i') .ne. 0) then
c
c                Integer or character decoding
c  
                 read (chgcrd(ic)(i2:i3), fieldfmt(i), err=900)
     &              i_data
                 if (itype .eq. 1) pqnumunit(ix) = i_data
              else
                 a_data = chgcrd(ic)(i2:i3)
              endif
           endif
           go to 930

  900      write (errbuf(1), 910) chgcrd(ic)(1:80)
  910      format (' Data error in field [', a, ']')
           template = ' '
           template(i2:i3) = '**********'
           write (errbuf(1), 920) template
  920      format ('                     [', a, ']')
           call prterx('W', 2)

  930      continue
        enddo
 
        return
        end
