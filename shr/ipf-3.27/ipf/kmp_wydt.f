C    @(#)kmp_wydt.f	20.5 2/28/00
        integer function kmp_wydt (i,j)  
  
        include 'ipfinc/parametr.inc'  
        include 'ipfinc/bus.inc'   
        include 'ipfinc/arcntl.inc'   
        include 'ipfinc/qsdup.inc'  
        include 'ipfinc/qksrt.inc'   
  
        integer MAXWYEDELTA
        parameter (MAXWYEDELTA = 500)
        common /wye_delta/ num_wye_delta, lwye_delta(3,MAXWYEDELTA),
     &                     wye_delta(MAXWYEDELTA)

        integer MAXPTIRECORDS
        parameter (MAXPTIRECORDS = 16000)
        common /scratch/ count, array(4,MAXBUS), htable_2(MAXBUS),
     &                   nextptr_2(MAXBUS), count_newbus, 
     &                   newbusno(MAXBUS), count_newzone, 
     &                   newzoneno(MAXCZN), count_newown, 
     &                   newownno(MAXOWN),  tempc(MAXPTIRECORDS),
     &                   sort_tempc(MAXPTIRECORDS), txflag(MAXBUS),
     &                   txindex(2,MAXWYEDELTA), procnode(MAXBUS),
     &                   vangle(MAXBUS), itemp(MAXBUS)
        integer array, count, htable_2, count_newbus, newbusno, 
     &          count_newzone, newzoneno, count_newown, newownno, 
     &          sort_tempc, txflag, txindex, procnode
        character tempc*80

        character id1*1, id2*1
c           
c       Sort fields:
c       1. base1 (High to Low)
c       2. base2 (High to Low)
c       3. bus1
c       4. bus2
c       5. id
c  
        k1 = lwye_delta(1,i)
        m1 = lwye_delta(1,j)
        kmp_wydt = -100.0 * (base(k1) - base(m1))
        if (kmp_wydt .eq. 0) then
          k2 = lwye_delta(2,i)
          m2 = lwye_delta(2,j)
          kmp_wydt = -100.0 * (base(k2) - base(m2))
          if (kmp_wydt .eq. 0) then
            kmp_wydt = kompr (bus(k1), bus(m1), junk)
            if (kmp_wydt .eq. 0) then
              kmp_wydt = kompr (bus(k2), bus(m2), junk)
              if (kmp_wydt .eq. 0) then
                id1 = char (lwye_delta(3,i))
                id2 = char (lwye_delta(3,j))
                kmp_wydt = kompr (id1, id2, junk)
              endif
            endif
          endif
        endif
        return  
        end  
