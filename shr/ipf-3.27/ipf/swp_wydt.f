C    @(#)swp_wydt.f	20.5 2/28/00
        subroutine swp_wydt (i,j)

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
     &                   newownno(MAXOWN), tempc(MAXPTIRECORDS),
     &                   sort_tempc(MAXPTIRECORDS), txflag(MAXBUS),
     &                   txindex(2,MAXWYEDELTA), procnode(MAXBUS),
     &                   vangle(MAXBUS), itemp(MAXBUS)
        integer array, count, htable_2, count_newbus, newbusno, 
     &          count_newzone, newzoneno, count_newown, newownno, 
     &          sort_tempc, txflag, txindex, procnode
        character tempc*80

        do k = 1, 3
          ixtemp = lwye_delta(k,i)
          lwye_delta(k,i) = lwye_delta(k,j)
          lwye_delta(k,j) = ixtemp
        enddo
        temp = wye_delta(i)
        wye_delta(i) = wye_delta(j)
        wye_delta(j) = temp

        return
        end
