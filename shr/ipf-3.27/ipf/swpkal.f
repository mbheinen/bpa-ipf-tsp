C    @(#)swpkal.f	20.3 2/13/96
        subroutine swpkal(m,n)
 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/area.inc'
        include 'ipfinc/sort.inc'
 
        itemp=kaloc(m)
        kaloc(m)=kaloc(n)
        kaloc(n)=itemp
        itemp=nsysno(m)
        nsysno(m)=nsysno(n)
        nsysno(n)=itemp
 
        return
        end
