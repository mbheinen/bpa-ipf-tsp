C    @(#)swap_cb2.f	20.3 2/13/96
        subroutine swap_cb2 (i,j)

        include 'ipfinc/parametr.inc'

        include 'ipfinc/cbsorc.inc'
        include 'ipfinc/cbus.inc'

        integer p
        character tempc*20 

        tempc = cbkey(i)
        cbkey(i) = cbkey(j)
        cbkey(j) = tempc 

        do k = 1, 12
           p = kbctbl(k,i)
           kbctbl(k,i) = kbctbl(k,j)
           kbctbl(k,j) = p
        enddo

        return
        end
