C    @(#)swpcbx.f	20.3 2/13/96
        subroutine swpcbx(m,n)
C
C       Swap customer bus entities M and N.
C
        common /scratch/ ntemp, ktemp(12,100), temp_index(100)
        real temp(12,100)
        integer temp_index
        equivalence (temp, ktemp)

        itemp = temp_index(m)
        temp_index(m) = temp_index(n)
        temp_index(n) = itemp

        do 100, i = 1, 12
           itemp = ktemp(i,m)
           ktemp(i,m) = ktemp(i,n)
           ktemp(i,n) = itemp
  100   continue
c
        return
        end
