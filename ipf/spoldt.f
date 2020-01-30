C    @(#)spoldt.f	20.3 2/13/96
        subroutine spoldt(m,n)
C
C       Swap OLDTBX entities M & N
C
 
      include 'ipfinc/parametr.inc'
      include 'ipfinc/oldtbx.inc'
 
C
        do 100 i = 1, 8
           itbx = oldtbx(i,m)
           oldtbx(i,m) = oldtbx(i,n)
           oldtbx(i,n) = itbx
  100   continue
        return
        end
