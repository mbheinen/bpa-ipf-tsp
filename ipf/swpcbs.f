C    @(#)swpcbs.f	20.3 2/13/96
        subroutine swpcbs(m,n)
 
C          SWAP CUSTOMER BUS ENTRIES M & N
 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/cbsorc.inc'
      include 'ipfinc/cbus.inc'
 
 
        character cbkeyt*20
 
        do 100,i=1,11
           ctbl=bctbl(i,m)
           bctbl(i,m)=bctbl(i,n)
           bctbl(i,n)=ctbl
  100   continue
 
        cbkeyt=cbkey(m)
        cbkey(m)=cbkey(n)
        cbkey(n)=cbkeyt
 
        return
        end
