C    @(#)swpxdt.f	20.3 2/13/96
        subroutine swpxdt(m,n)
C
C       SWAP XDATA RCDS M & N
C
        include 'ipfinc/parametr.inc'
        include 'ipfinc/xdata.inc'
C
        double precision xdat

        do i = 1,22
           xdat=xdata(i,m)
           xdata(i,m)=xdata(i,n)
           xdata(i,n)=xdat
        enddo
C
        return
        end
