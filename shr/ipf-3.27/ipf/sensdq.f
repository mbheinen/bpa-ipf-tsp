C    @(#)sensdq.f	20.3 2/13/96
        function sensdq (kt,dpt)
        double precision dpt(2,*)
C
C       This function computes the change in reactive injections from th
C       voltage perturbation DPT.
C
        include 'ipfinc/parametr.inc'
        include 'ipfinc/gamma.inc'
C
C       CALL SENJAC WITH "KSW = 1" TO COMPUTE JACOBIAN ELEMENTS FOR
C       Q-CONSTRAINT REGARDLESS OF BUS TYPE.
C
        call senjac (kt,1)
        sensdq = 0.0
        l = korder(0)
        do while (l .gt. 0)
           mt = kolum(l)
           sensdq = sensdq + rowj(l) * dpt(1,mt) + rowl(l) * dpt(2,mt)
           l = korder(l)
        enddo
        return
        end
