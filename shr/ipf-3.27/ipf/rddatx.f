C    @(#)rddatx.f	20.3 2/13/96
        subroutine rddatx (iu,a,n,error)
 
        dimension a(n)
        logical error
C
        error = .false.
        read (iu,err=900,end=900) a
        return
  900   error = .true.
        return
        end
