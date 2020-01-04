C    @(#)readdc.f	20.3 2/13/96
        subroutine readdc
C
C       This is a dummy routine which is substituted for a similar
C       routine in EPRI's RP1964 code.
C
        write (*, 100) 'READDC'
  100   format (' Dummy routine ', a, ' entered.')
        return
        end
