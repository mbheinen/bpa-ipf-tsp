C    @(#)stats.f	20.3 2/13/96
        subroutine stats(cpuscs, clktme, pflts, dirio, bufio)
        real cpuscs,clktme
        integer pflts,dirio,bufio

C       RETURN JOB STATISTICS
C               CPUSECS - REAL - ACCUMULATED CPU TIME
C               CLKTIME - REAL - TIME-OF-DAY, IN SECONDS

        cpuscs = cpu_secs(0.0)
        clktme = float( n_secs(0) )

C***    The following have been set to zero, since the method of
C***    obtaining them is non portable
C               PFLTS   - INT. - NO. OF PAGE FAULTS
C               DIRIO   - INT. - NO. OF DIRECT I/O
C               BUFIO   - INT. - NO. OF BUFFERED I/O
        pflts = 0
        dirio = 0
        bufio = 0

        return
        end
