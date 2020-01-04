C    %W% %G%
        logical function chkbch(batch)                                  
        logical batch
c    
c       Determines for VAX whether or not the job is run in
c       batch mode.  Since UNIX does not have a batch mode, it
c       returns false both as a function and to the outgoing
c       argument.

        batch = .false.
        chkbch = .false.
        return
        end
