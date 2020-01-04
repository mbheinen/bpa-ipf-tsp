C    @(#)set_batch.f	20.3 2/13/96
c
        subroutine set_batch()
        common /is_batch / is_batch       ! 0 = interactive, 1 = batch

        is_batch = 1 

        return
        end
