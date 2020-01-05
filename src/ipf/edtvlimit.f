C    @(#)edtvlimit.f	20.3 2/13/96
        subroutine edtvlimit (pos, type, vmin, vmax, vlow, vhigh)
        integer pos
        character type *(*)

        include 'ipfinc/parametr.inc'
        include 'ipfinc/sortuvov.inc'
        
        if (type .eq. 'insert') then
c
c          Insert new entity.
c
           do i = num_limit, pos, -1
              v_max(i+1) = v_max(i)
              v_min(i+1) = v_min(i)
              v_range(1,i+1) = v_range(1,i)
              v_range(2,i+1) = v_range(2,i)
           enddo
           v_max(pos) = vmax
           v_min(pos) = vmin
           v_range(1,pos) = vlow
           v_range(2,pos) = vhigh
           num_limit = num_limit + 1

        else if (type .eq. 'delete') then
c
c          Delete current entity.
c
           do i = pos, num_limit-1
              v_max(i) = v_max(i+1)
              v_min(i) = v_min(i+1)
              v_range(1,i) = v_range(1,i+1)
              v_range(2,i) = v_range(2,i+1)
           enddo
           num_limit = num_limit - 1

        else if (type .eq. 'replace') then
c
c          Replace current entity.
c
           v_max(pos) = vmax
           v_min(pos) = vmin
           v_range(1,pos) = vlow
           v_range(2,pos) = vhigh
        endif
        return
        end  
