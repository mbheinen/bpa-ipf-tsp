
        integer MAXCOMMODE
        parameter (MAXCOMMODE = 600)
        common /comm_mode/ 
     &          num_comm_mode,             ! Counter of common modes
     &          num_changes,               ! Counter of change records
     &          num_types,                 ! Counter of temporary data
     &          comm_ptr(MAXCOMMODE),      ! Pointer to change records
     &          change_ptr(3*MAXCOMMODE),  ! Pointer to temporary values
     &          change_nxt(3*MAXCOMMODE),  ! Pointer to next change
     &          orig_type(8,8*MAXCOMMODE), ! Array of temporary changes
     &          orig_val(8,8*MAXCOMMODE),  ! Array of temporary changes
     &          orig_nxt(8*MAXCOMMODE),    ! Pointer to next temporary 
c                                          ! changes
     &          comm_status(MAXCOMMODE)    ! Special status

        integer num_comm_mode, num_changes, num_types, comm_ptr,
     &          change_ptr, change_nxt, orig_type, orig_nxt, comm_status
        real    orig_val

        common /comm_modec/ 
     &          change_rcd(3*MAXCOMMODE),  ! Change record(s) associated
c                                          ! with common mode outage
     &          comm_mode(MAXCOMMODE)      ! Name of common modes
        character change_rcd*80, comm_mode*40

