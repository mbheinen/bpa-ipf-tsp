
	common /owner_cm/ num_owners, owner_number(MAXOWN),
     &                    owner_code(MAXOWN), owner_name(MAXOWN)

        integer num_owners, owner_number
        character owner_code*3, owner_name*32

