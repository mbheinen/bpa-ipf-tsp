pf_rec b; 

char net_data[80]; 

printf("Enter branch identifying data: "); 
gets(net_data); 
pf_rec_a2b(net_data, &b, "I"); 
pf_rec_branch(&b, "O")