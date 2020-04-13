int cnt;
char owners[64][4]];

pf_get_list((char *)owners, 64, OWNER_LIST);

printf("owners=\n");

for (cnt=0; cnt < 64; ++cnt) 
{
    printf("%-5s",owners[cnt]);
}
