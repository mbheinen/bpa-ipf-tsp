int cnt;
char zones[32][3]; /* array for zone list */

pf_get_list((char *)zones, 10, ZONE_LIST, "");

for (cnt = 0; cnt < 10; ++cnt)
{
    char area_name[11];
    int error;
    error = pf_area_of_zone(area_name, zones[cnt]);
    printf("zone %-5s is in area %-10s\n", zones[cnt], area_name);
}