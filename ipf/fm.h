#ifdef IPFCONSTOK
#define IPFCONST const
#else
#define IPFCONST
#endif
FILE *gopen(IPFCONST char *filename, IPFCONST char *mode);
char *gfind(IPFCONST char *filename, IPFCONST char *mode);
