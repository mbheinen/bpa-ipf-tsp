/* 
* file of debug flags
*/
extern unsigned long ipfdebug;
#define ipfdbio stderr
#define DB_NoMask          (0L)
#define DB_TraceMask       (1L<<0)  /*    1 */
#define DB_LineTapMask     (1L<<1)  /*    2 */
#define DB_BusSectionMask  (1L<<2)  /*    4 */
#define DB_Toolbox         (1L<<3)  /*    8 */
#define DB_Edge            (1L<<4)  /*   16 */
#define DB_Vertex          (1L<<5)  /*   32 */
#define DB_Filedlgrtn      (1L<<6)  /*   64 */
#define DB_SelfTest        (1L<<7)  /*  128 */
#define DB_GraphPSCorMask  (1L<<8)  /*  256 */
#define DB_BusFilterMask   (1L<<9)  /*  512 */
#define DB_Pf_CbMask       (1L<<10) /* 1024 */
#define DB_QuikExitMask    (1L<<11) /* 2048 */
#define DB_TimerMask       (1L<<12) /* 4096 */
#define DB_GraphMask       (1L<<13) /* 8192 */
