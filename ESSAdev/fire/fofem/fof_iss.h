//
// FIRE-FOFEM $Id$
//

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */
/*                         Individual Species Structure                      */
typedef struct  {
#define  eC_ISS_SPe 10
   char cr_Spe [ eC_ISS_SPe + 1 ];
   int  i_Den;                          /* Density                           */
   int  i_DBH;                          /* Diam Breast Height                */
   int  i_TreHgt;                       /* Tree Height                       */
   int  i_CroRat;                       /* Crown Ration                      */
}  d_ISS;

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */
void ISS_InitTbl (void);
void ISS_Init (d_ISS *a_ISS);
int  ISS_Set  (d_ISS *a_ISS);
void ISS_Display (void);
int  ISS_Get  (d_ISS *a_ISS, int iX);
int  ISS_Count (void);
