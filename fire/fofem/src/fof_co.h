/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fof_co.c
* Desc: Consumerd Output Structure
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */
typedef struct {


/* "YES" of "NO", tell is model actually ignited under the specified conditions. */
   char cr_BrnIg[10];


/* All fuel loads are in Tons Per Acre                                       */
   int   i_LitEqu;         /* Litter   -   Equation used to perform calc     */
   float f_LitPre;                      /* Preburn loading                   */
   float f_LitCon;                      /* Consumed amount                   */
   float f_LitPos;                      /* Post burn loading (remaining load)*/
   float f_LitPer;                      /* percent of load consumed          */

   int   i_DW1Equ;                      /* 1 Hour  0->1/4 inch               */
   float f_DW1Pre;
   float f_DW1Con;
   float f_DW1Pos;
   float f_DW1Per;

   int   i_DW10Equ;                     /* 10 Hour  1/4 -> 1 inch            */
   float f_DW10Pre;
   float f_DW10Con;
   float f_DW10Pos;
   float f_DW10Per;

   int   i_DW100Equ;                    /* 100 Hour 1 -> 3 inch              */
   float f_DW100Pre;
   float f_DW100Con;
   float f_DW100Pos;
   float f_DW100Per;

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* 3+ Down wood combined totals                                              */
   int   i_Snd_DW1kEqu;                 /* Equation number                   */
   float f_Snd_DW1kPre;                 /* Pre fire load                     */
   float f_Snd_DW1kCon;                 /* Consumed amount                   */
   float f_Snd_DW1kPos;                 /* Post Fire Remaining load          */
   float f_Snd_DW1kPer;                 /* Precent Pre Load was reduced by   */

   int   i_Rot_DW1kEqu;
   float f_Rot_DW1kPre;
   float f_Rot_DW1kCon;
   float f_Rot_DW1kPos;
   float f_Rot_DW1kPer;

/*.......................................................................... */
   int   i_DufEqu;                      /* Duff Load                         */
   float f_DufPre;
   float f_DufCon;
   float f_DufPos;
   float f_DufPer;

   int   i_DufDepEqu;                   /* Duff Depth Reduction              */
   float f_DufDepPre;                   /* Prefire depth in inches           */
   float f_DufDepCon;                   /* inches of depth consumed          */
   float f_DufDepPos;                   /* Postfire depth                    */
   float f_DufDepPer;                   /* what percent was consumed         */

   float f_MSEPer;                      /* Mineral Soil Exposer percent       */
   int   i_MSEEqu;                      /* Equation                           */

   int   i_HerEqu;                      /* Herbaceous                        */
   float f_HerPre;
   float f_HerCon;
   float f_HerPos;
   float f_HerPer;

   int   i_ShrEqu;                      /* Shrub                             */
   float f_ShrPre;
   float f_ShrCon;
   float f_ShrPos;
   float f_ShrPer;

   int   i_FolEqu;                      /* Foliage                           */
   float f_FolPre;
   float f_FolCon;
   float f_FolPos;
   float f_FolPer;

   int   i_BraEqu;                      /* Branch                            */
   float f_BraPre;
   float f_BraCon;
   float f_BraPos;
   float f_BraPer;

   float f_TotPre;                      /* Total of all Loads                */
   float f_TotCon;
   float f_TotPos;
   float f_TotPer;


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */
/* 3+ inch wood individual size clasees,                                     */
/* for equation number see above combined totals                             */
   float f_S3Pos;                       /* Post burn load                    */
   float f_S3Con;                       /* Amount Consumed                   */
   float f_S3Pre;                       /* Pre burn load                     */
   float f_S3Per;                       /* Percent consumed                  */

   float f_S6Pos ;
   float f_S6Con ;
   float f_S6Pre ;
   float f_S6Per ;

   float f_S9Pos ;
   float f_S9Con ;
   float f_S9Pre ;
   float f_S9Per ;

   float f_S20Pos;
   float f_S20Con;
   float f_S20Pre;
   float f_S20Per;

   float f_R3Pos ;
   float f_R3Con ;
   float f_R3Pre ;
   float f_R3Per ;

   float f_R6Pos ;
   float f_R6Con ;
   float f_R6Pre ;
   float f_R6Per ;

   float f_R9Pos ;
   float f_R9Con ;
   float f_R9Pre ;
   float f_R9Per ;

   float f_R20Pos;
   float f_R20Con;
   float f_R20Pre;
   float f_R20Per;

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* Emission totals                                                           */
/* Emissions are in Pounds Per Acre                                          */
   float f_PM10F;                  /* Flaming                                */
   float f_PM25F;
   float f_CH4F;
   float f_COF;
   float f_CO2F;
   float f_NOXF;
   float f_SOXF;

   float f_PM10S;                  /* Smoldering                             */
   float f_PM25S;
   float f_CH4S;
   float f_COS;
   float f_CO2S;
   float f_NOXS;
   float f_SOXS;


/* Flame & Smoldering Duration, which is the time step in which the          */
/*  emission finally stop (last one).                                        */
/* These are reported back in total seconds                                  */
   float f_FlaDur;
   float f_SmoDur;

/* Total loads consumed in Flaming and Smoldering                            */
/* NOTE: ---> expressed in Tons Per Acre                                     */
   float f_FlaCon;                 /* Total Flame Consumed                   */
   float f_SmoCon;                 /* Total Smold Consumed                   */


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* Fire intensity and duration                                               */
/* see the BOV_Get_TimHea() function                                         */
/* i_Time....time duration of burnup simulation in seconds                   */
/* f_fi......average fire intensity                                          */
/* i_Cnt.....the number of times steps burnup did during simulation          */
/* cr_SHC a comment is found in here when the fire duratiion is short        */
/*         and states the mineral soil heating is minimal                    */
/* These values are meant to be send directly into the Soil Heating          */
/*  modeling function                                                        */
   int   i_Time;                           /* time in seconds                */
   int   i_Cnt;                            /* count for taking average       */
   float f_fi;                             /* heat intensity                 */
   char  cr_SHC[200];                      /* Soil Heating Comment           */
} d_CO;
