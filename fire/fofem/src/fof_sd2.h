
/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/*                   Soil Input Parameters - Duff Model                      */
d_SD  sr_SD[] = {

/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/
 {   e_FinSil,    /* Fine Silt                                    */
     -1,              /* midburn 720  1680 900 1100        1000  900  */
     -1,              /* burntime 240  180  300  300        400  400  */
    1000,             /* stoptime 1200 2400 1800 1800      1500  1500 */
    -1,               /* duffdepth 0.08 0.06 0.05  0.04    0.07  0.07 */
    18.4E6,           /* duff heat content - J/m3                     */
    180,              /* duff density - kg/m3                         */
    1.3e6,            /* soil bulk density - g/m3                     */
    2.35e6,           /* soil particle density - g/m3                 */
    0.207,            /* extrapolated water cont. at -1 J/kg          */
    2.31,             /* thermal conductivity of mineral fraction     */
    0.071,            /* de Vries shape factor                        */
    0.148,            /* water content for liquid recirculation       */
    4.14,             /* power for recirculation function             */
    60,               /* time step - s                                */
    -1,               /* starting soil water content - m3/m3          */
    e_StaSoiTem },    /* starting soil temperatue - C                 */

/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/
 {   e_LoaSke,    /* Loamy Skeletal                               */
     -1,              /* midburn 720  1680 900 1100        1000  900  */
     -1,              /* burntime 240  180  300  300        400  400  */
    1000,             /* stoptime 1200 2400 1800 1800      1500  1500 */
    -1,               /* duffdepth 0.08 0.06 0.05  0.04    0.07  0.07 */
    18.4E6,           /* duff heat content - J/m3                     */
    180,              /* duff density - kg/m3                         */
    0.8e6,            /* soil bulk density - g/m3                     */
    2.13e6,           /* soil particle density - g/m3                 */
    0.321,            /* extrapolated water cont. at -1 J/kg          */
    1.03,             /* thermal conductivity of mineral fraction     */
    0.13,             /* de Vries shape factor                        */
    0.133,            /* water content for liquid recirculation       */
    6.08,             /* power for recirculation function             */
    60,               /* time step - s                                */
    -1,               /* starting soil water content - m3/m3          */
    e_StaSoiTem },    /* starting soil temperatue - C                 */

/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/
 {   e_Fin,       /* Fine        */
     -1,              /* midburn 720  1680 900 1100        1000  900  */
     -1,              /* burntime 240  180  300  300        400  400  */
    1000,             /* stoptime 1200 2400 1800 1800      1500  1500 */
    -1,               /* duffdepth 0.08 0.06 0.05  0.04    0.07  0.07 */
    18.4E6,           /* duff heat content - J/m3                     */
    180,              /* duff density - kg/m3                         */
    1.15e6,           /* soil bulk density - g/m3                     */
    2.35e6,           /* soil particle density - g/m3                 */
    0.202,            /* extrapolated water cont. at -1 J/kg          */
    2.21,             /* thermal conductivity of mineral fraction     */
    0.084,            /* de Vries shape factor                        */
    0.152,            /* water content for liquid recirculation       */
    4.63,             /* power for recirculation function             */
    60,               /* time step - s                                */
    -1,               /* starting soil water content - m3/m3          */
    e_StaSoiTem },    /* starting soil temperatue - C                 */

/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/
 {   e_CoaSil,    /* Coarse Silt      */
     -1,              /* midburn 720  1680 900 1100        1000  900  */
     -1,              /* burntime 240  180  300  300        400  400  */
    1000,             /* stoptime 1200 2400 1800 1800      1500  1500 */
    -1,               /* duffdepth 0.08 0.06 0.05  0.04    0.07  0.07 */
    18.4E6,           /* duff heat content - J/m3                     */
    180,              /* duff density - kg/m3                         */
    1.23e6,           /* soil bulk density - g/m3                     */
    2.35e6,           /* soil particle density - g/m3                 */
    0.157,            /* extrapolated water cont. at -1 J/kg          */
    2.53,             /* thermal conductivity of mineral fraction     */
    0.103,            /* de Vries shape factor                        */
    0.218,            /* water content for liquid recirculation       */
    3.43,             /* power for recirculation function             */
    60,               /* time step - s                                */
    -1,               /* starting soil water content - m3/m3          */
    e_StaSoiTem },    /* starting soil temperatue - C                 */

/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/
 {   e_CoaLoa,    /* "Coarse Loam    */
     -1,              /* midburn 720  1680 900 1100        1000  900  */
     -1,              /* burntime 240  180  300  300        400  400  */
    1000,             /* stoptime 1200 2400 1800 1800      1500  1500 */
    -1,               /* duffdepth 0.08 0.06 0.05  0.04    0.07  0.07 */
    18.4E6,           /* duff heat content - J/m3                     */
    180,              /* duff density - kg/m3                         */
    1.3e6,            /* soil bulk density - g/m3                     */
    2.35e6,           /* soil particle density - g/m3                 */
    0.102,            /* extrapolated water cont. at -1 J/kg          */
    2.57,             /* thermal conductivity of mineral fraction     */
    0.106,            /* de Vries shape factor                        */
    0.127,            /* water content for liquid recirculation       */
    2.93,             /* power for recirculation function             */
    60,               /* time step - s                                */
    -1,               /* starting soil water content - m3/m3          */
    e_StaSoiTem },    /* starting soil temperatue - C                 */
/*. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*/
 {  "",               /* "Royal"     */
    -1,               /* midburn 720  1680 900 1100        1000  900  */
    -1,               /* burntime 240  180  300  300        400  400  */
    -1,               /* stoptime 1200 2400 1800 1800      1500  1500 */
    -1,               /* duffdepth 0.08 0.06 0.05  0.04    0.07  0.07 */
    -1,               /* duff heat content - J/m3                     */
    -1,               /* duff density - kg/m3                         */
    -1,               /* soil bulk density - g/m3                     */
    -1,               /* soil particle density - g/m3                 */
    -1,               /* extrapolated water cont. at -1 J/kg          */
    -1,               /* thermal conductivity of mineral fraction     */
    -1,               /* de Vries shape factor                        */
    -1,               /* water content for liquid recirculation       */
    -1,               /* power for recirculation function             */
    -1,               /* time step - s                                */
    -1,               /* starting soil water content - m3/m3          */
    -1 }              /* starting soil temperatue - C                 */
} ;
