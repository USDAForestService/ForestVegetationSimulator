//
// $Id$
//
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fof_co2.c
* Desc: Consumerd Output functions
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/

/*
#ifdef FOF_DLL_EXPORT
extern "C" {
   __declspec(dllexport) void CO_Init (d_CO *a_CO);
}
#endif

#ifdef FOF_DLL_IMPORT
extern "C" {
   __declspec(dllimport) void CO_Init (d_CO *a_CO);
}
#endif
*/

#ifdef BorlandXX
void WINAPI CO_Init (d_CO *a_CO);
#endif

void CO_Init (d_CO *a_CO);
