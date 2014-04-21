
/*
#ifdef FOF_DLL_EXPORT
extern "C" {
   __declspec(dllexport) int CM_Mngr (d_CI *a_CI,  d_CO *a_CO, char cr_ErrMes[]) ;
}
#endif

#ifdef FOF_DLL_IMPORT
extern "C" {
   __declspec(dllimport) int CM_Mngr (d_CI *a_CI,  d_CO *a_CO, char cr_ErrMes[]);
}
#endif

*/

#ifdef BorlandXX
int WINAPI CM_Mngr (d_CI *a_CI,  d_CO *a_CO, char cr_ErrMes[]);
#endif

int  CM_Mngr (d_CI *a_CI,  d_CO *a_CO, char cr_ErrMes[]);
