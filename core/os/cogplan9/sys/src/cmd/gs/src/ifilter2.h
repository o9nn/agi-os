#ifndef ifilter2_INCLUDED
#  define ifilter2_INCLUDED
int zcf_setup(os_ptr op, stream_CF_state * pcfs, gs_ref_memory_t *imem);
int zlz_setup(os_ptr op, stream_LZW_state * plzs);
int zpd_setup(os_ptr op, stream_PDiff_state * ppds);
int zpp_setup(os_ptr op, stream_PNGP_state * ppps);
#endif