#ifndef CLBLAST_CLBLAST_NETLIB_C_H_
#define CLBLAST_CLBLAST_NETLIB_C_H_
#if defined(_WIN32) && defined(CLBLAST_DLL)
#if defined(COMPILING_DLL)
#define PUBLIC_API __declspec(dllexport)
#else
#define PUBLIC_API __declspec(dllimport)
#endif
#else
#define PUBLIC_API
#endif
#ifdef __cplusplus
extern "C" {
#endif
typedef enum CLBlastLayout_ { CLBlastLayoutRowMajor = 101,
CLBlastLayoutColMajor = 102 } CLBlastLayout;
typedef enum CLBlastTranspose_ { CLBlastTransposeNo = 111, CLBlastTransposeYes = 112,
CLBlastTransposeConjugate = 113 } CLBlastTranspose;
typedef enum CLBlastTriangle_ { CLBlastTriangleUpper = 121,
CLBlastTriangleLower = 122 } CLBlastTriangle;
typedef enum CLBlastDiagonal_ { CLBlastDiagonalNonUnit = 131,
CLBlastDiagonalUnit = 132 } CLBlastDiagonal;
typedef enum CLBlastSide_ { CLBlastSideLeft = 141, CLBlastSideRight = 142 } CLBlastSide;
typedef enum CLBlastKernelMode_ { CLBlastKernelModeCrossCorrelation = 141, CLBlastKernelModeConvolution = 152 } CLBlastKernelMode;
typedef CLBlastLayout CBLAS_ORDER;
typedef CLBlastTranspose CBLAS_TRANSPOSE;
typedef CLBlastTriangle CBLAS_UPLO;
typedef CLBlastDiagonal CBLAS_DIAG;
typedef CLBlastSide CBLAS_SIDE;
#define CblasRowMajor CLBlastLayoutRowMajor
#define CblasColMajor CLBlastLayoutColMajor
#define CblasNoTrans CLBlastTransposeNo
#define CblasTrans CLBlastTransposeYes
#define CblasConjTrans CLBlastTransposeConjugate
#define CblasUpper CLBlastTriangleUpper
#define CblasLower CLBlastTriangleLower
#define CblasNonUnit CLBlastDiagonalNonUnit
#define CblasUnit CLBlastDiagonalUnit
#define CblasLeft CLBlastSideLeft
#define CblasRight CLBlastSideRight
void PUBLIC_API cblas_srotg(float* sa,
float* sb,
float* sc,
float* ss);
void PUBLIC_API cblas_drotg(double* sa,
double* sb,
double* sc,
double* ss);
void PUBLIC_API cblas_srotmg(float* sd1,
float* sd2,
float* sx1,
const float sy1,
float* sparam);
void PUBLIC_API cblas_drotmg(double* sd1,
double* sd2,
double* sx1,
const double sy1,
double* sparam);
void PUBLIC_API cblas_srot(const int n,
float* x, const int x_inc,
float* y, const int y_inc,
const float cos,
const float sin);
void PUBLIC_API cblas_drot(const int n,
double* x, const int x_inc,
double* y, const int y_inc,
const double cos,
const double sin);
void PUBLIC_API cblas_srotm(const int n,
float* x, const int x_inc,
float* y, const int y_inc,
float* sparam);
void PUBLIC_API cblas_drotm(const int n,
double* x, const int x_inc,
double* y, const int y_inc,
double* sparam);
void PUBLIC_API cblas_sswap(const int n,
float* x, const int x_inc,
float* y, const int y_inc);
void PUBLIC_API cblas_dswap(const int n,
double* x, const int x_inc,
double* y, const int y_inc);
void PUBLIC_API cblas_cswap(const int n,
void* x, const int x_inc,
void* y, const int y_inc);
void PUBLIC_API cblas_zswap(const int n,
void* x, const int x_inc,
void* y, const int y_inc);
void PUBLIC_API cblas_sscal(const int n,
const float alpha,
float* x, const int x_inc);
void PUBLIC_API cblas_dscal(const int n,
const double alpha,
double* x, const int x_inc);
void PUBLIC_API cblas_cscal(const int n,
const void* alpha,
void* x, const int x_inc);
void PUBLIC_API cblas_zscal(const int n,
const void* alpha,
void* x, const int x_inc);
void PUBLIC_API cblas_scopy(const int n,
const float* x, const int x_inc,
float* y, const int y_inc);
void PUBLIC_API cblas_dcopy(const int n,
const double* x, const int x_inc,
double* y, const int y_inc);
void PUBLIC_API cblas_ccopy(const int n,
const void* x, const int x_inc,
void* y, const int y_inc);
void PUBLIC_API cblas_zcopy(const int n,
const void* x, const int x_inc,
void* y, const int y_inc);
void PUBLIC_API cblas_saxpy(const int n,
const float alpha,
const float* x, const int x_inc,
float* y, const int y_inc);
void PUBLIC_API cblas_daxpy(const int n,
const double alpha,
const double* x, const int x_inc,
double* y, const int y_inc);
void PUBLIC_API cblas_caxpy(const int n,
const void* alpha,
const void* x, const int x_inc,
void* y, const int y_inc);
void PUBLIC_API cblas_zaxpy(const int n,
const void* alpha,
const void* x, const int x_inc,
void* y, const int y_inc);
float PUBLIC_API cblas_sdot(const int n,
const float* x, const int x_inc,
const float* y, const int y_inc);
double PUBLIC_API cblas_ddot(const int n,
const double* x, const int x_inc,
const double* y, const int y_inc);
void PUBLIC_API cblas_cdotu_sub(const int n,
const void* x, const int x_inc,
const void* y, const int y_inc,
void* dot);
void PUBLIC_API cblas_zdotu_sub(const int n,
const void* x, const int x_inc,
const void* y, const int y_inc,
void* dot);
void PUBLIC_API cblas_cdotc_sub(const int n,
const void* x, const int x_inc,
const void* y, const int y_inc,
void* dot);
void PUBLIC_API cblas_zdotc_sub(const int n,
const void* x, const int x_inc,
const void* y, const int y_inc,
void* dot);
float PUBLIC_API cblas_snrm2(const int n,
const float* x, const int x_inc);
double PUBLIC_API cblas_dnrm2(const int n,
const double* x, const int x_inc);
float PUBLIC_API cblas_scnrm2(const int n,
const void* x, const int x_inc);
double PUBLIC_API cblas_dznrm2(const int n,
const void* x, const int x_inc);
float PUBLIC_API cblas_sasum(const int n,
const float* x, const int x_inc);
double PUBLIC_API cblas_dasum(const int n,
const double* x, const int x_inc);
float PUBLIC_API cblas_scasum(const int n,
const void* x, const int x_inc);
double PUBLIC_API cblas_dzasum(const int n,
const void* x, const int x_inc);
float PUBLIC_API cblas_ssum(const int n,
const float* x, const int x_inc);
double PUBLIC_API cblas_dsum(const int n,
const double* x, const int x_inc);
float PUBLIC_API cblas_scsum(const int n,
const void* x, const int x_inc);
double PUBLIC_API cblas_dzsum(const int n,
const void* x, const int x_inc);
int PUBLIC_API cblas_isamax(const int n,
const float* x, const int x_inc);
int PUBLIC_API cblas_idamax(const int n,
const double* x, const int x_inc);
int PUBLIC_API cblas_icamax(const int n,
const void* x, const int x_inc);
int PUBLIC_API cblas_izamax(const int n,
const void* x, const int x_inc);
int PUBLIC_API cblas_isamin(const int n,
const float* x, const int x_inc);
int PUBLIC_API cblas_idamin(const int n,
const double* x, const int x_inc);
int PUBLIC_API cblas_icamin(const int n,
const void* x, const int x_inc);
int PUBLIC_API cblas_izamin(const int n,
const void* x, const int x_inc);
int PUBLIC_API cblas_ismax(const int n,
const float* x, const int x_inc);
int PUBLIC_API cblas_idmax(const int n,
const double* x, const int x_inc);
int PUBLIC_API cblas_icmax(const int n,
const void* x, const int x_inc);
int PUBLIC_API cblas_izmax(const int n,
const void* x, const int x_inc);
int PUBLIC_API cblas_ismin(const int n,
const float* x, const int x_inc);
int PUBLIC_API cblas_idmin(const int n,
const double* x, const int x_inc);
int PUBLIC_API cblas_icmin(const int n,
const void* x, const int x_inc);
int PUBLIC_API cblas_izmin(const int n,
const void* x, const int x_inc);
void PUBLIC_API cblas_sgemv(const CLBlastLayout layout, const CLBlastTranspose a_transpose,
const int m, const int n,
const float alpha,
const float* a, const int a_ld,
const float* x, const int x_inc,
const float beta,
float* y, const int y_inc);
void PUBLIC_API cblas_dgemv(const CLBlastLayout layout, const CLBlastTranspose a_transpose,
const int m, const int n,
const double alpha,
const double* a, const int a_ld,
const double* x, const int x_inc,
const double beta,
double* y, const int y_inc);
void PUBLIC_API cblas_cgemv(const CLBlastLayout layout, const CLBlastTranspose a_transpose,
const int m, const int n,
const void* alpha,
const void* a, const int a_ld,
const void* x, const int x_inc,
const void* beta,
void* y, const int y_inc);
void PUBLIC_API cblas_zgemv(const CLBlastLayout layout, const CLBlastTranspose a_transpose,
const int m, const int n,
const void* alpha,
const void* a, const int a_ld,
const void* x, const int x_inc,
const void* beta,
void* y, const int y_inc);
void PUBLIC_API cblas_sgbmv(const CLBlastLayout layout, const CLBlastTranspose a_transpose,
const int m, const int n, const int kl, const int ku,
const float alpha,
const float* a, const int a_ld,
const float* x, const int x_inc,
const float beta,
float* y, const int y_inc);
void PUBLIC_API cblas_dgbmv(const CLBlastLayout layout, const CLBlastTranspose a_transpose,
const int m, const int n, const int kl, const int ku,
const double alpha,
const double* a, const int a_ld,
const double* x, const int x_inc,
const double beta,
double* y, const int y_inc);
void PUBLIC_API cblas_cgbmv(const CLBlastLayout layout, const CLBlastTranspose a_transpose,
const int m, const int n, const int kl, const int ku,
const void* alpha,
const void* a, const int a_ld,
const void* x, const int x_inc,
const void* beta,
void* y, const int y_inc);
void PUBLIC_API cblas_zgbmv(const CLBlastLayout layout, const CLBlastTranspose a_transpose,
const int m, const int n, const int kl, const int ku,
const void* alpha,
const void* a, const int a_ld,
const void* x, const int x_inc,
const void* beta,
void* y, const int y_inc);
void PUBLIC_API cblas_chemv(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n,
const void* alpha,
const void* a, const int a_ld,
const void* x, const int x_inc,
const void* beta,
void* y, const int y_inc);
void PUBLIC_API cblas_zhemv(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n,
const void* alpha,
const void* a, const int a_ld,
const void* x, const int x_inc,
const void* beta,
void* y, const int y_inc);
void PUBLIC_API cblas_chbmv(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n, const int k,
const void* alpha,
const void* a, const int a_ld,
const void* x, const int x_inc,
const void* beta,
void* y, const int y_inc);
void PUBLIC_API cblas_zhbmv(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n, const int k,
const void* alpha,
const void* a, const int a_ld,
const void* x, const int x_inc,
const void* beta,
void* y, const int y_inc);
void PUBLIC_API cblas_chpmv(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n,
const void* alpha,
const void* ap,
const void* x, const int x_inc,
const void* beta,
void* y, const int y_inc);
void PUBLIC_API cblas_zhpmv(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n,
const void* alpha,
const void* ap,
const void* x, const int x_inc,
const void* beta,
void* y, const int y_inc);
void PUBLIC_API cblas_ssymv(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n,
const float alpha,
const float* a, const int a_ld,
const float* x, const int x_inc,
const float beta,
float* y, const int y_inc);
void PUBLIC_API cblas_dsymv(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n,
const double alpha,
const double* a, const int a_ld,
const double* x, const int x_inc,
const double beta,
double* y, const int y_inc);
void PUBLIC_API cblas_ssbmv(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n, const int k,
const float alpha,
const float* a, const int a_ld,
const float* x, const int x_inc,
const float beta,
float* y, const int y_inc);
void PUBLIC_API cblas_dsbmv(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n, const int k,
const double alpha,
const double* a, const int a_ld,
const double* x, const int x_inc,
const double beta,
double* y, const int y_inc);
void PUBLIC_API cblas_sspmv(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n,
const float alpha,
const float* ap,
const float* x, const int x_inc,
const float beta,
float* y, const int y_inc);
void PUBLIC_API cblas_dspmv(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n,
const double alpha,
const double* ap,
const double* x, const int x_inc,
const double beta,
double* y, const int y_inc);
void PUBLIC_API cblas_strmv(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int n,
const float* a, const int a_ld,
float* x, const int x_inc);
void PUBLIC_API cblas_dtrmv(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int n,
const double* a, const int a_ld,
double* x, const int x_inc);
void PUBLIC_API cblas_ctrmv(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int n,
const void* a, const int a_ld,
void* x, const int x_inc);
void PUBLIC_API cblas_ztrmv(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int n,
const void* a, const int a_ld,
void* x, const int x_inc);
void PUBLIC_API cblas_stbmv(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int n, const int k,
const float* a, const int a_ld,
float* x, const int x_inc);
void PUBLIC_API cblas_dtbmv(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int n, const int k,
const double* a, const int a_ld,
double* x, const int x_inc);
void PUBLIC_API cblas_ctbmv(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int n, const int k,
const void* a, const int a_ld,
void* x, const int x_inc);
void PUBLIC_API cblas_ztbmv(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int n, const int k,
const void* a, const int a_ld,
void* x, const int x_inc);
void PUBLIC_API cblas_stpmv(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int n,
const float* ap,
float* x, const int x_inc);
void PUBLIC_API cblas_dtpmv(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int n,
const double* ap,
double* x, const int x_inc);
void PUBLIC_API cblas_ctpmv(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int n,
const void* ap,
void* x, const int x_inc);
void PUBLIC_API cblas_ztpmv(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int n,
const void* ap,
void* x, const int x_inc);
void PUBLIC_API cblas_strsv(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int n,
const float* a, const int a_ld,
float* x, const int x_inc);
void PUBLIC_API cblas_dtrsv(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int n,
const double* a, const int a_ld,
double* x, const int x_inc);
void PUBLIC_API cblas_ctrsv(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int n,
const void* a, const int a_ld,
void* x, const int x_inc);
void PUBLIC_API cblas_ztrsv(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int n,
const void* a, const int a_ld,
void* x, const int x_inc);
void PUBLIC_API cblas_stbsv(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int n, const int k,
const float* a, const int a_ld,
float* x, const int x_inc);
void PUBLIC_API cblas_dtbsv(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int n, const int k,
const double* a, const int a_ld,
double* x, const int x_inc);
void PUBLIC_API cblas_ctbsv(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int n, const int k,
const void* a, const int a_ld,
void* x, const int x_inc);
void PUBLIC_API cblas_ztbsv(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int n, const int k,
const void* a, const int a_ld,
void* x, const int x_inc);
void PUBLIC_API cblas_stpsv(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int n,
const float* ap,
float* x, const int x_inc);
void PUBLIC_API cblas_dtpsv(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int n,
const double* ap,
double* x, const int x_inc);
void PUBLIC_API cblas_ctpsv(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int n,
const void* ap,
void* x, const int x_inc);
void PUBLIC_API cblas_ztpsv(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int n,
const void* ap,
void* x, const int x_inc);
void PUBLIC_API cblas_sger(const CLBlastLayout layout,
const int m, const int n,
const float alpha,
const float* x, const int x_inc,
const float* y, const int y_inc,
float* a, const int a_ld);
void PUBLIC_API cblas_dger(const CLBlastLayout layout,
const int m, const int n,
const double alpha,
const double* x, const int x_inc,
const double* y, const int y_inc,
double* a, const int a_ld);
void PUBLIC_API cblas_cgeru(const CLBlastLayout layout,
const int m, const int n,
const void* alpha,
const void* x, const int x_inc,
const void* y, const int y_inc,
void* a, const int a_ld);
void PUBLIC_API cblas_zgeru(const CLBlastLayout layout,
const int m, const int n,
const void* alpha,
const void* x, const int x_inc,
const void* y, const int y_inc,
void* a, const int a_ld);
void PUBLIC_API cblas_cgerc(const CLBlastLayout layout,
const int m, const int n,
const void* alpha,
const void* x, const int x_inc,
const void* y, const int y_inc,
void* a, const int a_ld);
void PUBLIC_API cblas_zgerc(const CLBlastLayout layout,
const int m, const int n,
const void* alpha,
const void* x, const int x_inc,
const void* y, const int y_inc,
void* a, const int a_ld);
void PUBLIC_API cblas_cher(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n,
const float alpha,
const void* x, const int x_inc,
void* a, const int a_ld);
void PUBLIC_API cblas_zher(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n,
const double alpha,
const void* x, const int x_inc,
void* a, const int a_ld);
void PUBLIC_API cblas_chpr(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n,
const float alpha,
const void* x, const int x_inc,
void* ap);
void PUBLIC_API cblas_zhpr(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n,
const double alpha,
const void* x, const int x_inc,
void* ap);
void PUBLIC_API cblas_cher2(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n,
const void* alpha,
const void* x, const int x_inc,
const void* y, const int y_inc,
void* a, const int a_ld);
void PUBLIC_API cblas_zher2(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n,
const void* alpha,
const void* x, const int x_inc,
const void* y, const int y_inc,
void* a, const int a_ld);
void PUBLIC_API cblas_chpr2(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n,
const void* alpha,
const void* x, const int x_inc,
const void* y, const int y_inc,
void* ap);
void PUBLIC_API cblas_zhpr2(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n,
const void* alpha,
const void* x, const int x_inc,
const void* y, const int y_inc,
void* ap);
void PUBLIC_API cblas_ssyr(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n,
const float alpha,
const float* x, const int x_inc,
float* a, const int a_ld);
void PUBLIC_API cblas_dsyr(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n,
const double alpha,
const double* x, const int x_inc,
double* a, const int a_ld);
void PUBLIC_API cblas_sspr(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n,
const float alpha,
const float* x, const int x_inc,
float* ap);
void PUBLIC_API cblas_dspr(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n,
const double alpha,
const double* x, const int x_inc,
double* ap);
void PUBLIC_API cblas_ssyr2(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n,
const float alpha,
const float* x, const int x_inc,
const float* y, const int y_inc,
float* a, const int a_ld);
void PUBLIC_API cblas_dsyr2(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n,
const double alpha,
const double* x, const int x_inc,
const double* y, const int y_inc,
double* a, const int a_ld);
void PUBLIC_API cblas_sspr2(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n,
const float alpha,
const float* x, const int x_inc,
const float* y, const int y_inc,
float* ap);
void PUBLIC_API cblas_dspr2(const CLBlastLayout layout, const CLBlastTriangle triangle,
const int n,
const double alpha,
const double* x, const int x_inc,
const double* y, const int y_inc,
double* ap);
void PUBLIC_API cblas_sgemm(const CLBlastLayout layout, const CLBlastTranspose a_transpose, const CLBlastTranspose b_transpose,
const int m, const int n, const int k,
const float alpha,
const float* a, const int a_ld,
const float* b, const int b_ld,
const float beta,
float* c, const int c_ld);
void PUBLIC_API cblas_dgemm(const CLBlastLayout layout, const CLBlastTranspose a_transpose, const CLBlastTranspose b_transpose,
const int m, const int n, const int k,
const double alpha,
const double* a, const int a_ld,
const double* b, const int b_ld,
const double beta,
double* c, const int c_ld);
void PUBLIC_API cblas_cgemm(const CLBlastLayout layout, const CLBlastTranspose a_transpose, const CLBlastTranspose b_transpose,
const int m, const int n, const int k,
const void* alpha,
const void* a, const int a_ld,
const void* b, const int b_ld,
const void* beta,
void* c, const int c_ld);
void PUBLIC_API cblas_zgemm(const CLBlastLayout layout, const CLBlastTranspose a_transpose, const CLBlastTranspose b_transpose,
const int m, const int n, const int k,
const void* alpha,
const void* a, const int a_ld,
const void* b, const int b_ld,
const void* beta,
void* c, const int c_ld);
void PUBLIC_API cblas_ssymm(const CLBlastLayout layout, const CLBlastSide side, const CLBlastTriangle triangle,
const int m, const int n,
const float alpha,
const float* a, const int a_ld,
const float* b, const int b_ld,
const float beta,
float* c, const int c_ld);
void PUBLIC_API cblas_dsymm(const CLBlastLayout layout, const CLBlastSide side, const CLBlastTriangle triangle,
const int m, const int n,
const double alpha,
const double* a, const int a_ld,
const double* b, const int b_ld,
const double beta,
double* c, const int c_ld);
void PUBLIC_API cblas_csymm(const CLBlastLayout layout, const CLBlastSide side, const CLBlastTriangle triangle,
const int m, const int n,
const void* alpha,
const void* a, const int a_ld,
const void* b, const int b_ld,
const void* beta,
void* c, const int c_ld);
void PUBLIC_API cblas_zsymm(const CLBlastLayout layout, const CLBlastSide side, const CLBlastTriangle triangle,
const int m, const int n,
const void* alpha,
const void* a, const int a_ld,
const void* b, const int b_ld,
const void* beta,
void* c, const int c_ld);
void PUBLIC_API cblas_chemm(const CLBlastLayout layout, const CLBlastSide side, const CLBlastTriangle triangle,
const int m, const int n,
const void* alpha,
const void* a, const int a_ld,
const void* b, const int b_ld,
const void* beta,
void* c, const int c_ld);
void PUBLIC_API cblas_zhemm(const CLBlastLayout layout, const CLBlastSide side, const CLBlastTriangle triangle,
const int m, const int n,
const void* alpha,
const void* a, const int a_ld,
const void* b, const int b_ld,
const void* beta,
void* c, const int c_ld);
void PUBLIC_API cblas_ssyrk(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose,
const int n, const int k,
const float alpha,
const float* a, const int a_ld,
const float beta,
float* c, const int c_ld);
void PUBLIC_API cblas_dsyrk(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose,
const int n, const int k,
const double alpha,
const double* a, const int a_ld,
const double beta,
double* c, const int c_ld);
void PUBLIC_API cblas_csyrk(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose,
const int n, const int k,
const void* alpha,
const void* a, const int a_ld,
const void* beta,
void* c, const int c_ld);
void PUBLIC_API cblas_zsyrk(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose,
const int n, const int k,
const void* alpha,
const void* a, const int a_ld,
const void* beta,
void* c, const int c_ld);
void PUBLIC_API cblas_cherk(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose,
const int n, const int k,
const float alpha,
const void* a, const int a_ld,
const float beta,
void* c, const int c_ld);
void PUBLIC_API cblas_zherk(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose,
const int n, const int k,
const double alpha,
const void* a, const int a_ld,
const double beta,
void* c, const int c_ld);
void PUBLIC_API cblas_ssyr2k(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose ab_transpose,
const int n, const int k,
const float alpha,
const float* a, const int a_ld,
const float* b, const int b_ld,
const float beta,
float* c, const int c_ld);
void PUBLIC_API cblas_dsyr2k(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose ab_transpose,
const int n, const int k,
const double alpha,
const double* a, const int a_ld,
const double* b, const int b_ld,
const double beta,
double* c, const int c_ld);
void PUBLIC_API cblas_csyr2k(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose ab_transpose,
const int n, const int k,
const void* alpha,
const void* a, const int a_ld,
const void* b, const int b_ld,
const void* beta,
void* c, const int c_ld);
void PUBLIC_API cblas_zsyr2k(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose ab_transpose,
const int n, const int k,
const void* alpha,
const void* a, const int a_ld,
const void* b, const int b_ld,
const void* beta,
void* c, const int c_ld);
void PUBLIC_API cblas_cher2k(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose ab_transpose,
const int n, const int k,
const void* alpha,
const void* a, const int a_ld,
const void* b, const int b_ld,
const float beta,
void* c, const int c_ld);
void PUBLIC_API cblas_zher2k(const CLBlastLayout layout, const CLBlastTriangle triangle, const CLBlastTranspose ab_transpose,
const int n, const int k,
const void* alpha,
const void* a, const int a_ld,
const void* b, const int b_ld,
const double beta,
void* c, const int c_ld);
void PUBLIC_API cblas_strmm(const CLBlastLayout layout, const CLBlastSide side, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int m, const int n,
const float alpha,
const float* a, const int a_ld,
float* b, const int b_ld);
void PUBLIC_API cblas_dtrmm(const CLBlastLayout layout, const CLBlastSide side, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int m, const int n,
const double alpha,
const double* a, const int a_ld,
double* b, const int b_ld);
void PUBLIC_API cblas_ctrmm(const CLBlastLayout layout, const CLBlastSide side, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int m, const int n,
const void* alpha,
const void* a, const int a_ld,
void* b, const int b_ld);
void PUBLIC_API cblas_ztrmm(const CLBlastLayout layout, const CLBlastSide side, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int m, const int n,
const void* alpha,
const void* a, const int a_ld,
void* b, const int b_ld);
void PUBLIC_API cblas_strsm(const CLBlastLayout layout, const CLBlastSide side, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int m, const int n,
const float alpha,
const float* a, const int a_ld,
float* b, const int b_ld);
void PUBLIC_API cblas_dtrsm(const CLBlastLayout layout, const CLBlastSide side, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int m, const int n,
const double alpha,
const double* a, const int a_ld,
double* b, const int b_ld);
void PUBLIC_API cblas_ctrsm(const CLBlastLayout layout, const CLBlastSide side, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int m, const int n,
const void* alpha,
const void* a, const int a_ld,
void* b, const int b_ld);
void PUBLIC_API cblas_ztrsm(const CLBlastLayout layout, const CLBlastSide side, const CLBlastTriangle triangle, const CLBlastTranspose a_transpose, const CLBlastDiagonal diagonal,
const int m, const int n,
const void* alpha,
const void* a, const int a_ld,
void* b, const int b_ld);
void PUBLIC_API cblas_shad(const int n,
const float alpha,
const float* x, const int x_inc,
const float* y, const int y_inc,
const float beta,
float* z, const int z_inc);
void PUBLIC_API cblas_dhad(const int n,
const double alpha,
const double* x, const int x_inc,
const double* y, const int y_inc,
const double beta,
double* z, const int z_inc);
void PUBLIC_API cblas_chad(const int n,
const void* alpha,
const void* x, const int x_inc,
const void* y, const int y_inc,
const void* beta,
void* z, const int z_inc);
void PUBLIC_API cblas_zhad(const int n,
const void* alpha,
const void* x, const int x_inc,
const void* y, const int y_inc,
const void* beta,
void* z, const int z_inc);
void PUBLIC_API cblas_somatcopy(const CLBlastLayout layout, const CLBlastTranspose a_transpose,
const int m, const int n,
const float alpha,
const float* a, const int a_ld,
float* b, const int b_ld);
void PUBLIC_API cblas_domatcopy(const CLBlastLayout layout, const CLBlastTranspose a_transpose,
const int m, const int n,
const double alpha,
const double* a, const int a_ld,
double* b, const int b_ld);
void PUBLIC_API cblas_comatcopy(const CLBlastLayout layout, const CLBlastTranspose a_transpose,
const int m, const int n,
const void* alpha,
const void* a, const int a_ld,
void* b, const int b_ld);
void PUBLIC_API cblas_zomatcopy(const CLBlastLayout layout, const CLBlastTranspose a_transpose,
const int m, const int n,
const void* alpha,
const void* a, const int a_ld,
void* b, const int b_ld);
void PUBLIC_API cblas_sim2col(const CLBlastKernelMode kernel_mode,
const int channels, const int height, const int width, const int kernel_h, const int kernel_w, const int pad_h, const int pad_w, const int stride_h, const int stride_w, const int dilation_h, const int dilation_w,
const float* im,
float* col);
void PUBLIC_API cblas_dim2col(const CLBlastKernelMode kernel_mode,
const int channels, const int height, const int width, const int kernel_h, const int kernel_w, const int pad_h, const int pad_w, const int stride_h, const int stride_w, const int dilation_h, const int dilation_w,
const double* im,
double* col);
void PUBLIC_API cblas_cim2col(const CLBlastKernelMode kernel_mode,
const int channels, const int height, const int width, const int kernel_h, const int kernel_w, const int pad_h, const int pad_w, const int stride_h, const int stride_w, const int dilation_h, const int dilation_w,
const void* im,
void* col);
void PUBLIC_API cblas_zim2col(const CLBlastKernelMode kernel_mode,
const int channels, const int height, const int width, const int kernel_h, const int kernel_w, const int pad_h, const int pad_w, const int stride_h, const int stride_w, const int dilation_h, const int dilation_w,
const void* im,
void* col);
void PUBLIC_API cblas_scol2im(const CLBlastKernelMode kernel_mode,
const int channels, const int height, const int width, const int kernel_h, const int kernel_w, const int pad_h, const int pad_w, const int stride_h, const int stride_w, const int dilation_h, const int dilation_w,
const float* col,
float* im);
void PUBLIC_API cblas_dcol2im(const CLBlastKernelMode kernel_mode,
const int channels, const int height, const int width, const int kernel_h, const int kernel_w, const int pad_h, const int pad_w, const int stride_h, const int stride_w, const int dilation_h, const int dilation_w,
const double* col,
double* im);
void PUBLIC_API cblas_ccol2im(const CLBlastKernelMode kernel_mode,
const int channels, const int height, const int width, const int kernel_h, const int kernel_w, const int pad_h, const int pad_w, const int stride_h, const int stride_w, const int dilation_h, const int dilation_w,
const void* col,
void* im);
void PUBLIC_API cblas_zcol2im(const CLBlastKernelMode kernel_mode,
const int channels, const int height, const int width, const int kernel_h, const int kernel_w, const int pad_h, const int pad_w, const int stride_h, const int stride_w, const int dilation_h, const int dilation_w,
const void* col,
void* im);
#ifdef __cplusplus
}
#endif
#endif