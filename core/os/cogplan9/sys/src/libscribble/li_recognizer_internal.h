#define MAXSCLASSES 100
typedef struct PointList {
Stroke;
int xrange, yrange;
struct PointList* next;
} point_list;
typedef struct {
char* file_name;
int nclasses;
point_list* ex[MAXSCLASSES];
char* cnames[MAXSCLASSES];
point_list* canonex[MAXSCLASSES];
point_list* dompts[MAXSCLASSES];
} rClassifier;
typedef struct {
uint li_magic;
rClassifier li_rc;
} li_recognizer;
#define LI_DEFAULT_CLASSIFIER_FILE "default.cl"
#define LI_CLASSIFIER_EXTENSION ".cl"
#define LI_SUPPORTED_LOCALE REC_DEFAULT_LOCALE