#define NUM_RECS 3
#define DEFAULT_REC_DIR "classsifiers"
#define REC_DEFAULT_USER_DIR "/sys/lib/scribble/classifiers"
#define CLASSIFIER_DIR "lib/classifiers"
#define DEFAULT_LETTERS_FILE "letters.cl"
#define DEFAULT_DIGITS_FILE "digits.cl"
#define DEFAULT_PUNC_FILE "punc.cl"
struct graffiti {
recognizer rec[3];
char cldir[200];
li_recognizer_train rec_train;
li_recognizer_getClasses rec_getClasses;
};
extern char *cl_name[3];