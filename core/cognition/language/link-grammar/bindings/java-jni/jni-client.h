#include <jni.h>
#ifndef _LinkGrammar_H_
#define _LinkGrammar_H_
#ifdef __cplusplus
extern "C" {
#endif
JNIEXPORT void JNICALL Java_org_linkgrammar_LinkGrammar_setMaxParseSeconds
(JNIEnv *, jclass, jint);
JNIEXPORT void JNICALL Java_org_linkgrammar_LinkGrammar_setMaxCost
(JNIEnv *, jclass, jdouble);
JNIEXPORT void JNICALL Java_org_linkgrammar_LinkGrammar_setDictionariesPath
(JNIEnv *, jclass, jstring);
JNIEXPORT void JNICALL Java_org_linkgrammar_LinkGrammar_setLanguage
(JNIEnv *, jclass, jstring);
JNIEXPORT void JNICALL Java_org_linkgrammar_LinkGrammar_setMaxLinkages
(JNIEnv *, jclass, jint);
JNIEXPORT jint JNICALL Java_org_linkgrammar_LinkGrammar_getMaxLinkages
(JNIEnv *, jclass);
JNIEXPORT void JNICALL Java_org_linkgrammar_LinkGrammar_init
(JNIEnv *, jclass);
JNIEXPORT void JNICALL Java_org_linkgrammar_LinkGrammar_parse
(JNIEnv *, jclass, jstring);
JNIEXPORT void unit_test_jparse(JNIEnv *, const char*);
JNIEXPORT void JNICALL Java_org_linkgrammar_LinkGrammar_close
(JNIEnv *, jclass);
JNIEXPORT void JNICALL Java_org_linkgrammar_LinkGrammar_doFinalize
(JNIEnv *, jclass);
JNIEXPORT jint JNICALL Java_org_linkgrammar_LinkGrammar_getNumWords
(JNIEnv *, jclass);
JNIEXPORT jstring JNICALL Java_org_linkgrammar_LinkGrammar_getLinkageWord
(JNIEnv *, jclass, jint);
JNIEXPORT jstring JNICALL Java_org_linkgrammar_LinkGrammar_getLinkageDisjunct
(JNIEnv *, jclass, jint);
JNIEXPORT jstring JNICALL
Java_org_linkgrammar_LinkGrammar_getVersion(JNIEnv *, jclass);
JNIEXPORT jstring JNICALL
Java_org_linkgrammar_LinkGrammar_getDictVersion(JNIEnv *, jclass);
JNIEXPORT jint JNICALL Java_org_linkgrammar_LinkGrammar_getNumSkippedWords
(JNIEnv *, jclass);
JNIEXPORT jint JNICALL Java_org_linkgrammar_LinkGrammar_getNumLinkages
(JNIEnv *, jclass);
JNIEXPORT void JNICALL Java_org_linkgrammar_LinkGrammar_makeLinkage
(JNIEnv *, jclass, jint);
JNIEXPORT jint JNICALL Java_org_linkgrammar_LinkGrammar_getLinkageNumViolations
(JNIEnv *, jclass);
JNIEXPORT jdouble JNICALL Java_org_linkgrammar_LinkGrammar_getLinkageDisjunctCost
(JNIEnv *, jclass);
JNIEXPORT jdouble JNICALL Java_org_linkgrammar_LinkGrammar_getLinkageLinkCost
(JNIEnv *, jclass);
JNIEXPORT jint JNICALL Java_org_linkgrammar_LinkGrammar_getNumLinks
(JNIEnv *, jclass);
JNIEXPORT jint JNICALL Java_org_linkgrammar_LinkGrammar_getLinkLWord
(JNIEnv *, jclass, jint);
JNIEXPORT jint JNICALL Java_org_linkgrammar_LinkGrammar_getLinkRWord
(JNIEnv *, jclass, jint);
JNIEXPORT jstring JNICALL Java_org_linkgrammar_LinkGrammar_getLinkLLabel
(JNIEnv *, jclass, jint);
JNIEXPORT jstring JNICALL Java_org_linkgrammar_LinkGrammar_getLinkRLabel
(JNIEnv *, jclass, jint);
JNIEXPORT jstring JNICALL Java_org_linkgrammar_LinkGrammar_getLinkLabel
(JNIEnv *, jclass, jint);
JNIEXPORT jstring JNICALL Java_org_linkgrammar_LinkGrammar_getConstituentString
(JNIEnv *, jclass);
JNIEXPORT jstring JNICALL Java_org_linkgrammar_LinkGrammar_getLinkString
(JNIEnv *, jclass);
#ifdef __cplusplus
}
#endif
#endif