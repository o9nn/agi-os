#include <testlib.h>
int main(int argc, char *argv[], int envc, char *envp[])
{
int ret = printf("hello!!\n");
ASSERT_RET(ret, "printf() should return 0 here");
return 0;
}