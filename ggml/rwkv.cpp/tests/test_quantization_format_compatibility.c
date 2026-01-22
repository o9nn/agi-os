#include <stdlib.h>
#include <stdio.h>
#include <rwkv.h>
#include "logit_difference_validator.inc"
#define VERSION_COUNT 4
int main(void) {
fprintf(stderr, "System info: %s\n", rwkv_get_system_info_string());
const char * versions[VERSION_COUNT] = {
"4v0-660K",
"5v1-730K",
"5v2-730K",
"6v0-3m"
};
const float differences[VERSION_COUNT * 2] = {
-000.170404F,
+000.278034F,
-163.439407F,
-018.017435F,
+025.273308F,
+048.068733F,
-021.151785F,
+003.576909F
};
for (int i = 0; i < VERSION_COUNT; i++) {
float * expected_logits = calloc(N_VOCAB, sizeof(float));
load_expected_logits(expected_logits, versions[i]);
test_model(versions[i], "Q5_0", expected_logits, differences[i * 2 + 0]);
test_model(versions[i], "Q5_1", expected_logits, differences[i * 2 + 1]);
free(expected_logits);
}
return 0;
}