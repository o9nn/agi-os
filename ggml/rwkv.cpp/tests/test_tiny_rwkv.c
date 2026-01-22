#include <stdlib.h>
#include <stdio.h>
#include <rwkv.h>
#include "logit_difference_validator.inc"
#define VERSION_COUNT 5
#define FORMAT_COUNT 7
int main(void) {
late_abort = true;
fprintf(stderr, "System info: %s\n", rwkv_get_system_info_string());
rwkv_set_print_errors(NULL, false);
const char * versions[VERSION_COUNT] = {
"4v0-660K",
"5v1-730K",
"5v2-730K",
"6v0-3m",
"7v0-834K"
};
const char * formats[FORMAT_COUNT] = {
"FP32",
"FP16",
"Q4_0",
"Q4_1",
"Q5_0",
"Q5_1",
"Q8_0"
};
const float expected_difference_sum_full[VERSION_COUNT * 2] = {
+0.001000F,
-0.013652F,
+0.001000F,
-0.289921F,
+0.001000F,
+0.455912F,
+0.001000F,
-0.416620F,
+0.001000F,
+0.005766F
};
const float expected_difference_sum_quantized_FP32[VERSION_COUNT * (FORMAT_COUNT - 2)] = {
-000.160030F,
-000.547409F,
-000.170404F,
+000.278034F,
+000.076282F,
+117.932594F,
-026.712271F,
-163.439407F,
-018.017435F,
+000.585238F,
+035.271305F,
+067.015076F,
+025.273308F,
+048.068733F,
-009.441034F,
-007.588121F,
+021.939022F,
-027.332073F,
+003.576909F,
-009.539596F,
+000.136785F,
+000.002614F,
-000.063645F,
-000.064663F,
+000.011924F
};
const float expected_difference_sum_quantized_FP16[VERSION_COUNT * (FORMAT_COUNT - 2)] = {
+000.154614F,
-000.539827F,
-000.180142F,
+000.294953F,
+000.077226F,
+119.471931F,
-028.245888F,
-159.870956F,
-039.708530F,
-000.962695F,
+034.135971F,
+065.573822F,
+021.588751F,
+029.726818F,
-007.242277F,
-007.660988F,
+021.797060F,
-027.269241F,
+003.405264F,
-009.734720F,
+000.136678F,
-000.005140F,
-000.064447F,
-000.063531F,
+000.010921F
};
for (int i_version = 0; i_version < VERSION_COUNT; i_version++) {
float * expected_logits = calloc(N_VOCAB, sizeof(float));
load_expected_logits(expected_logits, versions[i_version]);
for (int i_format = 0; i_format < FORMAT_COUNT; i_format++) {
if (i_format < 2) {
test_model(versions[i_version], formats[i_format], expected_logits, expected_difference_sum_full[i_version * 2 + i_format]);
continue;
}
char source_file_name[128];
char dest_format[32];
char dest_file_name[128];
snprintf(source_file_name, sizeof(source_file_name), "tiny-rwkv-%s-FP32.bin", versions[i_version]);
snprintf(dest_format, sizeof(dest_format), "FP32-to-%s", formats[i_format]);
snprintf(dest_file_name, sizeof(dest_file_name), "tiny-rwkv-%s-%s.bin", versions[i_version], dest_format);
rwkv_quantize_model_file(source_file_name, dest_file_name, formats[i_format]);
test_model(versions[i_version], dest_format, expected_logits, expected_difference_sum_quantized_FP32[i_version * (FORMAT_COUNT - 2) + (i_format - 2)]);
snprintf(source_file_name, sizeof(source_file_name), "tiny-rwkv-%s-FP16.bin", versions[i_version]);
snprintf(dest_format, sizeof(dest_format), "FP16-to-%s", formats[i_format]);
snprintf(dest_file_name, sizeof(dest_file_name), "tiny-rwkv-%s-%s.bin", versions[i_version], dest_format);
rwkv_quantize_model_file(source_file_name, dest_file_name, formats[i_format]);
test_model(versions[i_version], dest_format, expected_logits, expected_difference_sum_quantized_FP16[i_version * (FORMAT_COUNT - 2) + (i_format - 2)]);
}
free(expected_logits);
}
if (must_abort) {
abort();
}
return 0;
}