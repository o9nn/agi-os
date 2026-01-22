#!/bin/bash
f1_full_name=$1
f1_name="${f1_full_name%.*}"
f2_full_name=$2
f2_name="${f2_full_name%.*}"
target_name="${f1_name}__${f2_name}.metta"
rm -f ${target_name}
grep Contains ${f1_full_name} | grep -v ":" > ${target_name}
grep EVALUATION ${f1_full_name} >> ${target_name}
grep Contains ${f2_full_name} | grep -v ":" >> ${target_name}
grep EVALUATION ${f2_full_name} >> ${target_name}