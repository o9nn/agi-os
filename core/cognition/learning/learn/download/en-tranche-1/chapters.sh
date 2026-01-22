#! /bin/bash
cat $1 | sed ':a;N;$!ba;s/\n/xxx-foo-xxx/g' > xxx
cat xxx |sed 's/xxx-foo-xxx\rxxx-foo-xxx/\n\x0b\n/g' > yyy
cat yyy |sed 's/\rxxx-foo-xxx/\n/g' > zzz
split -l 50 -t '' --filter=' sed "s///g" > $FILE' zzz $2
rm xxx yyy zzz