perl -p -i -e 's/^A/]/g' $1
perl -p -i -e 's/^[aeo]/|/g' $1
perl -p -i -e 's/[aeo]//g' $1
perl -p -i -e 's/~//g' $1