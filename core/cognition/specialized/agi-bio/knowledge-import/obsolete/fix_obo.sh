#!/bin/bash
sed -i -e 's/(/\[/g' go.obo
sed -i -e 's/)/]/g' go.obo
python GO_scm.py
sed -i -e 's/\"/"/g' GO.scm
sed -i '1839165,1839469d' GO.scm