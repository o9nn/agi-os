#! /bin/bash
FLODIR=/home/linas/src/fractal/image/
BINDIR=/home/linas/src/fractal/generate/
$BINDIR/renorm scat-dcos ren 1
cat ren.flo | $FLODIR/flo2mtv |mtvtoppm | pnmtopng > scat-dcos.png