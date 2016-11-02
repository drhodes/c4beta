# need to cat a bunch of js.  this is an ugly, ugly hack to run tests
# from the cmd line with node.

SIMDIR=./bsim/bsim/simulation
LIBDIR=./bsim/libs

cat $LIBDIR/underscore.js \
    $LIBDIR/backbone.js \
    $SIMDIR/global.js \
    $SIMDIR/beta.js \
    $SIMDIR/bsim_common.js \
    $SIMDIR/beta_instructions.js \
    $SIMDIR/memory.js \
    $SIMDIR/big_table.js \
    $SIMDIR/regfile_view.js \
    $SIMDIR/tty.js \
    > testblob.js

node testblob.js
