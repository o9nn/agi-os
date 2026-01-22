#! /bin/bash
export DICT_DIR=$TEXT_DIR/fake-lang
export GEN_CORPUS_DIR=$TEXT_DIR/fake-corpus
export $CONFIG_DIR/DICT_CONF=1-dict-conf.scm
export SENT_SHORTEST=3
export SENT_LONGEST=12
export NUM_SENTENCES=25000