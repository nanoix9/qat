#!/bin/bash

bin_dir=$(dirname $0)
cd $bin_dir
qati=../qati.byte

for file in $(ls *.qat); do
    echo "executing file $file ..."
    if $qati $file | diff - ${file/%.qat/.out}; then
        echo "PASSED"
    else
        echo "FAILED"
    fi
done
