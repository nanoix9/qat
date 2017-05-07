#!/bin/bash

bin_dir=$(dirname $0)
qati=$bin_dir/../qati.byte

for file in $(ls *.qat); do
    echo "executing file $file ..."
    $qati $file
done
