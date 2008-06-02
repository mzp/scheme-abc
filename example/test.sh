#!/bin/sh
for file in $@; do
    echo -n $file "..."
    # generate expected output
    sed -n 's/;;; *//p' $file > $file.expect
    
    # compile and execute
    ocamlbuild -quiet main.byte -- $file
    avmplus a.abc > $file.actual

    # compare
    diff $file.expect $file.actual
    result=$?

    rm -rf $file.expect $file.actual

    if (($result!=0)); then
	echo "error"
	exit 1
    else
	echo "ok"
    fi
done