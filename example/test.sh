#!/bin/sh
for file in $@; do
    echo -n $file "..."
    # generate expected output
    sed -n 's/;;; *//p' $file > $file.expect
    
    # compile and execute
    ocamlbuild -quiet main.byte -- $file
    avmplus a.abc > $file.actual

    # compare
    diff $file.expect $file.actual > $file.diff
    result=$?

    if (($result!=0)); then
	echo "error"
	cat $file.diff
    else
	echo "ok"
    fi

    rm -f $file.expect $file.actual $file.diff

done