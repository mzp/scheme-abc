#!/bin/sh
for file in $@; do
    /bin/echo -n "${file}..."
    # generate expected output
    sed -n 's/;;; *//p' $file > $file.expect
    
    # compile and execute
    src/aosh $file
    avmplus a.abc > $file.actual

    # compare
    diff $file.expect $file.actual > $file.diff

    result=$?

    if [ $result != 0 ]; then
	echo "error"
	cat $file.diff
	exit 1
    else
	echo "ok"
    fi

    rm -f $file.expect $file.actual $file.diff    
done
