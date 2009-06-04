#!/bin/sh
ROOT=$1
shift
for file in $@; do
    /bin/echo -n "${file}..."
    # generate expected output
    sed -n 's/;;; *//p' $file | sed "s/\r\n/\n/g" > $file.expect

    # compile and execute
    ${ROOT}/scm/habc-scm -I "${ROOT}/lib/habc:." std.ho $file

    if [ $? != 0 ]; then
	exit 1
    fi

    avmplus a.abc | sed "s/\r\n/\n/g" > $file.actual

    # compare
    diff $file.expect $file.actual > $file.diff
    cmp  $file.expect $file.actual

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
