#!/bin/sh

# ------------------------------
# config
# ------------------------------
dest=$PWD/dest
src=$PWD/src

# ------------------------------
# function
# ------------------------------
function check(){
    cmd=$1
    /bin/echo -n "cheking $1 ... "
    if which $1 > /dev/null; then
	/bin/echo "ok"
    else
	/bin/echo "not found"
	exit 1
    fi
}

function exists(){
    /bin/echo -n "check $1 ... "
    if [[ -e $1 ]]; then
	echo "ok"
    else
	echo "not found"
	exit 1
    fi
}

trap 'exit 1' 2

echo ------------------------------
echo cleanup
echo ------------------------------
rm -rf $dest
rm -f summary.log
rm -f unittest.log
rm -f integrate.log
rm -f build.log

echo ------------------------------
echo check
echo ------------------------------
check git
check ocaml
check ocamlfind
check omake
check avmplus
check swfmill

echo ------------------------------
echo check out
echo ------------------------------
if [[ $SKIP -ne 1 ]]; then
    rm -rf $src
    git clone --depth 1 git://github.com/mzp/scheme-abc.git $src
    cd $src
else
    cd $src
    echo "pull"
    git pull
fi

echo ------------------------------
echo build
echo ------------------------------
git clean -fx
omake config PREFIX=$dest
omake all
echo "build:$?" >> summary.log

echo ------------------------------
echo unit test
echo ------------------------------
omake check-detail
echo "unittest:$?" >> summary.log
mv unittest.log ..

echo ------------------------------
echo integrate test
echo ------------------------------
omake integrate
echo "integrate:$?" >> summary.log
mv integrate.log ..

echo ------------------------------
echo swf test
echo ------------------------------
omake install

cd $dest
exists bin/habc     | tee swf.log
exists bin/habc-scm | tee -a swf.log
exists bin/habc-xml | tee -a swf.log
exists bin/habc-xml | tee -a swf.log

exists lib/habc/std.ho | tee -a swf.log
exists lib/habc/stub.stub.ho | tee -a swf.log
exists lib/habc/flash.stub.ho | tee -a swf.log

echo ------------------------------
echo make webpage
echo ------------------------------
