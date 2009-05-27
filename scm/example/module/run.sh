ROOT=$1
cd $(dirname $0)
${ROOT}/scm/habc-scm -c -I "${ROOT}/lib/habc" fact.scm
${ROOT}/habc-scm -I "${ROOT}/lib/habc/:." fact.ho main.scm
avmplus a.abc
