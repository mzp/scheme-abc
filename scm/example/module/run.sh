ROOT=$1
cd $(dirname $0)
${ROOT}/scm/habc-scm -c -I "${ROOT}/lib/habc:." fact.scm
${ROOT}/habc-scm -I "${ROOT}/lib/habc/:."  fact.ho  main.scm
res=$(avmplus a.abc)

if [[ $res == 3628800 ]]; then
    echo "module...ok"
else
    echo "module...ng"
fi
