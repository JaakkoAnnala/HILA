#!/bin/bash

export ERRORLOG=$(mktemp /tmp/abc-script.XXXXXX)
export fails=0
export num_tests=0

MAKEFILE=Makefile_gpu

check(){
    if [ $? -eq 0 ];
    then
        echo -e "$1 $test \e[36msuccess\e[39m"
    else
        echo -e "$1 $test \e[31mfailed\e[39m"
        echo -e " * $1 $test" >> $ERRORLOG
        export fails=$((fails+1))
    fi
    export num_tests=$((num_tests+1))
}

transform_c(){
    echo ../../build/transformer -DGPU --CUDA $1
    ../../build/transformer -DGPU --CUDA $1 2>/dev/null
    check transform
}

compile_c(){
    make -f ${MAKEFILE} -s ${1}.exe 2>/dev/null
    check compile
}

run_c(){
    echo ./$1
    ./${1}.exe
    check run
}

# Get a list from command line arguments or list all test files
if [ "$#" -gt 0 ]; then
    tests=( "$@" )
else
    tests=$(ls test_*.cpp  )
fi

make -f ${MAKEFILE} cleanall
for D in 1 2 3 4 ; do
  sed -i 's/OPTS = .*/OPTS = -DNDIM='${D}'/' Makefile
  for testfile in $tests; do
    test="${testfile%.*}"
    echo $test
    transform_c ${test}.cpp
    compile_c ${test}
    run_c ${test}
    rm ${test}.exe ${test}.cpt
  done
  make -f ${MAKEFILE} clean
done

if [ ${fails} -eq 0 ]; then
	echo -e "\e[36m All tests passed \e[39m"
else
	echo ${fails}/${num_tests} tests failed
	cat $ERRORLOG
fi

rm $ERRORLOG
exit 0