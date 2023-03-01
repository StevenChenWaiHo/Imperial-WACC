#!/bin/bash
ERRORS=0
TOTAL=0
for f in $(find $1 -name '*.wacc');
do 
    let TOTAL++
    #echo $f;
    # Compile file recording output
    SUCC=$(./compile $f)
    if [ $? -gt 0 ]; then 
        # Print any compilation errors
        let ERRORS++
        echo "[ERROR] Compilation Failed for ${f}"
        echo "${SUCC}"
        echo "Exit code: ${?}"
        continue
    fi

    FNAME=$(sed 's/.wacc/.s/' <<< "${f##*/}")
    arm-linux-gnueabi-gcc -o testEXE -mcpu=arm1176jzf-s -mtune=arm1176jzf-s $FNAME
    rm -rf $FNAME
    # Get output produced by compiled code
    OUT=$(qemu-arm -L /usr/arm-linux-gnueabi/ testEXE)
    
    # Retrieve the expected output from the example file
    EXP_OUT=$(sed -n -e '/# Output:/,$p' $f | sed '1d' | sed -e '/# Program:/,$d' | sed 's/# //' | sed 's/#//')
    
    # Check for differences
    if [ "${OUT}" != "${EXP_OUT}" ]; then
        # Print outputs if different
        let ERRORS++
        echo "[ERROR] Difference in expected output in ${f}"
        echo "- Output:"
        echo "${OUT}"
        echo "- Expected:"
        echo "${EXP_OUT}"
    fi
done

echo "${ERRORS}/${TOTAL} tests failed."

# Exit with success/failure
if [ $ERRORS -gt 0 ]; then
    exit 1
fi
