#!/bin/bash
SUCCESS=0
SYNTAX_ERR=100
SEMANTIC_ERR=200
ERRORS=0
TOTAL=0
# Check all invalid syntax programs
for f in $(find ../wacc_examples/invalid/syntaxErr -name '*.wacc');
do 
    let TOTAL++
    # Compile file recording output
    SUCC=$(./compile $f)
    if [ !($? -eq SYNTAX_ERR) ]; then 
        # No syntax error produced
        echo "[ERROR] Code ${?} produced instead of syntax error"
        echo "${SUCC}"
        let ERRORS++
    fi
done

# Check all invalid semantic programs
for f in $(find ../wacc_examples/invalid/semanticErr -name '*.wacc');
do 
    let TOTAL++
    # Compile file recording output
    SUCC=$(./compile $f)
    if [ !($? -eq SEMANTIC_ERR) ]; then 
        # No semantic error produced
        echo "[ERROR] Code ${?} produced instead of semantic error"
        echo "${SUCC}"
        let ERRORS++
    fi
done

echo "${ERRORS}/${TOTAL} tests failed."

# Exit with success/failure
if [ $ERRORS -gt 0 ]; then
    exit 1
fi

