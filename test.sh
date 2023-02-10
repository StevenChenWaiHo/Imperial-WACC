echo "Running valid tests"
for f in $(find ../wacc_examples/valid -name '*.wacc'); do ./compile $f; done
echo "Running invalid tests"
for f in $(find ../wacc_examples/invalid -name '*.wacc'); do ./compile $f | grep "Successful"; done
