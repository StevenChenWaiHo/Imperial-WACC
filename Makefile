all:
	sbt compile assembly

clean:
	sbt clean && rm -rf wacc-39-compiler.jar

.PHONY: all clean
