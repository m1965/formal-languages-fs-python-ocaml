# JVM and Jasmin worked programs

The original Chapter 11 has no notebook, so notebook inventory alone misses these lecture examples. `Sum.fsx` translates the Java/C summation program to F#, and `Hello.fsx` provides the greeting example. The original hand-written `Hello.jas` and `Sum.jas` remain in the JVM target language; `Sum.java` and `sum.c` are preserved for source comparison. The platform-specific compiled `sum` binary is intentionally excluded.

From this directory, run `dotnet fsi --exec Sum.fsx` or `make run`. The script uses a tail-recursive loop and always-active checks for the exact result **666**, including the empty and one-element sums. `make hello` prints **Hello World!**.

With a JDK and the [Jasmin 3.0.3 jar-with-dependencies artifact](https://central.sonatype.com/artifact/ca.mcgill.sable/jasmin/3.0.3), run:

```sh
make jvm-sum jvm-hello JAVA=/path/to/jdk/bin/java JASMIN_JAR=/path/to/jasmin-3.0.3-jar-with-dependencies.jar
```

The Makefile replaces the original machine-specific `/usr/local/lib/jasmin.jar` dependency with explicit parameters and enables JVM bytecode verification. Both target programs were assembled and executed with OpenJDK 21 and Jasmin 3.0.3, producing **666** and **Hello World!**. Generated `.class` files are ignored. Chapter 12 expands these hand-written JVM examples into a complete F# compiler that generates Jasmin.
