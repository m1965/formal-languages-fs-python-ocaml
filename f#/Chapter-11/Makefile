DOTNET ?= dotnet
JAVA ?= java
JASMIN_JAR ?=

.PHONY: run hello jvm-sum jvm-hello clean

run:
	"$(DOTNET)" fsi --exec Sum.fsx

hello:
	"$(DOTNET)" fsi --exec Hello.fsx

jvm-sum: Sum.class
	"$(JAVA)" -Xverify:all -cp . Sum

jvm-hello: Hello.class
	"$(JAVA)" -Xverify:all -cp . Hello

%.class: %.jas
	@test -n "$(JASMIN_JAR)" || { echo "Set JASMIN_JAR to the Jasmin jar-with-dependencies file" >&2; exit 1; }
	"$(JAVA)" -jar "$(JASMIN_JAR)" "$<"

clean:
	rm -f Hello.class Sum.class
