
Merc
====

A tool for compiling CubedOS message definitions into message encoding/decoding subprograms.

Merc is the CubedOS interface definition language (IDL) compiler. This tool converts a
description of CubedOS messages into provable SPARK packages containing message encoding and
decoding subprograms. The IDL is an extended version of the eXternal Data Representation (XDR)
standard described in RFC-4506. We call our extended XDR "modified" XDR, or MXDR. It is documented
in the Merc_User_Manual.md file.

Build Instructions
------------------

Merc is an [SBT](https://www.scala-sbt.org/) based project. After installing a suitable
JVM (we recommend version 17.x.y for some x, y) and SBT, run an SBT interactive session while
inside the top level Merc folder. There you can use the usual SBT commands such as `compile`,
`package`, `test`, and `assembly` to build and exercise the code. Any IDE that supports SBT
integration, such as IntelliJ can also be used.

Testing Instructions
--------------------
The sbt 'test' command can be used for the automated tests. There are also manual integration tests which require an Ada compiler and the SPARK/Ada formal verification toolset. Both these can be found on AdaCore's website for free. The integration tests are in testData/CodeGeneration, there is another README in there with more instructions.
