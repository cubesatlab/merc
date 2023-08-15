Test Instructions:

Run Merc on all of the mxdr files in this directory, generating api files in this same directory. This directory contains a gpr project which includes the generated API files and some integration tests.

1) The entire project should build.
2) The API files should pass SPARK inspection.
3) The test executables in the projects complete successfully.

This project also serves as a compatibility check. If a change in Merc breaks compatibility with it, think before changing the tests.

Note: Currently, 8/15/23, the integration tests for Time and TimeSpan (m0006) are failing. To fix this, we need to decide on a standard for what Time values are acceptable. The Ada standard is pretty lax about what compilers need to implement, and I don't have time to figure it all out right now. It might be worth creating our own time types in the CubedOS library.


Tests:

m0003:
	Contains a definition of every type.
	
m0004-integration:
	Tests all the numeric types including ranges, not subtypes.
	Fixed array types, structs, fixed arrays of anonymous structs.
	Fixed arrays of anonymous non-valued enums.
	
m0005-integration:
	Test strings and variable length opaque data.

m0006-integration:
	Tests Time and Time_Span encoding/decoding.