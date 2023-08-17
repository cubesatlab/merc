TODO: After writing this, I realize it is a little redundant with the other documentation here which is written in tex. This document is newer than the tex is (8/17/23). Figure out what can be removed.

Merc is a tool that generates CubedOS module APIs implementations from specification files. The specification files are written in a modified version of XDR (MXDR) described in this document. Merc produces Ada header and body files which are used by CubedOS modules to communicate.


***Commandline Usage***

-o path

The directory to put generated API files.

-t path

the directory to find the template files used during code generation. This is used mostly for debugging Merc.

TODO: expected project structure.

***MXDR Specification***

MXDR is derived from the XDR standard with specialized data types and semantics for message passing and high integrity applications. An MXDR file defines the interface of one CubedOS module. It specifies what messages the module is prepared to receive and what messages other modules can expect to receive from it.

This section will detail MXDR's differences from XDR.

**File Naming Convention**

An MXDR file name ends in ".mxdr". The name of the file is the name of the described module. For example, a ping server module with the exact module name `Ping_Server` would have an MXDR file name "Ping_Server.mxdr".

Modules with package prefixes like `CubedOS.Time_Server` include their prefix in the file name substituting dots for dashes: "CubedOS-Time_Server.mxdr". Capitalization should be preserved.

**Message Passing Semantics**

MXDR allows the specification of `message` types which are specialized structs. 

```
// Message description
message struct <- Message_Name {
	Type Component_Name;
	Type2 Component_Name2;
	        ...
};
```

`Message` types specify the direction that the message will be sent: `->` means the message type is received by the module, `<-` means the message is sent by the module. Notice that any one message type may not be received and sent by the same module. This information will be used by the implementing module.

Modules may only receive message types which are explicitly declared as acceptable. For messages described in the MXDR file, this is acheived with the direction arrow. Messages described by other modules may be declared acceptable by importing them.

`RECEIVES LBRACE IDENTIFIER (COMMA IDENTIFIER)* RBRACE FROM package_name;`

For example, a module that interacts with the CubedOS.Time_Server and CubedOS.File_Server modules would import the message types it needs to receive.

```
receives {Tick_Reply} from CubedOS.Time_Server;
receives {Open_Reply, Write_Reply} from CubedOS.File_Server;
```

All import statements must occur at the beginning of the file before any type or message definitions.

**Additional Types**

MXDR adds some specialized data types relevant to CubedOS applications. Note that these are currently poorly defined and require revision.

	`Time`
	
	Store a non-negative time value with nanosecond accuracy. Values occupy 64 bits. The value will be converted to the target language's native Time type. For Ada, `Ada.Real_Time.Time`.
	
	`TimeSpan`
	
	Stores a non-negative time value with nanosecond accuracy. Values occupy 64 bits. The value will be converted to the target language's native TimeSpan type. For Ada, `Ada.Real_Time.Time`.
	
	Stores a non-negative time value with millisecond accuracy.
	
**Additional Type Rules**

MXDR supports a data typing system similar to Ada's and stronger than is specifed by XDR. This provides greater safety for module implementations.

Ada is unlike most programming languages in its type system. Types in Ada and MXDR may carry several constraints on their value, and strong typing with explicit conversion is used as a method of organization.

For example in Ada the units "meters" and "cenimeters" may take on different types, despite both being fundamentally integers. The type system prevents meters and cenimeters from being added without casting one to the other or overriding the addition operator. This added expression reduces the chance of error at the expense of verbosity, a tradeoff which is often justified in high-integrity applications.

Further, type invariants are more easily and frequently applied. The dimensions of an object may be stored in a numeric type which must be non-negative. Any attempt to assign a negative value to the type would be detected as an error.

*Type Invariants*

MXDR supports range constraints on numeric types and more complex constraints on message types.

When defining a numeric type, a range constraint may be added. For example, a integer type representing the military time in hours could limit it's values to only valid times. The range operator `..` is inclusive.

`typedef unsigned int Military_Time_Hour range 0 .. 23;`

Modules which receive military times may then safetly assume the value is on [0, 23]. Numeric types may also have their range referenced after their creation through the `'Last` and `'First` suffixes.

`Military_Time_Hour'Last` evaluates to 23 and retains the type of the range.

For message types, more general type invariants are supported. A message definition may be followed by the `with` keyword and then `message_invariant =>` and then several comma-separated invariant conditions.

```
message Current_Time <- {
	Military_Time_Hour Hour;
	Military_Time_Minutes Minute;
} with message_invariant => Hour < 10, Minute > 20;
```

*Type Substitution*

The `typedef` keyword carries more weight than in XDR. Two types declared from the same base type by default are not substitutable.

```
typedef int Dollars_Type;
typedef int Meters_Type;
```

Dollars and meters logically are different quantities. Regardless of the fact that dollars and meters are both integer types, they may not be combined in arithmetic operations without an explicit type conversion.

Two types may be made substitutable using subtyping. This is appropriate for types which have special values. Consider a type `Pointer` which refers to a space in memory. Perhaps the value 0 is reserved for communicating null. Data may only be read from the set of non-null pointers (pointers which are not zero). This situation could be explicitly modeled with two types.

```
const Last_Pointer = 256;
typedef unsigned int Pointer_Type range 0 .. Last_Pointer;
typedef unsigned int Valid_Pointer_Type is Pointer_Type range 1 .. Last_Pointer;
```

Contexts which require a non-null pointer may take the `Valid_Pointer_Type`, and they will never be given a null pointer. On the other hand, contexts which may take a null pointer will take the `Pointer_Type`. `Valid_Pointer_Type` is a subtype of `Pointer_Type` and as such may be substituted.


***Generated API files***

Currently Merc only supports targeting SPARK/Ada.

TODO: Formally document how MXDR names should be translated to Ada names.
Current behavior:
- Anonymous types are assigned names depending on their context.
	- An array of anonymous structs gives the struct the name `arrayName_Struct`
	- An array of anonymous enums gives the enum the name `arrayName_Enum`
	
- We might want to change these names at some point.
- Variably sized data like variable opaque, variable arrays and strings are stored on the heap in certain circumstances. These values are always supplied ot encoder functions on thr stack, but they are decoded onto the heap and returned by reference. Free procedures are generated for these types with the name`Free`.
- User visible arrays start at index 1.

			
***Limitations of Merc***

Merc currently doesn't implement the following XDR features:

- Optional data
- Union types

For Ada compatibility, defined names must not overlap with Ada keywords:

    "abort",     "abs",          "abstract", "accept",  "access",    "aliased",
    "all",       "and",          "array",    "at",      "begin",     "body",
    "case",      "constant",     "declare",  "delay",   "delta",     "digits",
    "do",        "else",         "elsif",    "end",     "entry",     "exception",
    "exit",      "for",          "function", "generic", "goto",      "if",
    "in",        "interface",    "is",       "limited", "loop",      "mod",
    "new",       "not",          "null",     "of",      "or",        "others",
    "out",       "overriding",   "package",  "pragma",  "private",   "procedure",
    "protected", "raise",        "range",    "record",  "rem",       "renames",
    "requeue",   "return",       "reverse",  "select",  "separate",  "some",
    "subtype",   "synchronized", "tagged",   "task",    "terminate", "then",
    "type",      "until",        "use",      "when",    "while",     "with",
    "xor"

