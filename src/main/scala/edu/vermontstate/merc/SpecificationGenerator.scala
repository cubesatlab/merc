package edu.vermontstate.merc

import edu.vermontstate.merc.TypeRep.{ArrayRep, ConstRep, ContinuousRep, EnumRep, EnumValue, FixedArrayRep, FixedOpaqueRep, MStructRep, MXDREntity, Ranged, Rep, StringRep, StructComponent, StructRep, VariableArrayRep, VariableOpaqueRep}
import AdaGeneratorCommon.{adaFriendlyTypeName, assertMXDRTreeAcceptableForAda, passedByReference}
import MXDRReadingHelpers.resolveValue

import java.io.File

class SpecificationGenerator(
      templateFolder : String,

      /**
       * Everything that comes before the module name,
       * properly capitalized. In the case of the module
       * CubedOS.Time_Server,
       * the prefix is "CubedOS", not including the dot.
       * May be empty.
       */
      modulePrefix : String,
      /**
       * The properly capitalized module name, no dots.
       * Doesn't include "-.API".
       */
      moduleName     : String,
      /**
       * The file name of the specification file being written to.
       * Ex) cubedos-time_server-api.ads
       */
      fileName       : String,
      symbolTable    : BasicSymbolTable,
      mxdrTree       : MXDRTree,
      dependencyReader: DependencyReader,
      out            : java.io.PrintStream,
      reporter       : Reporter) {

  // The number of indentations where output lines start.
  private var indentationLevel = 0
  // All the message types this module may receive
  // Doesn't include "_Msg", only the name of the message type.
  private val receiveTypes = dependencyReader.receiveMessages
  // Contains the package name of every module this one depends on.
  // For a Ping_Client module which receives Ping_Reply messages
  // defined by a Ping_Server module, this list will contain
  // "Ping_Server".
  // If the Ping_Client receives messages defined by the
  // CubedOS.Time_Server module, the set includes
  // "CubedOS.Time_Server". The strings should be legal
  // ada imports.
  private val packageDependencies = dependencyReader.dependsOn


  private def doIndentation(): Unit = {
    for (_ <- 0 until indentationLevel) {
      out.print("   ")
    }
  }

  /**
   * Inserts the given string into a copy of the given list of strings, immediately
   * after the the given indicator.
   * @param lineList A list of all the lines in the file.
   * @param str The string to insert.
   * @param indicator The search string to insert after.
   * @return A new list of strings.
   */
  private def insertLine(lineList: List[String], str: String, indicator: String): List[String] = {
    var result = List[String]()
    for (line <- lineList) {
      result = line :: result
      if (line.contains(indicator)) {
        result = ("\n" + str) :: result
      }
    }
    result.reverse
  }

  /**
   * Creates a new list of strings with some added lines
   * which may be conditionally necessary.
   * @param input The initial list of strings.
   * @return A new list of strings with the necessary additions.
   */
  private def addNecessaryImports(input: List[String]): List[String] = {
    var flagTimer = 0
    for (i <- symbolTable.getMStructs) {
      for (ii <- symbolTable.getStructComponentNames(i)) {
        val s = symbolTable.getStructComponentTypeName(i, ii)
        if (s.contains("TimeSpanRep") ||
          s.contains("TimeRep")) {
          flagTimer = 1
        }
      }
    }
    var result = List[String]()
    if (flagTimer == 1) {
      val s1 = "with Ada.Real_Time;"
      val a1 = "SPARK_Mode(On);"
      val finalTemplateLines = insertLine(input, s1, a1)
      result = finalTemplateLines
    }
    else {
      result = input
    }
    result
  }

  /**
   * Loads the template.ads file into a list of lines
   * and then does some modifications to it before
   * returning it.
   */
  def processTemplate(): List[String] = {
    val source = scala.io.Source.fromFile(templateFolder + File.separator + "template.ads")
    var result = source.getLines().toList
    source.close()
    result = addNecessaryImports(result)
    result
  }

  def generate(): Unit = {
    assertMXDRTreeAcceptableForAda(mxdrTree)
    val lines = processTemplate()
    for (line <- lines) {
      val prefixString = if (modulePrefix.isEmpty) "" else modulePrefix + "."
      val newLine = line.replace("%MODULENAME%", prefixString + moduleName)
                        .replace("%FILENAME%", fileName)
      if (line.contains("%BULK%")) {
        generateBulk()
      }
      else if (line.contains("%SPECIMPORTS%")) {
        for (dependency <- packageDependencies) {
          println(s"with $dependency.API; use $dependency.API;")
        }
      }
      else {
        out.println(newLine)
      }
    }
  }

  private def generateBulk(): Unit = {
    indentationLevel += 1
    println("pragma Elaborate_Body;")
    println("type Octet_Array_Ptr is access CubedOS.Lib.Octet_Array;")
    println("type String_Ptr is access String;")
    println()
    println(s"This_Module : constant Module_ID_Type := Name_Resolver.$moduleName;")
    println()

    // Message type enum
    // This file may be only imports and not define any types
    if (symbolTable.getMStructs.nonEmpty)
      printMessageTypeEnum()

    // Write universal message type declarations
    for (name <- symbolTable.getMStructs) {
      println(s"${name}_Msg : constant Universal_Message_Type := (This_Module, Message_Type'Pos($name));")
    }
    println()

    printModuleMetadata()
    println()

    for (entityType <- mxdrTree.getItems) {
      val symbol = entityType match {
        case x: Rep =>
          x.typeName.get
        case _ =>
          "" // This value should never be used
      }
      entityType match {
        // Scala doesn't check the T of ConstRep[T], but we don't care
        case constant: ConstRep[Rep @unchecked] => printConstantDef(constant)
        case rep: EnumRep => printEnumDef(symbol, rep)
        case r: MStructRep => printMessageSpecs(r)
        case r: StructRep => printStructDef(symbol, r)
        case rep: FixedOpaqueRep => doFixedOpaqueDef(symbol, rep)
        case rep: VariableOpaqueRep =>
          println(s"type $symbol is new ${adaFriendlyTypeName(rep)}")
          println(s"   with Dynamic_Predicate => $symbol'Length <= ${rep.maxBytes.value};")
          println(s"type ${symbol}_Ptr is access $symbol;")
          println(s"procedure Free is new Ada.Unchecked_Deallocation($symbol, ${symbol}_Ptr);")
          println()
        case arr: ArrayRep => doArrayDef(symbol, arr)
        case rep: StringRep =>
          println(s"subtype $symbol is ${rep.baseTypeName.getOrElse(adaFriendlyTypeName(rep))}")
          println(s"   with Dynamic_Predicate => $symbol'Length <= ${rep.maxLength};")
          println(s"type ${symbol}_Ptr is access $symbol;")
          println(s"procedure Free is new Ada.Unchecked_Deallocation($symbol, ${symbol}_Ptr);")
          println()
        case rep: Rep => doBasicTypeDef(symbol, rep)
        case _ =>
          throw new Error("Unimplemented")
      }
    }
    indentationLevel -= 1
  }

  private def doArrayDef(symbol: String, typeRep: ArrayRep): Unit = {
    typeRep match {
      case arr: FixedArrayRep =>
        println(s"type $symbol is array (1..${arr.size.value}) of ${arr.elementType.typeName.getOrElse(adaFriendlyTypeName(arr.elementType))};")
      case arr: VariableArrayRep =>
        println(s"type $symbol is array (Natural range <>) of ${arr.elementType.typeName.getOrElse(adaFriendlyTypeName(arr.elementType))};")
        println(s"type ${symbol}_Ptr is access $symbol;")
    }
  }

  private def doFixedOpaqueDef(name: String, typeRep: FixedOpaqueRep): Unit = {
    println(s"type $name is new ${adaFriendlyTypeName(typeRep)}(1..${typeRep.bytes.value});")
    println()
  }

  /**
   * Prints the definition for the Message_Type enum.
   */
  private def printMessageTypeEnum(): Unit = {
    doIndentation()
    out.println("type Message_Type is")
    indentationLevel += 1
    doIndentation()
    out.print("(")
    val n = symbolTable.getMStructs
    for (name <- n) {
      if (name == n.last) {
        if (n.size == 1) {
          out.println(name + ");")
        }
        else {
          println(name + ");")
        }
      }
      else {
        if (name == n.head) {
          out.println(name + ", ")
        }
        else {
          println(name + ", ")
        }
      }
    }
    out.println("")
    indentationLevel -= 1
  }

  /**
   * Prints the information necessary for a module to implement
   * the API.
   * - the This_Receives list
   * - the Module_Metadata constant.
   */
  private def printModuleMetadata(): Unit = {
    doIndentation()
    out.print("This_Receives : aliased constant Message_Type_Array := ")
    if (receiveTypes.size == 1) {
      out.println("(0 => " + receiveTypes.last + "_Msg);")
    } else if (receiveTypes.nonEmpty) {
      out.println("(")
      for (typeName <- receiveTypes) {
        doIndentation()
        out.print("" + typeName + "_Msg")
        if (typeName == receiveTypes.last)
          out.println(");")
        else out.println(",")
      }
    } else {
      out.println("Empty_Type_Array;")
    }

    println("Mail_Target : aliased constant Module_Metadata := Define_Module(This_Module, This_Receives'Access);")
  }

  /**
   * Prints a decoder function for the given message type.
   * @param message The message to decode.
   * @param direction If the message is sent of received by the module.
   */
  def printDecode(message: MStructRep, direction: MessageDirection.Value): Unit ={
    val msgName = message.typeName.get
    val decodeString = "_Decode"
    println("procedure " + msgName + decodeString)
    indentationLevel += 1

    // Print parameters for the message content
    val rep = new AdaGeneratorMessageHelper(message)
    rep.printDecoderParams(indentationLevel, out)

    indentationLevel -= 1
    println("with")
    indentationLevel += 1
    println("Global => null,")
    println("Pre => Is_" + msgName + "(Message) and Payload(Message) /= null;")

    // All outputs depend on the message
//    doIndentation()
//    out.print("Depends => ((")
//    val components = symbolTable.getStructComponentNames(msgName).toList
//    val commaSeparatedComponentList = components.reduce((existingList, componentName) => {
//      val t = symbolTable.getStructComponentTypeName(msgName, componentName)
//
//      var newList = existingList + ", " + componentName
//      if (t == "string"
//        || t == "CubedOS.Lib.Octet_Array"
//        || t == "opaque") {
//        newList += s", ${componentName}_Size"
//      }
//      newList
//    })
//    out.println(s"${commaSeparatedComponentList}, Decode_Status) => Message);")
    out.println("")

    // Print the message invariants
    for (i <- message.invariants.indices) {
      if (i == message.invariants.size - 1) {
        println("Post => " + message.invariants(i) + ";")
      }
      else {
        println("Post => " + message.invariants(i) + ",")
      }
    }
    indentationLevel -= 1
    out.println("")

  }

  /**
   * Write a message encoder subprogram.
   * @param msgRep The message being encoded.
   * @param messageInvariants Message component identifiers.
   */
  def printEncode(msgRep: MStructRep, messageInvariants: List[String], direction: MessageDirection.Value): Unit = {
    val msgName = msgRep.typeName.get

    val encodeString = "_Encode"
    println("procedure " + msgName + encodeString)
    indentationLevel += 1

    val struct = new AdaGeneratorMessageHelper(msgRep)
    // Parameters
    val (stringFlag, _) = struct.printEncoderParams(indentationLevel, out)

    // Aspects
    indentationLevel -= 1
    println("with")
    indentationLevel += 1

    var preConditions = List[String]()
    if (stringFlag != "")
      preConditions = preConditions :+ "(0 < " + stringFlag + "'Length and " + stringFlag + "'Length <= XDR_Size_Type'Last - 12)"
    for (i <- messageInvariants.indices)
        preConditions = preConditions :+ messageInvariants(i)
    // Message is being sent to the right place
    if (direction == MessageDirection.In) { // The receiving module id is known at compile time
      preConditions = preConditions :+ s"Receiver_Address.Module_ID = This_Module"
    } else if (direction == MessageDirection.Out) { // Sending module id is known at compile time
      preConditions = preConditions :+ s"Sender_Address.Module_ID = This_Module"
    }

    if (preConditions.nonEmpty) {
      println("Pre => true")
      indentationLevel += 1
      for (i <- preConditions.indices) {
        if (i == preConditions.size - 1) {
          println("and then " + preConditions(i) + ",")
        } else
          println("and then " + preConditions(i))
      }
      indentationLevel -= 1
    }
    //Post conditions
    println(s"Post => CubedOS.Message_Types.Message_Type(Result) = ${msgName}_Msg")
    indentationLevel += 1
    println("and CubedOS.Message_Types.Sender_Address(Result) = Sender_Address")
    println("and CubedOS.Message_Types.Receiver_Address(Result) = Receiver_Address")

    println ("and Payload(Result) /= null;")
    indentationLevel -= 1

    indentationLevel -= 1
    out.println("")
  }

  /**
   * Prints specification for a type unsafe sender subprogram for the
   * given message struct.
   * @param msgRep The message to send.
   * @param messageInvariants Preconditions specified by the user to include in the spec.
   * @param direction Whether the module is sending or receiving this message.
   * @param withStatus Whether to include a result status parameter in the spec.
   */
  def printUnsafeSend(msgRep: MStructRep, messageInvariants: List[String], direction: MessageDirection.Value, withStatus: Boolean): Unit = {
    val msgName = msgRep.typeName.get
    println("procedure Send_" + msgName)
    indentationLevel += 1

    val struct = new AdaGeneratorMessageHelper(msgRep)

    // Parameters
    val (stringFlag, _) = struct.printUnsafeSenderParams(indentationLevel, out, withStatus)

    // Aspects
    indentationLevel -= 1
    println("with")
    indentationLevel += 1

    println("Global => (In_Out => Mailboxes),")

    // There will be at least 1 precondition
    var preConditions = List[String]()
    if (stringFlag != "")
      preConditions = preConditions :+ "(0 < " + stringFlag + "'Length and " + stringFlag + "'Length <= XDR_Size_Type'Last - 12)"
    for (i <- messageInvariants.indices)
      preConditions = preConditions :+ messageInvariants(i)
    // Message is being sent to the right place
    if (direction == MessageDirection.In) { // The receiving module id is known at compile time
      preConditions = preConditions :+ s"Receiver_Address.Module_ID = This_Module"
    } else if (direction == MessageDirection.Out) { // Sending module id is known at compile time
      preConditions = preConditions :+ s"Module_ID(Sender) = This_Module"
    }
    if (withStatus) {
      preConditions = preConditions :+ s"Receiver_Address.Domain_ID = Domain_ID"
    }


    println("Pre => Messaging_Ready")
    indentationLevel += 1
    for (i <- preConditions.indices) {
      println("and then " + preConditions(i))
    }
    indentationLevel -= 1

    println(";")

    indentationLevel -= 1
    out.println("")
  }

  /**
   * Prints a message send subprogram specification for the given message.
   * @param msgRep The message being sent.
   * @param messageInvariants Preconditions to put in the spec.
   * @param direction Whether the message is sent or received by the module.
   * @param withStatus If the produced subprogram should have a result status parameter.
   */
  def printSafeSend(msgRep: MStructRep, messageInvariants: List[String], direction: MessageDirection.Value, withStatus: Boolean): Unit = {
    val msgName = msgRep.typeName.get
    println("procedure Send_" + msgName)
    indentationLevel += 1
    val _ = symbolTable.getType[MStructRep](msgName)
    val struct = new AdaGeneratorMessageHelper(msgRep)

    // Parameters
    val (stringFlag, _) = struct.printSafeSenderParams(indentationLevel, out, withStatus)

    // Aspects
    indentationLevel -= 1
    println("with")
    indentationLevel += 1

    println("Global => (In_Out => Mailboxes),")

    // There will be at least 1 precondition
    var preConditions = List[String]()
    if (stringFlag != "")
      preConditions = preConditions :+ "(0 < " + stringFlag + "'Length and " + stringFlag + "'Length <= XDR_Size_Type'Last - 12)"
    for (i <- messageInvariants.indices)
      preConditions = preConditions :+ messageInvariants(i)
    // Message is being sent to the right place
    if (direction == MessageDirection.In) { // The receiving module id is known at compile time
      preConditions = preConditions :+ s"Receiving_Module.Module_ID = This_Module"
    } else if (direction == MessageDirection.Out) { // Sending module id is known at compile time
      preConditions = preConditions :+ s"Module_ID(Sender) = This_Module"
    }

    // Destination safety checks
    preConditions = preConditions :+ s"Receives(Receiving_Module, ${msgName}_Msg)"
    preConditions = preConditions :+ (if (withStatus) "Has_Module(This_Domain, Receiving_Module.Module_ID)"
      else "Has_Module(Receiving_Domain, Receiving_Module.Module_ID)")

    println("Pre => Messaging_Ready")
    indentationLevel += 1
    for (i <- preConditions.indices) {
      println("and then " + preConditions(i))
    }
    indentationLevel -= 1

    println(";")

    indentationLevel -= 1
    out.println("")
  }

  /**
   * Print the check message type function.
   * @param message The message to check.
   */
  private def doCheck(message: MStructRep): Unit ={
    val checkString = "Is_"
    println("function " + checkString + message.typeName.get + "(Message : Message_Record) return Boolean is")
    indentationLevel += 1
    println(s"(CubedOS.Message_Types.Message_Type(Message) = ${message.typeName.get}_Msg);")
    indentationLevel -= 1
  }

  /**
   * Prints the components of a struct.
   * type Struct_Type is
   *  record
   *    ... <- This stuff
   *  end record;
   * @param components The struct's components.
   */
  private def printStructComponents(components: List[StructComponent]): Unit ={
    indentationLevel += 1
    for(component <- components) {
      val componentName = component.name
      val typeRep = component.typeRep

      assert(!(component.typeRep.isInstanceOf[ArrayRep] && component.typeRep.isAnonymous),
        "Anonymous arrays should have been removed in pre-processing.")

      if (passedByReference(typeRep))
        println(s"$componentName : ${typeRep.typeName.getOrElse(adaFriendlyTypeName(typeRep))}_Ptr;")
      else {
        typeRep match {
          case o: FixedOpaqueRep =>
            println(s"$componentName : ${typeRep.typeName.getOrElse(adaFriendlyTypeName(typeRep))}(1..${o.bytes.value});")
          case _ =>
            println(s"$componentName : ${typeRep.typeName.getOrElse(adaFriendlyTypeName(typeRep))};")
        }
      }
    }

    indentationLevel -= 1
  }

  /**
   * $constName : constant $constType := $constValue;
   * @param constRep Must be named.
   */
  private def printConstantDef(constRep: ConstRep[Rep]): Unit ={
    doIndentation()
    out.print(s"${constRep.name.get} : constant ")

    if (constRep.typeName.nonEmpty) {
      out.print(constRep.typeName.get)
    } else if (constRep.typeRep.nonEmpty) {
      out.print(constRep.typeRep.get.baseTypeName.getOrElse(adaFriendlyTypeName(constRep.typeRep.get)))
    } else {
      val constTypeName = inferTypeOfConst(constRep)
      out.print(constTypeName)
    }

    out.println(s" := ${constRep.value};")
    println()
  }

  /**
   * Determines the type of the constant.
   * Will recursively search through const references
   * or determine the type of a literal.
   * @param value The constant.
   * @return
   */
  private def inferTypeOfConst(value: ConstRep[Rep]): String = {
    if (value.typeRep.nonEmpty) return value.typeRep.get.baseTypeName.getOrElse(adaFriendlyTypeName(value.typeRep.get))

    if (value.isSymbolic) {
      return inferTypeOfConst(symbolTable.getConstant[Rep](value.value))
    }

    // Assume that all constants are numeric
    val numericVal = resolveValue(value.value, symbolTable)
    if (numericVal.toString.contains('.'))
      "Float"
    else "Integer"
  }

  /**
   * Prints definitions for the basic types.
   */
  private def doBasicTypeDef(name: String, typeRep: Rep): Unit = {
    val isRanged = typeRep.isInstanceOf[Ranged] && typeRep.asInstanceOf[Ranged].isExplicit
    val isSubtype = typeRep.isSubtype

    doIndentation()
    if (isSubtype) {
      out.print(s"subtype $name is ${typeRep.baseTypeName.get}")
    } else {
      out.print(s"type $name is")
      if (!isRanged) out.print(s" new ${adaFriendlyTypeName(typeRep)}")
      else if (isRanged && typeRep.isInstanceOf[ContinuousRep]) out.print(" new Float")
    }

    if (isRanged) {
      val rangedRep = typeRep.asInstanceOf[TypeRep.Ranged]
      out.print(s" range (${rangedRep.range.lowerBound.value}) .. (${rangedRep.range.upperBound.value})")
    }

    out.println(";")
    println()
  }

  private def printEnumDef(name: String, typeRep: EnumRep): Unit ={
    doIndentation()
    out.print("type " + name + " is (")

    // A valid enum must have one or more values
    printEnumValues(typeRep.values)
    out.print(")")
//    indentationLevel += 1
//    visitEnum_body(ctx.enum_body)
//    indentationLevel -= 1
    out.println(";")
    out.println("")
  }

  private def printEnumValues(components: List[EnumValue]): Unit = {
    if (components.isEmpty) return

    val values = components.map(x => (x.name, x.value))

    // Assume either all components have values, or none do

    out.print(values.head._1)
    if (values.head._2.nonEmpty)
      out.print(" = " + values.head._2.get)

    for (i <- 1 until values.length) {
      out.print(s", ${values(i)._1}")
      if (values(i)._2.nonEmpty)
        out.print(s" = ${values(i)._2.get}")
    }
  }

  /**
   * type $name is
   *    record
   *     ...
   *    end record;
   */
  private def printStructDef(name: String, structRep: StructRep): Unit ={
    println("type " + name + " is ")
    indentationLevel += 1
    println("record")
    printStructComponents(structRep.components)
    println("end record;")
    indentationLevel -= 1
    out.println("")
  }

  /**
   * Prints the specifications for the given message struct type.
   * Includes the encode, decoder, send and check subprograms.
   */
  private def printMessageSpecs(message: MStructRep): Unit = {
    val direction = message.direction
    val messageInvariants = message.invariants

    // TODO: Move this kind of thing to the semantic analyzer
//    if (message.isVoid && message.components.components.size > 1) {
//      System.out.println("Can't have multiple message struct")
//      System.out.println("parameters included with void.")
//      return
//    }

    printEncode(message, messageInvariants, direction)
    printUnsafeSend(message, messageInvariants, direction, withStatus = false)
    printUnsafeSend(message, messageInvariants, direction, withStatus = true)
    printSafeSend(message, messageInvariants, direction, withStatus = false)
    printSafeSend(message, messageInvariants, direction, withStatus = true)
    doCheck(message)
    if (!message.isVoid)
      printDecode(message, direction)
  }

  /**
   * Prints the given content with the correct indentation and
   * a newline at the end.
   * @param content The string to print, doesn't include the newline.
   */
  private def println(content: String = ""): Unit = {
    doIndentation()
    out.println(content)
  }
}
