package edu.vtc.merc

import edu.vtc.merc.AdaGeneratorCommon.{adaFriendlyTypeName, xdrTypeName}
import edu.vtc.merc.TypeRep._

import java.io.File

class BodyGenerator(
     templateFolder : String,

     /**
      * The formal prefix to the module name,
      * not including the final dot.
      * In CubedOS.Time_Server the prefix is
      * "CubedOS" without the "."
      */
     modulePrefix : String,
     /**
      * The properly capitalized, module name.
      * Doesn't include "-api" or any dots.
      */
     moduleName     : String,
     /**
      * The file name of the specification file being written to.
      * Ex) cubedos-time_server-api.adb
      */
     fileName       : String,
     symbolTable    : BasicSymbolTable,
     mxdrTree       : MXDRTree,
     out            : java.io.PrintStream,
     reporter       : Reporter) {

  // The number of indentations where output lines start.
  private var indentationLevel = 0

  private def doIndentation(): Unit = {
    for (_ <- 0 until indentationLevel)
      out.print("   ")
  }

  private def insertLine(bulk: List[String], str: String, indicator: String): List[String] = {
    var newBulk = List[String]()
    for (line <- bulk) {
      newBulk = line :: newBulk
      if (line.contains(indicator)) {
        newBulk = ("\n" + str) :: newBulk
      }
    }
    newBulk.reverse
  }

  private def addedLines(lines: List[String]): List[String] = {
    var flagTimer = 0
    for (i <- symbolTable.getMStructs) {
      for (ii <- symbolTable.getStructComponentNames(i)) {
        if (symbolTable.getStructComponentTypeName(i, ii).contains("TimeSpanRep") ||
          symbolTable.getStructComponentTypeName(i, ii).contains("TimeRep")) {
          flagTimer = 1
        }
      }
    }
    var l = List[String]()
    if (flagTimer == 1) {
      val s1 = "with Ada.Real_Time;\nuse Ada.Real_Time;"
      val a1 = "SPARK_Mode(On);"
      val finalTemplateLines = insertLine(lines, s1, a1)
      l = finalTemplateLines
    }
    else {
      l = lines
    }
    l
  }

  private def processTemplate(): List[String] = {
    val source = scala.io.Source.fromFile(templateFolder + File.separator + "template.adb")
    val lines = source.getLines().toList
    val newLines = addedLines(lines)
    source.close()
    newLines
  }

  /**
   * Produces the Ada body file.
   */
  def generate(): Unit = {
    val lines = processTemplate()
    for (line <- lines) {
      val prefixString = if (modulePrefix.nonEmpty) modulePrefix + "." else ""
      val newLine = line.replace("%MODULENAME%", prefixString + moduleName)
                        .replace("%FILEBASENAME%", fileName)
                        .replace("%FILENAME%", fileName)
      if (line.contains("%BULK%")) {
        indentationLevel += 1
        generateBulk()
        indentationLevel -= 1
      }
      else {
        out.println(newLine)
      }
    }
  }

  private def generateBulk(): Unit = {
    println("procedure Free is new Ada.Unchecked_Deallocation(String, String_Ptr);")
    println("procedure Free is new Ada.Unchecked_Deallocation(Octet_Array, Octet_Array_Ptr);")
    mxdrTree.getItems[MXDREntity]().foreach {
      case x: MStructRep =>
        handleMessage(x)
      case _ =>
    }
  }

  /**
   * Print body of encoder subprogram.
   * @param message Type info about the message. Doesn't
   *               contain anonymous array components.
   */
  private def printEncode(message: MStructRep): Unit = {
    assert(!message.components.exists(component => component.typeRep.isInstanceOf[ArrayRep] && component.typeRep.isAnonymous))
    val msgName = message.typeName.get

    // Signature
    val encodeString = "_Encode"
    println("procedure " + msgName + encodeString)
    indentationLevel += 1

    // Parameters
    val struct = new AdaGeneratorMessageHelper(message)
    struct.printEncoderParams(indentationLevel, out)
    indentationLevel -= 1
    println("is")

    // Declarations
    indentationLevel += 1
    println("subtype Data_Index_Type is XDR_Index_Type range 0 .. 1023;")
    println("Position   : Data_Index_Type;")
    println("Last       : Data_Index_Type;")
    println("subtype Definite_Data_Array is Data_Array(Data_Index_Type);")
    println("Payload : Data_Array_Owner := new Definite_Data_Array'(others => 0);")
    println("Message : Mutable_Message_Record;")

    indentationLevel -= 1

    // Implementation
    println("begin")
    indentationLevel += 1

    println("Position := 0;")

    // Encode every component
    for (component <- message.components) {
      printEncodeComponentIntoPayload(component.name, component.typeRep)
    }

    // Assemble the message
    println("Make_Empty_Message (")
    indentationLevel += 1
    println("Sender_Address   => Sender_Address,")
    println("Receiver_Address => Receiver_Address,")
    println("Request_ID   => Request_ID,")
    println(s"Message_Type => ${msgName}_Msg,")
    println("Payload => Payload,")
    println("Result => Message,")
    println("Priority   => Priority);")
    indentationLevel -= 1

    // Return it
    println("Result := Immutable(Message);")
    // Free the memory
    println("Delete(Message);")

    // Ignore final values
    println("pragma Unused(Last, Payload, Position, Message);")

    indentationLevel -= 1
    println("end " + msgName + encodeString + ";")
    println()
  }

  /**
   * Encodes the given value into the payload of the message
   * using the CubedOS.Lib.XDR functions.
   * @param valueName The name referring the value to encode in Ada.
   * @param typeRep The type of the data being encoded. If an array,
   *                must have a typename.
   * @param depth The call depth for recursion.
   */
  private def printEncodeComponentIntoPayload(valueName: String, typeRep: Rep, depth: Int = 0): Unit = {
    if (typeRep == VoidRep) return
    assert(!(typeRep.isInstanceOf[ArrayRep] && typeRep.isAnonymous),
      "SPARK/Ada doesn't support anonymous arrays.")

    val iteratorName = "I" + depth
    val typeName = typeRep.typeName.getOrElse(adaFriendlyTypeName(typeRep))

    typeRep match {
      case x: StructRep =>
        for (component <- x.components) {
          printEncodeComponentIntoPayload(valueName + "." + component.name, component.typeRep, depth + 1)
        }

      case arr: ArrayRep =>
        if (arr.isInstanceOf[VariableArrayRep]) {
          println(s"XDR.Encode(XDR.XDR_Unsigned($valueName'Length), Payload.all, Position, Last);")
          println("Position := Last + 1;")
        }

        println(s"for $iteratorName of $valueName loop")
        indentationLevel += 1
        printEncodeComponentIntoPayload(iteratorName, arr.elementType, depth + 1)
        indentationLevel -= 1
        println("end loop;")

      case _: EnumRep =>
        println(s"XDR.Encode(XDR.XDR_Unsigned($typeName'Pos($valueName)), Payload.all, Position, Last);")
        println("Position := Last + 1;")

      case _: StringRep =>
        println("XDR.Encode(XDR.XDR_Unsigned(" + valueName + "'Length), Payload.all, Position, Last);")
        println("Position := Last + 1;")
        // The first iteration is done on message components.
        // For encoder procedures, it's more convenient to pass in by value than reference.
        // Nested data structures always refer to strings by reference though.
        if (depth == 0)
          println(s"XDR.Encode(String($valueName), Payload.all, Position, Last);")
        else
          println(s"XDR.Encode(String($valueName.all), Payload.all, Position, Last);")
        println("Position := Last + 1;")

      case rep: NumericRep =>
        println(s"XDR.Encode(XDR.${xdrTypeName(rep)}($valueName), Payload.all, Position, Last);")
        println("Position := Last + 1;")

      case _: BoolRep =>
        println(s"XDR.Encode(XDR.XDR_Boolean'Val($typeName'Pos($valueName)), Payload.all, Position, Last);")
        println("Position := Last + 1;")

      case _: TimeSpanRep =>
        println("declare")
        indentationLevel += 1
        println("type Time_Float is delta 0.000_000_001 digits 18;")
        println("type Big_Float is delta 1.0 digits 18;")
        printlnOne("begin")
        println(s"XDR.Encode(XDR_Unsigned_Hyper(Time_Float(To_Duration($valueName)) * Big_Float(1_000_000_000.0)), Payload.all, Position, Last);")
        println("Position := Last + 1;")
        indentationLevel -= 1
        println("end;")

      case _: TimeRep =>
        println("declare")
        indentationLevel += 1
        println("Seconds : Seconds_Count;")
        println("Frac : Time_Span;")
        println("Result : XDR_Unsigned_Hyper;")
        printlnOne("begin")
        println(s"Split($valueName, Seconds, Frac);")
        println("Result := XDR_Unsigned_Hyper(Seconds) * 1_000_000_000 + XDR_Unsigned_Hyper(To_Duration(Frac) * 1_000_000_000);")
        println(s"XDR.Encode(Result, Payload.all, Position, Last);")
        println("Position := Last + 1;")
        indentationLevel -= 1
        println("end;")

      case _: OpaqueRep =>

        if (typeRep.isInstanceOf[VariableOpaqueRep]) {
          println(s"XDR.Encode(XDR.XDR_Unsigned($valueName'Length), Payload.all, Position, Last);")
          println("Position := Last + 1;")
        }
        // The first iteration is done on message components.
        // For encoder procedures, it's more convenient to pass in by value than reference.
        // Nested data structures always refer to strings by reference though.
        if (depth != 0 && typeRep.isInstanceOf[VariableOpaqueRep])
          println(s"XDR.Encode(Octet_Array($valueName.all), Payload.all, Position, Last);")
        else
          println(s"XDR.Encode(Octet_Array($valueName), Payload.all, Position, Last);")
        println("Position := Last + 1;")

      case x => throw new Error(s"Unimplemented component type ${x.toString}.")
    }
  }

  /**
   * Prints the unsafe send message procedure for the given message.
   * @param message The message type.
   * @param direction The direction the message is sent.
   * @param withStatus If the resulting procedure should include a send result status.
   */
  private def printUnsafeSend(message: MStructRep, direction: MessageDirection.Value, withStatus: Boolean): Unit = {
    val msgName = message.typeName.get
    println("procedure Send_" + msgName)
    indentationLevel += 1

    // Parameters
    val struct = new AdaGeneratorMessageHelper(message)
    val (_, _) = struct.printUnsafeSenderParams(indentationLevel, out, withStatus)
    indentationLevel -= 1

    // Declarations
    println("is")
    indentationLevel += 1
    println("Message : Message_Record;")
    indentationLevel -= 1

    // Implementation
    println("begin")
    indentationLevel += 1
    println("pragma Assert(Payload(Message) = null);")
    // Encode the message
    println(s"${msgName}_Encode(")
    indentationLevel += 1

    println("Sender_Address => (This_Domain.ID, Module_ID(Sender)),")
    println("Receiver_Address => Receiver_Address,")
    println("Request_ID => Request_ID,")

    struct.getDataParams.foreach(param => {
      println(param._1 + " => " + param._1 + ",")
    })

    println("Result => Message,")
    println("Priority => Priority);")
    indentationLevel -= 1

    // Send the message
    if (withStatus) println("Message_Manager.Send_Message(Sender, Message, Status);")
    else println("Message_Manager.Send_Message(Sender, Message);")

    indentationLevel -= 1
    println(s"end Send_$msgName;")
    println()
  }

  /**
   * Print the implementation of the safe message send procedure.
   * @param message The message being sent.
   * @param direction Whether the message is one sent or received
   *                  from the module.
   * @param withStatus If the resulting procedure should include a send
   *                   result status parameter.
   */
  private def printSafeSend(message: MStructRep, direction: MessageDirection.Value, withStatus: Boolean): Unit = {
    val msgName = message.typeName.get
    println("procedure Send_" + msgName)
    indentationLevel += 1

    // Parameters
    val struct = new AdaGeneratorMessageHelper(message)
    val (_, _) = struct.printSafeSenderParams(indentationLevel, out, withStatus)

    // Declarations
    printlnOne("is")
    println("Message : Message_Record;")
    if (!withStatus) println("Status : Status_Type := Unavailable;")

    // Implementation
    printlnOne("begin")
    println("pragma Assert(Payload(Message) = null);")
    // Encode the message
    println(s"${msgName}_Encode(")
    indentationLevel += 1

    println("Sender_Address => (This_Domain.ID, Module_ID(Sender)),")
    if (withStatus) println("Receiver_Address => (Domain_ID, Receiving_Module.Module_ID),")
    else println("Receiver_Address => (Receiving_Domain.ID, Receiving_Module.Module_ID),")
    println("Request_ID => Request_ID,")

    struct.getDataParams.foreach(param => {
      println(param._1 + " => " + param._1 + ",")
    })

    println("Result => Message,")
    println("Priority => Priority);")
    indentationLevel -= 1

    // Send the message
    if (withStatus) println("Message_Manager.Send_Message(Sender, Message, Receiving_Module, This_Domain, Status);")
    else println("Message_Manager.Send_Message(Sender, Message, Receiving_Module, Receiving_Domain, Status);")
    if (!withStatus) println("pragma Unused(Status);")

    indentationLevel -= 1
    println(s"end Send_$msgName;")
    println()
  }

  /**
   * Prints the local variable declarations that will
   * be needed to decode the given message.component name.
   *
   * procedure Decode_...
   * is
   * ... <- Prints this stuff
   * begin
   *
   * Depending on what the component's type, there
   * may be zero or more declarations. The declarations
   * take the form Raw_prefix_componentName.
   *
   * This function is recursive for some component types.
   * Each recursion prepends to the prefix the name of the outer
   * component and "_".
   *
   * @param prefix The string that should prefix the component name.
   *               Must end but not begin with an underscore.
   *               "OuterComponent_", ""
   * @param component The component to declare for.
   */
  private def printDeclarationsFor(prefix: String, component: StructComponent): Unit = {
    val componentName = component.name
    assert(!prefix.contains("__"), "Ada doesn't support \"__\" in names.")

    component.typeRep match {
      case struct: StructRep =>
        for (c <- struct.components) {
          printDeclarationsFor(prefix + componentName + "_", c)
        }
      case arr: ArrayRep =>
        if (arr.isInstanceOf[VariableArrayRep])
          println(s"$prefix${componentName}_Size : XDR_Unsigned;")

        arr.elementType match {
          case struct: StructRep =>
            for (c <- struct.components)
              printDeclarationsFor(prefix + componentName + "_", c)
          case _: EnumRep => println("Raw_" + prefix + componentName + " : XDR.XDR_Unsigned;")
          case _: StringRep => println("Raw_" + prefix + componentName + "_Size : XDR.XDR_Unsigned;")
          case _: BoolRep =>
            println("Raw_" + prefix +  componentName + "   : XDR.XDR_Boolean;")
          case rep: NumericRep =>
            println(s"Raw_$prefix$componentName : ${xdrTypeName(rep)};")
          case _: OpaqueRep =>
        }
      case _: EnumRep =>
        println("Raw_" + prefix + componentName + " : XDR.XDR_Unsigned;")
      case _: StringRep => println("Raw_" + prefix + componentName + "_Size : XDR.XDR_Unsigned;")
      case _: BoolRep =>
        println("Raw_" + prefix + componentName + "   : XDR.XDR_Boolean;")
      case rep: NumericRep =>
        println(s"Raw_$prefix$componentName : ${xdrTypeName(rep)};")
      case _: TimeSpanRep =>
        println(s"Raw_$prefix$componentName  : XDR.XDR_Unsigned_Hyper;")
      case _: TimeRep =>
        println("Raw_" + prefix + componentName + "   : XDR.XDR_Unsigned_Hyper;")
      case _: VariableOpaqueRep =>
//        println(s"Raw_$prefix${componentName}_Size : XDR.XDR_Unsigned;")
      case _: FixedOpaqueRep =>
      case r =>
        throw new Error(s"Unimplemented declarations for $r")
    }
  }

  /**
   * Generate decoder function for message.
   * @param message The message to decode.
   */
  private def printDecode(message: MStructRep): Unit = {
    val msgName = message.typeName.get
    val decodeString = "_Decode"
    println("procedure " + msgName + decodeString)
    indentationLevel += 1

    // The message direction doesn't matter here
    val rep = new AdaGeneratorMessageHelper(message)
    rep.printDecoderParams(indentationLevel, out)

    printlnOne("is")                                    // IS

    println("Position : Data_Index_Type;")

    // Print any intermediate storage variables needed for the message struct
    for (component <- message.components)
      printDeclarationsFor("", component)

    println("Last : Data_Index_Type;")
    indentationLevel -= 1

    println("begin")                                 // BEGIN
    indentationLevel += 1

    println("Decode_Status := Success;")
    // Set initial values for every component
    for (component <- message.components) {
      printAssignInitialValue(component.name, component.typeRep)
    }
    println("Position := 0;")
  println()
    println("-- Begin Decoding")
    for (component <- message.components) {
      printDecodeForItem(component.name, "Raw_" + component.name, component.typeRep)
    }

    indentationLevel -= 1
    println("end " + msgName + decodeString + ";")
    println()
  }

  /**
   * Prints the Ada logic to read the message values from the payload,
   * check that the result isn't malformed, then assign those values
   * to the out parameters of the decode procedure.
   * @param assignTarget The Ada string proceeding ":=" to assign the
   *                     read value to. If the type is an array, does not
   *                     include the index.
   * @param tempStore The Ada variable that should be used to store the
   *                  raw XDR value that was read from the payload before
   *                  assigning it to the output. Usually Raw_*. Must be
   *                  of the type being read. If we're reading an array,
   *                  this variable represents a single element of it.
   * @param typeRep The type of the data being decoded.
   * @param depth The number of recursions deep the function is.
   */
  private def printDecodeForItem(assignTarget: String, tempStore: String, typeRep: Rep, depth: Int = 0): Unit = {
    val iteratorName = "I" + depth

    def start_if(condition: String): Unit = {
      println(s"if $condition then")
      indentationLevel += 1
    }

    def decode_and_increment(storeIn: String): Unit = {
      println(s"XDR.Decode(Payload(Message).all, Position, $storeIn, Last);")
      println("Position := Last + 1;")
    }

    def assign(value: String): Unit = {
      println(s"$assignTarget := $value;")
    }

    // Note that SPARK has a hard time with if not condition return early proofs.
    def return_early_if(condition: String): Unit = {
      println(s"if $condition then Decode_Status := Malformed; return; end if;")
    }
    def else_return(): Unit = {
      printlnOne("else")
      println("Decode_Status := Malformed;")
      println("return;")
      indentationLevel -= 1
      println("end if;")
    }

    val typeName = typeRep.typeName.getOrElse(adaFriendlyTypeName(typeRep))

    typeRep match {
      case rep: StructRep =>
        println(s"-- Decoding ${rep.typeName.get}")
        for (component <- rep.components) {
          printDecodeForItem(assignTarget  + '.' + component.name, tempStore + "_" + component.name, component.typeRep, depth + 1)
        }

      case arr: FixedArrayRep =>
        println(s"for $iteratorName in 1 .. ${arr.size.value} loop -- Decoding elements of ${arr.typeName.get} of type ${arr.elementType.typeName.getOrElse(adaFriendlyTypeName(arr.elementType))}")
        indentationLevel += 1
        printDecodeForItem(s"$assignTarget($iteratorName)", tempStore, arr.elementType, depth + 1)
        indentationLevel -= 1
        println("end loop;")

      case arr: VariableArrayRep =>
        val sizeStore = assignTarget.replace('.', '_') + "_Size";
        decode_and_increment(sizeStore)

        return_early_if(s"$sizeStore > ${arr.maxSize.value}")

        assign(s"new ${arr.typeName.get}(0 .. $assignTarget'Length - 1)")

        println(s"for $iteratorName in 0 .. Natural($sizeStore) - 1 loop -- Decoding elements of ${arr.typeName.get} of type ${arr.elementType.typeName.getOrElse(adaFriendlyTypeName(arr.elementType))}")
        indentationLevel += 1
        printDecodeForItem(s"$assignTarget($iteratorName)", tempStore, arr.elementType, depth + 1)
        indentationLevel -= 1
        println("end loop;")

      case rep: EnumRep =>
        val enumName = rep.typeName.get
        decode_and_increment(tempStore)
        start_if(s"$tempStore in $enumName'Pos($enumName'First) .. $enumName'Pos($enumName'Last)")
        assign(s"$enumName'Val($tempStore)")
        else_return()

      case s: StringRep =>
        val sizeStore = assignTarget.replace('.', '_') + "_Size"

        println("declare")
        indentationLevel += 1
        println(s"$sizeStore : XDR_Unsigned;")
        printlnOne("begin")

        println(s"XDR.Decode(Payload(Message).all, Position, $sizeStore, Last);")
        println("Position := Last + 1;")
        start_if(s"$sizeStore < ${s.maxLength.toString}")
        // The string length is valid
        println("declare")
        indentationLevel += 1
        println(s"Final_String_Size : constant XDR_Unsigned := $sizeStore;")
        println(s"subtype Definite_String is ${s.typeName.getOrElse("String")}(1 .. Integer(Final_String_Size));")
        printlnOne("begin")

        println(s"Free($assignTarget);")
        println(s"${assignTarget} := new Definite_String'(others => ' ');")
        decode_and_increment(assignTarget + ".all")

        indentationLevel -= 1
        println("end;")
        else_return()
        indentationLevel -= 1
        println("end;")

      case rep: BoolRep =>
        val typeName = rep.typeName.getOrElse(adaFriendlyTypeName(rep))
        decode_and_increment(tempStore)
        assign(s"$typeName'Val(XDR.XDR_Boolean'Pos($tempStore))")

      case _: ContinuousRep =>
        println("declare")
        indentationLevel += 1
        println("Special : Special_Float_Value;")
        printlnOne("begin")

        println(s"XDR.Decode(Payload(Message).all, Position, $tempStore, Last, Special);")
        println("Position := Last + 1;")
        val xdrName = "XDR." + xdrTypeName(typeRep)
        start_if(s"$tempStore in $xdrName($typeName'First) .. $xdrName($typeName'Last) and Special = None")
        assign(s"$typeName($tempStore)")
        else_return()

        indentationLevel -= 1
        println("end;")

      case _: NumericRep =>
        println(s"XDR.Decode(Payload(Message).all, Position, $tempStore, Last);")
        println("Position := Last + 1;")
        val xdrName = "XDR." + xdrTypeName(typeRep)
        start_if(s"$tempStore in $xdrName($typeName'First) .. $xdrName($typeName'Last)")
        assign(s"$typeName($tempStore)")
        else_return()

      case _: TimeSpanRep =>
        println(s"XDR.Decode(Payload(Message).all, Position, $tempStore, Last);")
        println("Position := Last + 1;")
        assign(s"Seconds(Integer($tempStore / 1_000_000_000)) + Nanoseconds(Integer($tempStore mod 1_000_000_000))")

      case _: TimeRep =>
        println(s"XDR.Decode(Payload(Message).all, Position, $tempStore, Last);")
        println("Position := Last + 1;")
        assign(s"$typeName(Ada.Real_Time.Time_Of(Seconds_Count($tempStore / 1_000_000_000), Nanoseconds(Integer($tempStore mod 1_000_000_000))))")

      case VoidRep => // do nothing!
      
      case rep: VariableOpaqueRep =>
        val sizeStore = assignTarget.replace('.', '_') + "_Size"
        println("declare")
        indentationLevel += 1

        println(s"$sizeStore : XDR_Unsigned;")
        printlnOne("begin")

        println(s"XDR.Decode(Payload(Message).all, Position, $sizeStore, Last);")
        println("Position := Last + 1;")

        start_if(s"$sizeStore <= XDR_Unsigned(${rep.maxBytes.value})")

        println("declare")
        indentationLevel += 1
        println(s"Final_Size : constant XDR_Unsigned := $sizeStore;")
        println(s"subtype Definite_Octet_Array is $typeName(0 .. Natural(Final_Size) - 1);")
        printlnOne("begin")
        println(s"Free($assignTarget);")
        println(s"$assignTarget := new Definite_Octet_Array'(others => 0);")
        indentationLevel -= 1
        println("end;")

        println(s"XDR.Decode(Payload(Message).all, Position, Octet_Array(${assignTarget}.all), Last);")
        println("Position := Last + 1;")
        else_return()
        indentationLevel -= 1
        println("end;")

      case _: FixedOpaqueRep =>
        println(s"XDR.Decode(Payload(Message).all, Position, Octet_Array(${assignTarget}), Last);")
        println("Position := Last + 1;")

      case _ =>
        throw new Error("Unimplemented decode procedure")

    }
  }

  /**
   * Prints an Ada statement assigning an arbitrary initial value to the
   * given target. The target is an out parameter of a decoder function
   * and as such should use Ada friendly types.
   * @param assignTarget The string preceding the := operator.
   * @param typeRep The type of data being assigned.
   * @param depth Counts the number of recursions.
   */
  private def printAssignInitialValue(assignTarget: String, typeRep: Rep, depth: Int = 0): Unit = {
    val iteratorName = "I" + depth.toString

    val typeName = typeRep.typeName.getOrElse(adaFriendlyTypeName(typeRep))

    typeRep match {
      case _: NumericRep =>
        println(s"$assignTarget := $typeName'Last;")
      case _: BoolRep =>
        println(s"$assignTarget := False;")
      case e: EnumRep =>
        println(s"$assignTarget := ${e.typeName.get}'First;")
      case s: StringRep =>
        println("declare")
        indentationLevel += 1
        println(s"subtype Definite_String is ${s.typeName.getOrElse("String")}(1..0);")
        printlnOne("begin")
        println(s"$assignTarget := new Definite_String'(others => ' ');")
        indentationLevel -= 1
        println("end;")
      case arr: ArrayRep =>
        val elementRep = arr.elementType

        doIndentation()
        out.print(s"for $iteratorName in ")

        arr match {
          case _: FixedArrayRep =>
            out.println(s"${assignTarget}'Range loop")
          case _: VariableArrayRep =>
            val sizeVar = assignTarget + "'Length - 1"
            out.println(s"0 .. $sizeVar loop")
        }
        indentationLevel += 1

        printAssignInitialValue(s"$assignTarget($iteratorName)", elementRep, depth + 1)

        indentationLevel -= 1
        println("end loop;")
      case struct: StructRep =>
        for (component <- struct.components) {
          printAssignInitialValue(assignTarget + "." + component.name, component.typeRep, depth + 1)
        }
      case _: TimeRep =>
        println(s"$assignTarget := $typeName(Time_First);")
      case _: TimeSpanRep =>
        println(s"$assignTarget := $typeName(Time_Span_Zero);")
      case _: VariableOpaqueRep =>
        println(s"$assignTarget := new $typeName'($typeName(Zero_Width_Octet_Array));")
      case _: FixedOpaqueRep =>
        println(s"$assignTarget := (others => 0);")
      case x => throw new Error(s"Unimplemented assign initial value for ${x}")
    }
  }

  /**
   * Prints all necessary content for this message type.
   * @param message The message.
   */
  private def handleMessage(message: MStructRep): Unit = {
    assert(!message.components.exists(component => component.typeRep.isInstanceOf[ArrayRep] && component.typeRep.isAnonymous))

    val voidFlag = message.isVoid

    if (voidFlag && message.components.size > 1) {
      System.out.println("Can't have multiple message struct")
      System.out.println("parameters included with void.")
      return
    }

    val direction = message.direction

    printEncode(message)
    printUnsafeSend(message, direction, withStatus = true)
    printUnsafeSend(message, direction, withStatus = false)
    printSafeSend(message, direction, withStatus = true)
    printSafeSend(message, direction, withStatus = false)
    if (!voidFlag)
      printDecode(message)
    println()
  }

  /**
   * Prints the given content with the correct indentation and
   * a newline at the end.
   *
   * @param content The string to print, doesn't include the newline.
   */
  private def println(content: String = ""): Unit = {
    doIndentation()
    out.println(content)
  }

  /**
   * Prints the given content with one less indentation than
   * the current and a newline at the end. Doesn't affect the
   * global indentation.
   * @param content The content to print, doesn't include the newline.
   */
  private def printlnOne(content: String): Unit = {
    for (_ <- 0 until indentationLevel - 1) {
      out.print("   ")
    }
    out.println(content)
  }
}
