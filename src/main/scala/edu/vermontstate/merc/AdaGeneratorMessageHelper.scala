package edu.vermontstate.merc

import edu.vermontstate.merc.TypeRep.{ArrayRep, MStructRep, OpaqueRep, StringRep, VoidRep}
import AdaGeneratorCommon.{adaFriendlyTypeName, passedByReference}

import java.io.PrintStream

/**
 * This class wraps a message struct and can produce certain Ada
 * code used by the generators. This functionality should be moved into the AdaGeneratorCommon
 * file as individual functions rather than an object.
 */
class AdaGeneratorMessageHelper(msgRep: MStructRep) {
  /**
   * The name of the message type
   * "Tick_Reply"
   */
  val msgName: String = msgRep.typeName.get
  /**
   * Prints the parameters used in the encoder function.
   * Ex)
   *   (Name : Type;
   *   Name : Type)
   * @return stringFlag and dataFlag
   */
  def printEncoderParams(indentationLvl: Int, out: PrintStream): (String, String) = {
    def println(content: String): Unit = {
      out.print("   " * indentationLvl)
      out.println(content)
    }

    println("(Receiver_Address : in Message_Address;")
    println("Sender_Address : in Message_Address;")
    println("Request_ID : in Request_ID_Type;")

    val (stringFlag, dataFlag) = printDataParamsForEncoder(indentationLvl, out)
    println("Priority : in System.Priority := System.Default_Priority;")
    println("Result : out  Message_Record)")

    (stringFlag, dataFlag)
  }

  /**
   * Prints the parameters used in the decoder function.
   *
   * (Message : in Message Record;
   * ... <- Data from the message
   * Decode_Status : Decode_Status_Type)
   *
   */
  def printDecoderParams(indentationLvl: Int, out: PrintStream): Unit = {
    def println(content: String): Unit = {
      out.print("   " * indentationLvl)
      out.println(content)
    }

    println("(Message : in Message_Record;")

    printDataParamsForDecoder(indentationLvl, out)

    // Final non-optional parameter is the resulting message
    println("Decode_Status : out Message_Status_Type)")
  }

  /**
   * Prints the parameters used in the unsafe sender function.
   * Ex)
   * (Name : Type;
   * Name : Type)
   *
   * @param withStatus If true, adds an out Status_Type parameter.
   * @return stringFlag and dataFlag
   */
  def printUnsafeSenderParams(indentationLvl: Int, out: PrintStream, withStatus: Boolean): (String, String) = {
    def println(content: String): Unit = {
      out.print("   " * indentationLvl)
      out.println(content)
    }

    println("(Sender : Module_Mailbox;")
    println("Receiver_Address : Message_Address;")
    println("Request_ID : Request_ID_Type;")

    val (stringFlag, dataFlag) = printDataParamsForEncoder(indentationLvl, out)

    if (withStatus) println("Status : out Status_Type;")
    println("Priority : System.Priority := System.Default_Priority)")

    (stringFlag, dataFlag)
  }

  /**
   * Prints the parameters used in the safe sender function.
   * Ex)
   * (Name : Type;
   * Name : Type)
   *
   * @param withStatus If true, add an out Status_Type parameter.
   * @return stringFlag and dataFlag
   */
  def printSafeSenderParams(indentationLvl: Int, out: PrintStream, withStatus: Boolean): (String, String) = {
    def println(content: String): Unit = {
      out.print("   " * indentationLvl)
      out.println(content)
    }

    println("(Sender : Module_Mailbox;")
    println("Receiving_Module : Module_Metadata;")
    println("Request_ID : Request_ID_Type;")

    val (stringFlag, dataFlag) = printDataParamsForEncoder(indentationLvl, out)

    if (withStatus) println("Status : out Status_Type;")
    else println("Receiving_Domain : Domain_Metadata := This_Domain;")
    println("Priority : System.Priority := System.Default_Priority)")

    (stringFlag, dataFlag)
  }

  private type Pair = (String, Option[String])

  /**
   * Gets all the custom data parameters for this message as
   * an array. The first
   * value is the parameter name, the second is its type, if
   * it has a subtype. Skips Voids.
   */
  def getDataParams: List[Pair] = {
    var out: List[Pair] = Nil

    // Parameter for each component of message
    val rep = msgRep
    for(component <- rep.components) {
      val typeRep = component.typeRep
      if (typeRep != VoidRep)
        out = out :+ (component.name, typeRep.baseTypeName)
    }

    out
  }

  /**
   * Prints all the data parameters needed for encode functions
   * at the correct indentation to the output stream.
   * Cannot print anonymous arrays.
   * Ex)
   * Data_1 : in String;
   * @return stringFlag and dataFlag
   */
  private def printDataParamsForEncoder(indentationLvl: Int, out: PrintStream): (String, String) = {
    def println(content: String): Unit = {
      out.print("   " * indentationLvl)
      out.println(content)
    }

    var stringFlag = ""
    var dataFlag = ""

    val components = msgRep.components
    for (component <- components) {
      val componentName = component.name
      val typeRep = component.typeRep

      if (typeRep == VoidRep) return (stringFlag, dataFlag)
      assert(if (component.typeRep.isInstanceOf[ArrayRep]) component.typeRep.typeName.nonEmpty else true,
        "Anonymous arrays aren't allowed in procedures in SPARK.")
      var typeName = typeRep.typeName.getOrElse(adaFriendlyTypeName(typeRep))

      if (passedByReference(typeRep)) typeName += "_Ptr"

      if (typeRep.isInstanceOf[StringRep]) stringFlag = componentName

      if (typeRep.isInstanceOf[OpaqueRep]) dataFlag = componentName


      val resultTypeName = component.typeRep.typeName.getOrElse(
        adaFriendlyTypeName(component.typeRep))

      println(s"$componentName : in $resultTypeName;")
    }

    (stringFlag, dataFlag)
  }

  /**
   * Prints all the data parameters needed for decode functions
   * at the correct indentation to the output stream.
   * Ex)
   * Data_1 : out String;
   * Data_1_Size : out XDR_Unsigned;
   *
   */
  private def printDataParamsForDecoder(indentationLvl: Int, out: PrintStream): Unit = {
    def println(content: String): Unit = {
      out.print("   " * indentationLvl)
      out.println(content)
    }

    for (component <- msgRep.components) {
      val componentName = component.name

      val componentTypeName = component.typeRep.typeName.getOrElse(
        adaFriendlyTypeName(component.typeRep))

      if (passedByReference(component.typeRep))
        println(s"$componentName : out ${componentTypeName}_Ptr;")
      else
        println(s"$componentName : out $componentTypeName;")

    }
  }
}
