package edu.vtc.merc

import edu.vtc.merc.TypeRep.StructComponent

/**
 * The interface to symbol tables.
 */
trait SymbolTable {
  def getStructComponentNames(name: String): Iterable[String]
  def getStructComponentTypeName(name: String, subName: String): String
  def getTypeNames: Iterable[String]
  def getMStructs: List[String]
  def getComponentDefaultValue(name: String, subName: String): String
  def getArraySSize(name: String, id: String): String
}

/**
 * Defines several exception classes used by all symbol table classes.
 */
object SymbolTable {
  class SymbolTableException(message: String) extends Exception(message)
  class UnknownObjectNameException(message: String) extends SymbolTableException(message)
  class UnknownTypeNameException(message: String) extends SymbolTableException(message)
  class DuplicateObjectNameException(message: String) extends SymbolTableException(message)
  class DuplicateTypeNameException(message: String) extends SymbolTableException(message)
  class ConflictingNameException(message: String) extends SymbolTableException(message)
}

object MessageDirection extends Enumeration {
  type Main = Value
  /**
   * A message that is received by the module.
   */
  val In: MessageDirection.Value = Value(0);
  /**
   * A message that is sent from the module.
   */
  val Out: MessageDirection.Value = Value(1);
}
