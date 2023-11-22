package edu.vermontstate.merc

import edu.vermontstate.merc.TypeRep.{ConstRep, FixedArrayRep, MStructRep, MXDREntity, Rep, StructRep, StructuredRep}
import SymbolTable.{DuplicateObjectNameException, DuplicateTypeNameException, UnknownObjectNameException, UnknownTypeNameException}

import scala.collection.mutable

/**
 * A simple implementation of the SymbolTable trait that stores symbols in in-memory maps.
 */
class BasicSymbolTable extends SymbolTable {

  /**
   * Maps symbols to types.
   */
  private val types = mutable.HashMap[String, Rep]()

  /**
   * Maps symbols to constants.
   */
  private val constants = mutable.HashMap[String, ConstRep[Rep]]()

  /**
   * Stores all the symbols in the table in the order they were added.
   */
  private val order = mutable.ListBuffer[String]()

  /**
   * Adds the given type to the table.
   * @param symbol The name of the type.
   * @param rep The type representation.
   * @throws DuplicateTypeNameException If table already contains a type
   *                                    with the given name.
   */
  def addType(symbol: String, rep: Rep): Unit = {
    if (types.contains(symbol))
      throw new DuplicateTypeNameException("Type " + symbol + " has already been defined.")
    types(symbol) = rep
    order.addOne(symbol)
  }

  /**
   * Adds a constant to the symbol table.
   * @param symbol The key for the constant.
   * @param rep The constant stored at the key. Must be named.
   * @throws DuplicateObjectNameException If table already contains a constant with
   *                                      the given symbol.
   */
  def addConstant(symbol: String, rep: ConstRep[Rep]): Unit = {
    if (constants.contains(symbol)) {
      throw new DuplicateObjectNameException(symbol + " is already a defined constant.")
    }
    assert(rep.name.get == symbol)
    constants(symbol) = rep
    order.addOne(symbol)
  }

  /**
   * Adds either a type or a constant.
   * @param symbol
   * @param entity
   */
  def addEntity(symbol: String, entity: MXDREntity): Unit = {
    entity match {
      case x: ConstRep[Rep] =>
        addConstant(symbol, x)
      case x: Rep =>
        addType(symbol, x)
    }
  }

  /**
   * Iterates through all the symbols in the table in the order
   * they were added, with their definition.
   *
   * All constants will have names.
   * @return
   */
  def getAll(): Iterator[(String, MXDREntity)] = {
    return order.map(symbol => (symbol, types.getOrElse(symbol, constants(symbol)))).iterator
  }

  /**
   * Returns iterable of variable names for the components
   * the given struct.
   * @param name The name of the struct.
   * @return Iterable of component variable names.
   */
  override def getStructComponentNames(name: String): Iterable[String] = {
    getType[StructRep](name).components.map(r => r.name)
  }

  /**
   * Gets the type of a component in a structure.
   * This is different than the type, which can be user defined,
   * like Write_Size_Type is a user defined type.
   * This function returns the underlying type, of which there are
   * a predefined finite set. The underlying type of this example
   * might be UInt.
   * @param structName Name of a message struct.
   * @param componentName Name of component in the given message struct.
   * @return The underlying type name for the specified message component,
   *         or "" if the type id unknown.
   */
  def getStructComponentTypeName(structName: String, componentName: String): String = {
    val rep = getType[StructRep](structName).getComponent(componentName).typeRep
    var v = rep.toString

    if (v.contains("(")) {
      if (v.substring(0, v.indexOf("(")).contentEquals("IDRep")) {
        v = v.substring(v.indexOf("(") + 1)
      }
      if (v.contains("(")) {
        v = v.substring(0, v.indexOf("("))
      }
      if (v.contains(")")) {
        v = v.substring(0, v.indexOf(")"))
      }
    }

    v
  }

  /**
   * Gets the underlying type of a component in a structure.
   * This is different than the type, which can be user defined,
   * like Write_Size_Type is a user defined type.
   * This function returns the underlying type, of which there are
   * a predefined finite set. The underlying type of this example
   * might be UInt.
   *
   * @param structName    Name of a message struct.
   * @param componentName Name of component in the given message struct.
   * @return The underlying type rep for the specified message component.
   */
  def getStructComponentTypeRep(structName: String, componentName: String): TypeRep.Rep = {
    getType[StructRep](structName).getComponent(componentName).typeRep
  }


  /**
   * Gets the default value of a component of a struct.
   * @param structName The name of the struct in the table.
   * @param componentName The name of the component.
   * @return The default value, or "" if the component couldn't be found in the given struct,
   *         or "null" if the no default value was specified for the component.
   */
  override def getComponentDefaultValue(structName: String, componentName: String): String = {
    val struct = getType[StructRep](structName)

    struct.getComponent(componentName).value.getOrElse("")
  }


  /**
   * @return Iterable for all the unstructured type names
   */
  override def getTypeNames: Iterable[String] = {
    types.filter(element => !element._2.isInstanceOf[StructuredRep]).keys
  }

  /**
   * Gets the type referred to by a symbol.
   * @param symbol The name representing the type.
   * @tparam T The type of type. If you don't know, just use Rep.
   * @return
   * @throws UnknownObjectNameException If the symbol isn't in the table
   *                                    with the correct type.
   */
  def getType[T <: Rep](symbol: String): T = {
    try {
      types(symbol).asInstanceOf[T]
    }
    // Translate the exceptions to more meaningful ones.
    catch {
      case _: NoSuchElementException =>
        throw new UnknownObjectNameException(s"$symbol is not the name of an object.")
      case _: ClassCastException =>
        throw new UnknownObjectNameException(s"$symbol does not have the expected type.")
    }
  }

  /**
   * Gets the constant referred to by the given symbol.
   * @param symbol The name of the constant.
   * @tparam T The type of the constant. If unknown, just use Rep.
   *          If you know it's some type of number, use NumericRep.
   * @return The found constant.
   */
  def getConstant[T <: Rep](symbol: String): ConstRep[T] = {
    try {
      constants(symbol).asInstanceOf[ConstRep[T]]
    }
    // Translate the exceptions to more meaningful ones.
    catch {
      case _: NoSuchElementException =>
        throw new UnknownObjectNameException(s"$symbol is not the name of a constant.")
      case _: ClassCastException =>
        throw new UnknownObjectNameException(s"$symbol is not a constant of the expected type.")
    }
  }

  /**
   * Returns the size of the array in the struct.
   * (Upper bound)
   * @param structName The name of the struct.
   * @param componentName The name of an fixed-array component.
   * @return
   */
  def getArraySSize(structName: String, componentName: String): String = {
    getType[StructRep](structName).getComponent(componentName).typeRep.asInstanceOf[FixedArrayRep].size.value
  }

  /**
   * @return a list of all message structs by name.
   */
  def getMStructs: List[String] = {
    types.filter(pair => pair._2.isInstanceOf[MStructRep]).keys.toList
  }

  /**
   * Checks to see if a given type name is already present in the table.
   * @param symbol The name to search for.
   * @return True if the table has the name mapped to a type.
   */
  def hasType(symbol: String): Boolean = {
    types.contains(symbol)
  }
}
