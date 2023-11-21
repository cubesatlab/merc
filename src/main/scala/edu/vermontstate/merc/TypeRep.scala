package edu.vermontstate.merc

import scala.annotation.unused


/**
 * Defines several data types. These types can compose each other
 * and store data about themselves.
 */
object TypeRep {

  private val MAX_ARRAY_LENGTH: Int = (Math.pow(2, 32) - 1).toInt
  private val MAX_OPAQUE_LENGTH: Int = (Math.pow(2, 32) - 1).toInt
  private val MAX_STRING_LENGTH: Int = (Math.pow(2, 32) - 1).toInt

  /**
   * Creates an anonymous constant with the given value. Assumes the value is not symbolic and
   * is of the type given.
   * 
   * @param value The value of the constant.
   * @tparam T The constant's type.
   * @return A new constant.
   */
  def anonymousConstant[T <: Rep](value: String): ConstRep[T] = new ConstRep(None, value, false, None, None)

  sealed trait MXDREntity

  /**
   * Represents a data type in MXDR. The underlying type of the data is communicated by the name
   * of the inheriting class. Stores additional data about the type.
   */
  abstract class Rep(
                      /**
                       * If the type is explicitly declared a
                       * subtype, that subtype name.
                       * Ex)
                       * type Natural is Integer;
                       *
                       * baseTypeName would be "Integer"
                       *
                       * There is only a subtype if one was
                       * explicitly.
                       */
                      val baseTypeName: Option[String],
                    ) extends MXDREntity {

    /**
     * True if this type has an explicitly declared subtype. If true, baseTypeName is present.
     */
    val isSubtype: Boolean = baseTypeName.nonEmpty

    /**
     * The user defined type name of this type. An array of "double" (where double is an XDR
     * type) is an array of elements without a type name.
     */
    val typeName: Option[String] = None

    /**
     * Anonymous types might occur as array components in a struct.
     */
    val isAnonymous: Boolean = typeName.isEmpty
//    /**
//     * The minimum size of the type in bytes.
//     * Recursively calculated for composed types.
//     */
//    def minSize(): Int
//
//    /**
//     * The maximum size of the type in bytes.
//     * Recursively calculated for composed types.
//     */
//    def maxSize(): Int

    // Check that we didn't accidentally set the type value to an Option[String]. (Scala doesn't
    // do a type check here, just converts it to a string).
    assert(!typeName.getOrElse("").contains("("), "Type name may not contain '('")
    assert(!baseTypeName.getOrElse("").contains("("), "Base type name may not contain '('")
  }

  final object VoidRep extends Rep(None)

  // Unstructured types.

  /**
   * A class representing a type of boolean.
   * 
   * @param baseTypeName In cases where a "Boolean" is required in the MXDR, the typeName is
   * just "Boolean". If a typedef defines a new symbol for a Boolean, then this equals that
   * symbol.
   */
  case class BoolRep(override val typeName: Option[String],
                     override val baseTypeName: Option[String] = None,
                    ) extends Rep(baseTypeName)

  /**
   * A type with a numeric range on its values.
   */
  sealed trait Ranged {

    protected val explicitRange: Option[RangeConstraint]

    /**
     * Defined by the implementing type for cases when no custom range is imposed by the user.
     */
    def implicitLow(): Number
    /**
     * Defined by implementing types for cases when no custom range is imposed by the user.
     */
    def implicitHigh(): Number

    private def implicitRange(): RangeConstraint = new RangeConstraint(
      new ConstRep[NumericRep](null, implicitLow().toString, false, None, None),
      new ConstRep[NumericRep](null, implicitHigh().toString, false, None, None)
    )

    /**
     * True if the range was specified explicitly in the MXDR. If false, the range is implicit
     * to the type.
     */
    val isExplicit: Boolean = explicitRange.nonEmpty

    /**
     * The range of the type. If there is an explicit range, that range. Otherwise, the default
     * implicit range for the type.
     *
     * Even if the explicit/implicit range doesn't have typed endpoints, this value will by
     * inferring them.
     */
    val range: RangeConstraint = explicitRange.getOrElse(implicitRange())
  }

  /**
   * A category of types which are numeric and have a range.
   */
  abstract class NumericRep(typeName: Option[String]) extends Rep(typeName) with Ranged
  abstract class IntegralRep(typeName: Option[String]) extends NumericRep(typeName)
  abstract class ContinuousRep(typeName: Option[String]) extends NumericRep(typeName)

  case class DoubleRep(override val typeName: Option[String],
                       override val explicitRange: Option[RangeConstraint],
                       override val baseTypeName: Option[String] = None) extends ContinuousRep(baseTypeName) {

    override def implicitLow(): Number = -100.0
    override def implicitHigh(): Number = 100.0
  }
  case class FloatRep(override val typeName: Option[String],
                      override val explicitRange: Option[RangeConstraint] = None,
                      override val baseTypeName: Option[String] = None) extends ContinuousRep(baseTypeName) {
    override def implicitLow(): Number = -100.0
    override def implicitHigh(): Number = 100.0
  }
  case class HyperRep(override val typeName: Option[String],
                      override val explicitRange: Option[RangeConstraint] = None,
                      override val baseTypeName: Option[String] = None) extends IntegralRep(baseTypeName) {
    override def implicitLow(): Number = -100.0
    override def implicitHigh(): Number = 100.0
  }
  case class IntRep(override val typeName: Option[String],
                    override val explicitRange: Option[RangeConstraint] = None,
                    override val baseTypeName: Option[String] = None) extends IntegralRep(baseTypeName) {
    override def implicitLow(): Number = -2147483648
    override def implicitHigh(): Number = 2147483647
  }

  /**
   * A data type for data for which the structure is unknown.
   */
  abstract class OpaqueRep extends Rep(None)

  /**
   * The variable length opaque type.
   * 
   * @param maxBytes The maximum number of bytes this opaque type may hold. If omitted, a
   * default value specific to Merc is used.
   * 
   * @param typeName The name of the type.
   */
  case class VariableOpaqueRep(override val typeName: Option[String],
                               maxBytes: ConstRep[IntegralRep] = anonymousConstant(MAX_OPAQUE_LENGTH.toString)) extends OpaqueRep

  /**
   * A fixed length opaque type.
   * 
   * @param bytes The length of the opaque in bytes.
   */
  case class FixedOpaqueRep(override val typeName: Option[String],
                            bytes: ConstRep[IntegralRep]) extends OpaqueRep

  case class QuadRep(override val typeName: Option[String],
                     override val explicitRange: Option[RangeConstraint],
                     override val baseTypeName: Option[String] = None) extends IntegralRep(baseTypeName) {
    override def implicitLow(): Number = -100
    override def implicitHigh(): Number = 100
  }

  /**
   * A string type.
   * 
   * @param maxLength The maximum length of the string in bytes. Note that in XDR, one byte is
   * one ASCII character. Strings are prefixed by a 32-bit uint for the length. If unspecified,
   * defaults to the maximum length that can be stored in a 32-bit int. If specified, must be
   * lower than the max string length.
   * 
   * Note that there is no Fixed-length string type. All strings are variable length.
   */
  case class StringRep(override val typeName: Option[String],
                       var maxLength: Long = MAX_STRING_LENGTH,
                       override val baseTypeName: Option[String] = None) extends Rep(baseTypeName) {
    repOk()

    private def repOk(): Unit = {
      assert(maxLength <= MAX_STRING_LENGTH)
    }
  }

  /**
   * A time type.
   */
  case class TimeRep(override val typeName: Option[String],
                override val baseTypeName: Option[String] = None) extends Rep(baseTypeName)
  case class TimeSpanRep(override val typeName: Option[String],
                    override val baseTypeName: Option[String] = None) extends Rep(baseTypeName)
  case class UHyperRep(override val typeName: Option[String],
                       override val explicitRange: Option[RangeConstraint] = None,
                       override val baseTypeName: Option[String] = None) extends IntegralRep(baseTypeName) {
    override def implicitLow(): Number = 0
    override def implicitHigh(): Number = Math.pow(2, 64).toLong - 1
  }
  case class UIntRep(override val typeName: Option[String],
                     override val explicitRange: Option[RangeConstraint] = None,
                     override val baseTypeName: Option[String] = None) extends IntegralRep(baseTypeName) {

    override def implicitLow(): Number = 0
    override def implicitHigh(): Number = 4294967295L
  }

  // Structured and composite types.

  sealed trait StructuredRep extends Rep

  /**
   * Represents a symbol with an associated value. Optionally stores the type of the value.
   *
   * @param name The name of the symbol, or null if the constant is a literal.
   *
   * @param value The value of the symbol. May be the name of another constant, or a value. For
   * example, the following are all valid values: `Type'Last`, `10`, `3.14`,
   * `Name_Of_Other_Constant`.
   *
   * @param isSymbolic True if the value is a symbol, false if it can be converted strait to its
   * value via ".toInt"
   *
   * @param typeName The string in the MXDR of the type.
   *                 
   * @param typeRep The type of the constant, if the type was specified.
   */
  class ConstRep[+T <: Rep](val name: Option[String],
                            val value: String,
                            val isSymbolic: Boolean,
                            val typeName: Option[String],
                            val typeRep: Option[T]) extends MXDREntity

  /**
   * An array type.
   */
  abstract class ArrayRep extends Rep(None) with StructuredRep {
    /**
     * The type of element the array stores. Must be of a named type.
     */
    val elementType: Rep

    /**
     * Creates a new array but with the given element type.
     */
    def butWith(rep: Rep): ArrayRep
  }

  /**
   * Array type for fixed length arrays.
   * 
   * @param size A constant representing the length of the array.
   * 
   * @param elementType The element type of the array.
   */
  case class FixedArrayRep(override val typeName: Option[String],
                           size: ConstRep[IntegralRep],
                           override val elementType: Rep) extends ArrayRep {
    override def butWith(rep: Rep): FixedArrayRep = {
      new FixedArrayRep(typeName, size, rep)
    }
  }

  /**
   * Array type for variable length arrays.
   * 
   * @param elementType The element type of the array.
   * 
   * @param maxSize The maximum length of the array. If no value is specified, defaults to a
   * maximum array length.
   */
  case class VariableArrayRep(override val typeName: Option[String],
                              override val elementType: Rep,
                              maxSize: ConstRep[IntegralRep] = anonymousConstant(MAX_ARRAY_LENGTH.toString)) extends ArrayRep {
    override def butWith(rep: Rep): VariableArrayRep = {
      new VariableArrayRep(typeName, rep, maxSize)
    }
  }

  /**
   * Represents a component of a struct.
   * 
   * @param name The component's name.
   * 
   * @param typeRep The type of the component.
   * 
   * @param rawTypeName The string that appeared in the MXDR as the type name. May be a
   * fundamental XDR type, or an identifier representing a user defined type.
   * 
   * @param value Should this be here?
   */
  class StructComponent(val name: String,
                        val typeRep: Rep,
                        val rawTypeName: Option[String],
                        val value: Option[String]) {

    assert(!rawTypeName.getOrElse("").contains("("))

    // Ada doesn't allow double underscores, it's easier to just not allow it.
    assert(name.headOption.getOrElse(" ") != "_", "Type names may not begin with an underscore.")
    assert(!name.contains("__"))
  }

  /**
   * A value in an enum.
   * 
   * @param name The string identifier for the enum value.
   * @param value Optional value parameter for enum values. May be a symbol or just a number.
   */
  class EnumValue(val name: String, val value: Option[String])

  /**
   * An enum type.
   * 
   * @param typeName The type name of the enum. It is optional.
   * 
   * @param values The values the enum can take on. All value names are unique in the enum. They
   * either all have values or all don't. If they have values, all the values are unique. Has at
   * least one value.
   */
  case class EnumRep(override val typeName: Option[String],
                     values: List[EnumValue]) extends Rep(None) with StructuredRep {
    repOk()

    private def repOk(): Unit = {
      val names = values.map(_.name).toSet
      assert(names.size == values.size)

      val hasExplicitValues = values.head.value.nonEmpty
      if (hasExplicitValues) {
        assert(values.forall(_.value.nonEmpty))
        val valueValues = values.map(_.value.get).toSet
        assert(valueValues.size == values.length)
      } else {
        assert(values.forall(_.value.isEmpty))
      }
    }
  }

  /**
   * A struct type.
   * 
   * @param typeName The name of the type, always defined.
   * 
   * @param components The components of the struct. All component names are unique. If one of
   * the components is type VOID, it is the only component.
   */
  case class StructRep (override val typeName: Option[String],
                        components: List[StructComponent]) extends Rep(None) with StructuredRep {
    /**
     * Gets the component for the given name. Assumes a component with the given name exists.
     */
    def getComponent(name: String): StructComponent = components.find(_.name == name).get

    /**
     * True if the message has one component and that component has type void.
     */
    val isVoid: Boolean = components.size == 1 && components.head.typeRep == VoidRep

    /**
     * Creates a new StructRep but with the indicated component's type replaced by the given
     * type. If the indicated component isn't found, throws. If multiple components match the
     * indicator, replace all of them.
     * 
     * @param currentComponentTypeName The type name of the component to swap.
     * @param newComponentType The type rep to replace with.
     */
    def butWith(currentComponentTypeName: String, newComponentType: Rep): StructRep = {
      new StructRep(typeName, components.map(c => {
        if (c.typeRep.typeName.getOrElse("") == currentComponentTypeName)
          new StructComponent(c.name, newComponentType, c.rawTypeName, c.value)
        else c
      }))
    }

    assert(!components.contains(null))
    assert(components.map(_.name).toSet.size == components.length, "struct must not have duplicate component names")
    assert(
      if (components.exists(rep => {
        rep.typeRep == VoidRep
      }))
        components.length == 1
      else true,
    "if a struct has the Void component, it may have no other components")
  }

  /**
   * A message struct type.
   * 
   * @param typeName The name of the type, always defined.
   * 
   * @param components The components of the struct. All component names are unique. If one of
   * the components is type VOID, it is the only component.
   * 
   * @param direction Whether the message is sent or received by the given module.
   * 
   * @param invariants List of user specified invariants for the message. Strings take the form
   * of expressions.
   *
   * Note that MStructRep is *not* a case class. This is because case-to-case inheritance is
   * illegal, and StructRep is already a case class.
   */
  class MStructRep(override val typeName: Option[String],
                   override val components: List[StructComponent],
                   val direction: MessageDirection.Value,
                   val invariants: List[String]) extends StructRep(typeName, components) {

    override def butWith(currentComponentTypeName: String, newComponentType: Rep): MStructRep = {
      new MStructRep(typeName, components.map(c => {
        if (c.typeRep.typeName.getOrElse("") == currentComponentTypeName)
          new StructComponent(c.name, newComponentType, c.rawTypeName, c.value)
        else c
      }), direction, invariants)
    }
  }

  @unused
  case class UnionRep(override val typeName: Option[String],
                      @unused val name: String,
                      @unused val components: StructComponent,
                      override val baseTypeName: Option[String] = None) extends Rep(baseTypeName) with StructuredRep

  /**
   * A constraint that can be placed on numeric types.
   * 0 .. 10
   * Min_Temp .. Max_Temp
   * etc
   * @param lowerBound Constant representing the lower bound of the range.
   * @param upperBound Constant representing the upper bound of the range.
   *                   May be less than the upper bound.
   */
  case class RangeConstraint(lowerBound: ConstRep[NumericRep],
                             upperBound: ConstRep[NumericRep])

}

