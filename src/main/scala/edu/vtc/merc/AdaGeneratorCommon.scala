package edu.vtc.merc

import edu.vtc.merc.TypeRep._

/**
 * Contains utility functions common to both Ada generator classes.
 * Also some MXDR tree processing functions.
 */
object AdaGeneratorCommon {

  /************************************
   * Code Generation Helpers
   ************************************/

  /**
   * Takes a type rep and finds the Ada type name for
   * the class, preferring native Ada constructs like
   * Natural, Positive and custom types over the
   * XDR types.
   * Never includes the "_Ptr" suffix.
   * @param rep The type to name.
   * @return A type name.
   * @throws Error if the type doesn't directly correspond to
   *               an Ada type name. This includes enums, structs
   *               and arrays.
   */
  def adaFriendlyTypeName(rep: Rep): String = rep match {
    case _: BoolRep => "Boolean"
    case _: DoubleRep => "Float"
    case _: FloatRep => "Float"
    case _: HyperRep => "XDR_Hyper"
    case _: IntRep => "Integer"
    case _: StringRep => "String"
    case _: TimeRep => "Ada.Real_Time.Time"
    case _: TimeSpanRep => "Ada.Real_Time.Time_Span"
    case _: UHyperRep => "XDR_Unsigned_Hyper"
    case _: UIntRep => "Natural"
    case _: OpaqueRep => "Octet_Array"
    case _ => throw new Error(s"MXDR type ${rep.toString} doesn't directly correspond to a type in Ada")
  }

  /**
   * Takes the type rep and finds the XDR library
   * type name for it. Doesn't include the "XDR."
   * prefix or the "_Ptr" suffix.
   * @param rep The type to name.
   * @return The name of a type in the XDR library which
   *         can store the type given.
   */
  def xdrTypeName(rep: Rep): String = rep match {
    case _: BoolRep => "Boolean"
    case _: DoubleRep => "XDR_Double"
    case _: FloatRep => "XDR_Float"
    case _: UHyperRep => "XDR_Unsigned_Hyper"
    case _: HyperRep => "XDR_Hyper"
    case _: UIntRep => "XDR_Unsigned"
    case _: IntRep => "XDR_Integer"
    case _: StringRep => "String"
    case _: TimeRep => "XDR_Unsigned_Hyper"
    case _: TimeSpanRep => "XDR_Unsigned_Hyper"
    case _: OpaqueRep => "Octet_Array"
    case _: EnumRep => "XDR_Unsigned"
    case _ => throw new Error(s"MXDR type ${rep.toString} doesn't directly correspond to a type in the Ada XDR library")
  }

  /**
   * Determines if the given data type should be passed
   * by reference explicitly (using a ptr).
   * If the type is pass by reference, it's name
   * should be suffixed with "_Ptr".
   */
  def passedByReference(rep: Rep): Boolean = {
    rep match {
      case _: VariableArrayRep => true
      case _: VariableOpaqueRep => true
      case _: StringRep => true
      case _ => false;
    }
  }

  /************************************
   * Tree Processing
   ************************************/

  /**
   * Processes the given MXDR tree and table yielding new ones
   * that may be processed by the Ada API generators. Doesn't
   * effect the given items.
   */
  def processTreeForAda(inputTree: MXDRTree, inputTable: BasicSymbolTable): (MXDRTree, BasicSymbolTable) = {
    // 1) SPARK/Ada doesn't support anonymous arrays in structs
    // 2) No anonymous structs in arrays
    // 3) No anonymous enums in arrays

    val resultTree = new MXDRTree()
    val resultTable = new BasicSymbolTable()

    /**
     * Recursively searches for anonymous structures and replace them
     * with references to not anonymous ones. Doesn't effect the
     * result.
     */
    def makeSafe[T <: Rep](rep: T): T = {
      rep match {
        case a: ArrayRep =>
          assert(!a.isAnonymous)
          var result: ArrayRep = a

          val (safeArray, _) = pullOutAnonymousStruct(a)
          val (saferArray, _) = pullOutAnonymousEnum(safeArray)
          result = saferArray

          result = result.butWith(makeSafe(result.elementType))

          result.asInstanceOf[T]
        case s: StructRep =>
          assert(!s.isAnonymous)
          var result: StructRep = s

          val (safeStruct, _) = pullOutAnonymousArrays(s)
          result = safeStruct

          for (component <- result.components.filter(c => c.typeRep.isInstanceOf[StructuredRep])) {
            assert(component.typeRep.typeName.nonEmpty, s"Component ${component.name} has no type")
            result = result.butWith(component.typeRep.typeName.get, makeSafe(component.typeRep))
          }

          result.asInstanceOf[T]
        case _: EnumRep =>
          assert(!rep.isAnonymous)
          rep
        case _ =>
          rep
      }
    }

    /**
     * Looks with a depth of 1 for not-allowed anonymous things.
     * If it finds any, creates nominal versions and adds them
     * to the result.
     */
    def pullAndAdd(symbol: String, rep: Rep): Unit = {
      assert(!rep.isAnonymous)

      rep match {
        case x: StructRep =>
          // We don't need to check for anonymous child structs
          // because XDR's syntax doesn't allow it.
          // Similarly there are no child anonymous enums
          val (s, newArrayTypes) = pullOutAnonymousArrays(x)
          newArrayTypes.foreach(t => {
            pullAndAdd(t.typeName.get, t)
          })

          val safeStruct = makeSafe(s)
          assertEntityIsAcceptableForAda(safeStruct, "")
          resultTree.add(safeStruct)
          resultTable.addType(symbol, safeStruct)
        // All components which are themselves structs have already been checked
        case x: ArrayRep =>
          // We don't need to check for anonymous arrays because the syntax
          // of XDR doesn't allow it.
          val (safeArray, newStructType) = pullOutAnonymousStruct(x)
          val (a, newEnumType) = pullOutAnonymousEnum(safeArray)

          if (newStructType.nonEmpty) {
            pullAndAdd(newStructType.get.typeName.get, newStructType.get)
          }

          if (newEnumType.nonEmpty) {
            resultTree.add(newEnumType.get)
            resultTable.addType(newEnumType.get.typeName.get, newEnumType.get)
          }

          val reallySafeArray = makeSafe(a)
          assertEntityIsAcceptableForAda(reallySafeArray, "")
          resultTree.add(reallySafeArray)
          resultTable.addType(symbol, reallySafeArray)
        case _ =>
          assertEntityIsAcceptableForAda(rep, "")
          resultTree.add(rep)
          resultTable.addType(symbol, rep)
      }
    }

    for (pair <- inputTable.getAll()) {
      val (symbol, entity) = pair
      entity match {
        case x: Rep =>
          pullAndAdd(symbol, x)
        case x: ConstRep[Rep] =>
          resultTree.add(x)
          resultTable.addConstant(symbol, x)
      }
    }

    assertMXDRTreeAcceptableForAda(resultTree)
    (resultTree, resultTable)
  }

  /**
   * Proves that the given mxdr tree is suitable for consumption
   * by the Ada generators.
   */
  def assertMXDRTreeAcceptableForAda(mxdrTree: MXDRTree): Unit = {
    mxdrTree.getItems[MXDREntity]().foreach(item => assertEntityIsAcceptableForAda(item, ""))
  }

  /**
   * Produces a struct such that no component is an anonymous array.
   * Components that are an anonymous array will be replaced with a
   * named array, and each created named array will appear in the output.
   * Anonymous arrays are assigned a name with the getArrayName() function.
   * @param structRep A struct type, possibly containing anonymous arrays.
   *                  May be a struct or message struct.
   * @return A struct without anonymous arrays derived from the given struct,
   *         and a list of named array types which must be defined before
   *         the resulting struct.
   */
  private def pullOutAnonymousArrays(structRep: StructRep): (StructRep, List[ArrayRep]) = {
    val anonArrays = structRep.components.filter(c => c.typeRep.isInstanceOf[ArrayRep] && c.typeRep.isAnonymous)
    if (anonArrays.isEmpty) return (structRep, List[ArrayRep]())

    var newArrayTypes = List[ArrayRep]()
    val newComponents = structRep.components.map(c =>
      if (c.typeRep.isInstanceOf[ArrayRep] && c.typeRep.isAnonymous) {
        val newTypeRep = c.typeRep match {
          case rep: VariableArrayRep =>
            new VariableArrayRep(Some(getArrayName(structRep, c)), rep.elementType, rep.maxSize)
          case rep: FixedArrayRep =>
            new FixedArrayRep(Some(getArrayName(structRep, c)), rep.size, rep.elementType)
        }
        newArrayTypes = newArrayTypes :+ newTypeRep
        new StructComponent(c.name, newTypeRep, c.rawTypeName, c.value)
      } else c)

    val resultStructRep: StructRep = structRep match {
      case rep: MStructRep => new MStructRep(rep.typeName, newComponents, rep.direction, rep.invariants)
      case rep: StructRep => new StructRep(rep.typeName, newComponents)
    }

    assert(!resultStructRep.components.exists(c => c.typeRep.isInstanceOf[ArrayRep] && c.typeRep.isAnonymous))

    (resultStructRep, newArrayTypes)
  }

  /**
   * If the given array's element type is an anonymous struct,
   * returns a named struct with the same components and returns
   * an array with a reference to the created named struct.
   * The resulting array will never have an anonymous struct type.
   * If no operation is done, the resulting array === input array.
   * @param rep The given array rep must be named.
   * @return If created the struct type name will be the name of
   *         the array suffixed by "_Struct".
   */
  private def pullOutAnonymousStruct(rep: ArrayRep): (ArrayRep, Option[StructRep]) = {
    if (!rep.elementType.isInstanceOf[StructRep]) return (rep, None)
    if (!rep.elementType.isAnonymous) return (rep, None)

    val newName = Some(rep.typeName.get + "_Struct")

    val resultStruct = rep.elementType match {
      case s: MStructRep => new MStructRep(newName, s.components, s.direction, s.invariants)
      case s: StructRep => new StructRep(newName, s.components)
    }

    assert(!resultStruct.isAnonymous)

    val resultArray = rep match {
      case a: VariableArrayRep =>
        new VariableArrayRep(rep.typeName, resultStruct, a.maxSize)
      case a: FixedArrayRep =>
        new FixedArrayRep(rep.typeName, a.size, resultStruct)
    }

    assert(!resultArray.elementType.isAnonymous)

    (resultArray, Some(resultStruct))
  }

  /**
   * See the other pullOutAnonymous* functions for context.
   */
  private def pullOutAnonymousEnum(input: ArrayRep): (ArrayRep, Option[EnumRep]) = {
    assert(!input.isAnonymous)
    if (!input.elementType.isAnonymous) return (input, None)
    if (!input.elementType.isInstanceOf[EnumRep]) return (input, None)

    val name = input.typeName.get + "_Enum"
    val inputEnum = input.elementType.asInstanceOf[EnumRep]
    val resultEnum = new EnumRep(Some(name), inputEnum.values)

    val resultArray = input match {
      case x: FixedArrayRep =>
        new FixedArrayRep(x.typeName, x.size, resultEnum)
      case x: VariableArrayRep =>
        new VariableArrayRep(x.typeName, resultEnum, x.maxSize)
    }

    (resultArray, Some(resultEnum))
  }

  /**
   * Determines the created type name of the given struct component which is
   * an anonymous array.
   * @param rep The enclosing struct.
   * @param component The anonymous array component which is being named.
   * @return The name given to the anonymous array.
   */
  def getArrayName(rep: StructRep, component: StructComponent): String = rep.typeName.get + "_" + component.name

  /**
   * Recursively asserts that entities are safe for the Ada generators.
   */
  private def assertEntityIsAcceptableForAda(entity: MXDREntity, outerStructure: String): Unit = {
    val entityName = entity match {
      case r: Rep => r.typeName.getOrElse("")
      case _ => ""
    }
    entity match {
      case a: ArrayRep =>
        assert(!a.isAnonymous, s"There is an anonymous array in $outerStructure")
        assertEntityIsAcceptableForAda(a.elementType, entityName)
      case s: StructRep =>
        assert(!s.isAnonymous, s"There is an anonymous struct in $outerStructure")
        s.components.foreach(c => assertEntityIsAcceptableForAda(c.typeRep, entityName))
      case e: EnumRep =>
        assert(!e.isAnonymous, s"There is an anonymous enum in $outerStructure")
      case _ =>
    }
  }


}