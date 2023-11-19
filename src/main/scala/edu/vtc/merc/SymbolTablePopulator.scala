package edu.vtc.merc

import edu.vtc.merc.MXDRParser._
import edu.vtc.merc.MXDRReadingHelpers.{resolveConstant, resolveValue}
import edu.vtc.merc.TypeRep.{
  ArrayRep, BoolRep, ConstRep, ContinuousRep, DoubleRep, EnumRep, EnumValue, FixedArrayRep, FixedOpaqueRep,
  FloatRep, HyperRep, IntRep, IntegralRep, MStructRep, NumericRep, OpaqueRep, QuadRep, RangeConstraint, Rep,
  StringRep, StructComponent, StructRep, TimeRep, TimeSpanRep, UHyperRep, UIntRep, VariableArrayRep,
  VariableOpaqueRep, VoidRep, anonymousConstant}
import org.antlr.v4.runtime.Token

import scala.collection.mutable.ListBuffer

/**
 * Reads out symbols from the MXDR file, populating the given symbol table.
 * Also reports some semantic errors.
 */
class SymbolTablePopulator(
  nameOfFile : String,
  /**
   * The table to populate
   */
  symbolTable: BasicSymbolTable,
  reporter   : Reporter) extends MXDRBaseVisitor[Unit] {

  private val reservedWords = List(
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
    "xor")

  /**
   * Checks if the given string is a reserved word in SPARK/Ada. Since Ada is case insensitive,
   * it is necessary to do this check without regard to case.
   *
   * @param word The word to check.
   * @return True if and only if the word is reserved.
   */
  private def isReservedWord(word: String): Boolean = {
    reservedWords.contains(word.toLowerCase)
  }

  /**
   * If the given word is a reserved word, reports it to
   * the reporter.
   * @param token The word to check.
   */
  private def reportIfReserved(token: Token): Unit = {
    if (isReservedWord(token.getText)) {
      reporter.reportError(
        token.getLine,
        token.getCharPositionInLine + 1,
        token.getText + " is a reserved SPARK Ada word."
      )
    }
  }

  /**
   * Reads a struct_body and creates a rep obj to
   * represent its components.
   * @return The struct's components
   */
  def readStructBody(ctx: Struct_bodyContext): List[StructComponent] = {
    var components = List[StructComponent]()
    val numComponents = ctx.declaration().size()
    for (i <- 0 until numComponents) {
      val decl = ctx.declaration(i)
      val componentType = readDeclarationType(decl, None, None, isTypeDef = false)
      if (componentType == VoidRep) {
        components = components :+ (new StructComponent("Void", VoidRep, None, None))
      } else {
        val identifierToken = decl.IDENTIFIER().getSymbol
        reportIfReserved(identifierToken)
        val name = decl.IDENTIFIER().getText

        val defaultValue: Option[String] = if (decl.children.contains(ctx.declaration(i).CONSTANT())) {
          Some(decl.CONSTANT().getText)
        }
        else {
          None
        }


        val typeName = decl.start.getText

        components = components.:+(new StructComponent(name, componentType, Some(typeName), defaultValue))

      }
    }

    components
  }

  /**
   * Reads a declaration and creates a rep object to
   * represent the type. If the declaration is for an
   * array, it will return the array type.
   * "string name<10>" -> VariableArrayRep(componentType=string, maxLength=10)
   * "bool isOn = true" -> BooleanRep()
   *
   * @param ctx The context for the declaration.
   * @param range Optionally supplied range constraint to add to
   *              the type. It will only be applied if the type of
   *              the declaration is of an appropriate type. For
   *              example, if the type is Opaque, a given range will
   *              be ignored because Opaque's aren't numeric.
   * @param subtypeName Optional name of the declaration's subtype if
   *                    it has one. Only included in the result type
   *                    if it makes sense. For example, Opaque rep doesn't
   *                    support subtypes.
   * @param isTypeDef If true, the declaration takes place as part of a
   *                     type definition, the type therefore must specify a
   *                     type name, and the IDENTIFIER will be recorded.
   *                     If false, the declaration isn't a type definition,
   *                     and no type name will be recorded.
   * @return A type representation.
   */
  private def readDeclarationType(ctx: DeclarationContext, range: Option[RangeConstraint], subtypeName: Option[String], isTypeDef: Boolean): Rep = {
    if (ctx.VOID != null) return VoidRep
    // All other declaration types have an IDENTIFIER

    if (ctx.IDENTIFIER() == null) {
      throw new Error(s"Missing identfier in declaration ${ctx.getText}")
    }

    val name: String = ctx.IDENTIFIER().getText

    // The name of the type being created
    val typeName: Option[String] = if (isTypeDef) {
      Some(name)
    } else None

    // If the declaration has <> or []
    val hasDiscriminant = ctx.RANGLE != null || ctx.RBRACKET != null

    // If there is a discriminant, the IDENTIFIER names the outer structure
    // typedef innerTypeName typeName<>
    // typedef ____ typeName/innerTypename
    val innerTypeName: Option[String] = if (hasDiscriminant)
      if (ctx.type_specifier != null && ctx.type_specifier.IDENTIFIER != null)
        Some(ctx.type_specifier.IDENTIFIER.getText)
      else None
    else typeName

    // Determine the type. If the type is incompatible with an array
    // type, return from the function with it.
    // For example, opaque cannot be an array
    //    opaque thing<>
    // The <> belongs to the opaque, not an array.
    val typeRep: Rep = ctx.start.getText match {
      case "Ada.Real_Time.Time" => new TimeRep(innerTypeName, subtypeName)
      case "Ada.Real_Time.Time_Span" => new TimeSpanRep(innerTypeName, subtypeName)
      case "opaque" =>
        val value: Option[ConstRep[IntegralRep]] = if (ctx.value() != null) {
          if (ctx.value.IDENTIFIER != null) {
            Some(resolveConstant[IntegralRep](ctx.value.IDENTIFIER.getText, symbolTable))
          } else {
            Some(anonymousConstant[IntegralRep](ctx.value().getText))
          }
        } else None

        if (ctx.RANGLE() == null) {
          return new FixedOpaqueRep(typeName, value.getOrElse(anonymousConstant(ctx.value().getText)))
        }

        if (ctx.value() != null) {
          return new VariableOpaqueRep(typeName, value.get)
        } else {
          return new VariableOpaqueRep(typeName)
        }

      case "string" => {
        val hasMaxSize = ctx.value() != null
        if (hasMaxSize) {
          return new StringRep(typeName, resolveValue(ctx.value().getText, symbolTable).longValue(), baseTypeName = subtypeName)
        } else {
          return new StringRep(typeName, baseTypeName = subtypeName)
        }
      }
      case "int" => new IntRep(innerTypeName, range, baseTypeName = subtypeName)
      case "unsigned" =>
        if (ctx.type_specifier().stop.getText == "int") {
          new UIntRep(innerTypeName, range, baseTypeName = subtypeName)
        }
        else {
          new UHyperRep(innerTypeName, range, baseTypeName = subtypeName)
        }
      case "hyper" => new HyperRep(innerTypeName, range, baseTypeName = subtypeName)
      case "float" => new FloatRep(innerTypeName, range, baseTypeName = subtypeName)
      case "double" => new DoubleRep(innerTypeName, range, baseTypeName = subtypeName)
      case "quadruple" => new QuadRep(innerTypeName, range, baseTypeName = subtypeName)
      case "bool" => new BoolRep(innerTypeName, baseTypeName = subtypeName)
      case "enum" =>
        val p = readEnumBody(ctx.type_specifier().enum_type_spec().enum_body())
        new EnumRep(innerTypeName, p)
      case "struct" =>
        val components = readStructBody(ctx.type_specifier().struct_type_spec().struct_body())
        new StructRep(innerTypeName, components)
      case rootTypeName: String =>
        // If it matched no known symbol, it is an IDENTIFIER for a previously defined type.
        // MXDR: "typedef rootTypeName typeName;"
        if (!symbolTable.hasType(rootTypeName)) throw new Error(s"Unknown type ${rootTypeName}")
        val rootType = symbolTable.getType[Rep](rootTypeName)

        if (!isTypeDef) {
          rootType
        } else //   <------------------     HEY!!      ----------
          // Note that I'm resisting the temptation to make this a recursive function because
          // this process only ever goes one deep.
          // TODO: Maybe cleaner to add a factory class for numeric types?
          rootType match {
            case _: IntRep => new IntRep(innerTypeName, range, baseTypeName = subtypeName)
            case _: UIntRep => new UIntRep(innerTypeName, range, baseTypeName = subtypeName)
            case _: HyperRep => new HyperRep(innerTypeName, range, baseTypeName = subtypeName)
            case _: UHyperRep => new UHyperRep(innerTypeName, range, baseTypeName = subtypeName)
            case _: FloatRep => new FloatRep(innerTypeName, range, baseTypeName = subtypeName)
            case _: DoubleRep => new DoubleRep(innerTypeName, range, baseTypeName = subtypeName)
            case _: BoolRep => new BoolRep(innerTypeName, baseTypeName = subtypeName)
            case o: FixedOpaqueRep => new FixedOpaqueRep(innerTypeName, o.bytes)
            case o: VariableOpaqueRep => new VariableOpaqueRep(innerTypeName, o.maxBytes)
            case a: FixedArrayRep => new FixedArrayRep(innerTypeName, a.size, a.elementType)
            case a: VariableArrayRep => new VariableArrayRep(innerTypeName, a.elementType, a.maxSize)
            case _: QuadRep => new QuadRep(innerTypeName, range, baseTypeName = subtypeName)
            case _: StringRep => throw new Error("Not sure what should happen here")
            case _: TimeRep => new TimeRep(innerTypeName, baseTypeName = subtypeName)
            case _: TimeSpanRep => new TimeSpanRep(innerTypeName, baseTypeName = subtypeName)
            case _: MStructRep => throw new Error("Merc doesn't support subtyping message types.")
            case s: StructRep => new StructRep(innerTypeName, s.components)
            case e: EnumRep => new EnumRep(innerTypeName, e.values)
            case _ => throw new Error(s"Unimplemented type as base type in declaration ${ctx.getText}")
          }
    }

    // Is the declaration for an array?
    if (ctx.children.contains(ctx.RANGLE())) {
      if (ctx.value() == null) {
        new VariableArrayRep(typeName, typeRep)
      } else {
        new VariableArrayRep(typeName, typeRep, anonymousConstant[IntegralRep](resolveValue(ctx.value().getText, symbolTable).toString))
      }
    } else if (ctx.children.contains(ctx.RBRACKET())) {
      new FixedArrayRep(typeName, anonymousConstant[IntegralRep](resolveValue(ctx.value().getText, symbolTable).toString), typeRep)
    } else typeRep
  }

  /**
   * Reads an enum_body and creates a rep obj to
   * represent its components.
   * @param ctx
   * @return
   */
  def readEnumBody(ctx: Enum_bodyContext): List[EnumValue] = {
    var result = List[EnumValue]()

    val i = ctx.IDENTIFIER().size
    if (ctx.children.contains(ctx.EQUALS())) {
      for (x <- 0 until i) {
        val token = ctx.IDENTIFIER(x).getSymbol
        reportIfReserved(token)

        val name = ctx.IDENTIFIER(x).getText
        val value = ctx.value(x).CONSTANT().getText
        result = result.:+(new EnumValue(name, Some(value)))
      }
    }
    else {
      for (x <- 0 until i) {
        val token = ctx.IDENTIFIER(x).getSymbol
        reportIfReserved(token)

        val name = ctx.IDENTIFIER(x).getText
        result = result.:+(new EnumValue(name, None))
      }
    }
    result
  }

  override def visitSpecification(ctx: SpecificationContext): Unit = {
    visitChildren(ctx)
  }

  override def visitDeclaration(ctx: DeclarationContext): Unit = {
    visitChildren(ctx)
  }

  /**
   * Reads a range constraint producing a range constraint
   * object.
   * @param ctx
   * @param typeRep The type to assign to the endpoints of the range.
   * @return
   */
  private def readRangeConstraint(ctx: Range_constraintContext, typeRep: NumericRep): RangeConstraint = {

    val lowerBound: ConstRep[NumericRep] = if (ctx.constLowerBound != null) {
      new ConstRep[NumericRep](null, ctx.constLowerBound.getText, false, typeRep.typeName, Some(typeRep))
    } else { //ctx.varLowerBound != null
      new ConstRep[NumericRep](null, ctx.varLowerBound.getText, true, typeRep.typeName, Some(typeRep))
    }

    val upperBound: ConstRep[NumericRep] = if (ctx.constUpperBound != null) {
      new ConstRep[NumericRep](null, ctx.constUpperBound.getText, false, typeRep.typeName, Some(typeRep))
    } else { //ctx.varUpperBound != null
      new ConstRep[NumericRep](null, ctx.varUpperBound.getText, true, typeRep.typeName, Some(typeRep))
    }

    if (resolveValue(upperBound.value, symbolTable).doubleValue() < resolveValue(lowerBound.value, symbolTable).doubleValue()) {
      reporter.reportError(
        ctx.DOTDOT.getSymbol.getLine,
        ctx.DOTDOT.getSymbol.getCharPositionInLine + 1,
        "Invalid range constraint: Require lower bound <= upper bound")
    }

    // TODO: Check that the provided endpoints are of an acceptable type
    // This includes 1 vs 1.0 and typed constants and sign on unsigned types.

    new RangeConstraint(lowerBound, upperBound)
  }

  /**
   * Adds the type to the symbol table
   * @param ctx
   */
  override def visitBasic_TypeDef(ctx: Basic_TypeDefContext): Unit = {
    // We need the type of the declaration to infer the type of the range end points
    val initialTypeGuess = readDeclarationType(ctx.declaration, None, None, isTypeDef=true)

    if (initialTypeGuess == VoidRep) {
      return
    }

    val token = ctx.declaration().IDENTIFIER().getSymbol
    reportIfReserved(token)

    val name = ctx.declaration().IDENTIFIER().getText

    // If the type has a value, it will not also have a range or be an array
//    if (ctx.declaration().children.contains(ctx.declaration().CONSTANT())) {
//      val constValue = ctx.declaration().CONSTANT().getText
//      val typeRep = readDeclarationType(ctx.declaration(), None)
//      symbolTable.addType(name, typeRep, constValue)
//      return
//    }

    val subtype: Option[String] = if (ctx.subtype_spec != null) {
      Some(ctx.subtype_spec.typeName.getText)
    } else None

    val rangeConstraint: Option[RangeConstraint] = if (ctx.range_constraint() != null){
      Some(readRangeConstraint(ctx.range_constraint(), initialTypeGuess.asInstanceOf[NumericRep]))
    } else None


    val typeRep = readDeclarationType(ctx.declaration(), rangeConstraint, subtype, isTypeDef=true)
    symbolTable.addType(name, typeRep)
  }

  /**
   * Adds the type to the symbol table
   * @param ctx
   * @return
   */
  override def visitEnum_TypeDef(ctx: Enum_TypeDefContext): Unit = {
    val token = ctx.IDENTIFIER().getSymbol
    reportIfReserved(token)

    val name = ctx.IDENTIFIER().getText
    val components = readEnumBody(ctx.enum_body())
    symbolTable.addType(name, new EnumRep(Some(name), components))
  }

  /**
   * Adds the type to the symbol table
   * @param ctx
   * @return
   */
  override def visitStruct_TypeDef(ctx: Struct_TypeDefContext): Unit = {
    val token = ctx.IDENTIFIER().getSymbol
    reportIfReserved(token)

    val name = ctx.IDENTIFIER().getText
    val components = readStructBody(ctx.struct_body())
    val parsedType = new StructRep(Some(name), components)
    symbolTable.addType(name, parsedType)
  }

  /**
   * Adds the type to the symbol table
   *
   * @param ctx
   * @return
   */
  override def visitUnion_TypeDef(ctx: Union_TypeDefContext): Unit = {
    val token = ctx.IDENTIFIER().getSymbol
    reportIfReserved(token)

    val name = ctx.IDENTIFIER().getText
    //TODO
  }

  /**
   * Adds the type to the symbol table
   *
   * @param ctx
   * @return
   */
  override def visitMessage_TypeDef(ctx: Message_TypeDefContext): Unit = {
    val token = ctx.IDENTIFIER().getSymbol
    reportIfReserved(token)

    val name = ctx.IDENTIFIER().getText

    val components = readStructBody(ctx.struct_body())

    val direction = if (ctx.children.contains(ctx.LARROW))
      MessageDirection.Out
    else MessageDirection.In

    val invariants: List[String] = if (ctx.condition != null)
      readMessageInvariants(ctx.condition)
    else (new ListBuffer[String]()).toList

    symbolTable.addType(name, new MStructRep(Some(name), components, direction, invariants))
  }

  /**
   * Reads the given type definition returning a list of type invariants
   * specified by the user.
   *
   * @param ctx
   * @return List of expression strings.
   */
  private def readMessageInvariants(ctx: ConditionContext): List[String] = {
    var result = List[String]()

    for (i <- 0 until ctx.expression.size()) {
      if (ctx.expression(i).children.contains(ctx.expression(i).GOE)) {
        result = (ctx.expression(i).IDENTIFIER(0).getText + " " +
          ctx.expression(i).GOE.getText + " " +
          ctx.expression(i).IDENTIFIER(1).getText) :: result
      }
      else if (ctx.expression(i).children.contains(ctx.expression(i).LOE)) {
        result = (ctx.expression(i).IDENTIFIER(0).getText + " " +
          ctx.expression(i).LOE.getText + " " +
          ctx.expression(i).IDENTIFIER(1).getText) :: result
      }
      else if (ctx.expression(i).children.contains(ctx.expression(i).RANGLE)) {
        result = (ctx.expression(i).IDENTIFIER(0).getText + " " +
          ctx.expression(i).RANGLE.getText + " " +
          ctx.expression(i).IDENTIFIER(1).getText) :: result
      }
      else if (ctx.expression(i).children.contains(ctx.expression(i).LANGLE)) {
        result = (ctx.expression(i).IDENTIFIER(0).getText + " " +
          ctx.expression(i).LANGLE.getText + " " +
          ctx.expression(i).IDENTIFIER(1).getText) :: result
      }
      else if (ctx.expression(i).children.contains(ctx.expression(i).EQUALS)) {
        result = (ctx.expression(i).IDENTIFIER(0).getText + " " +
          ctx.expression(i).EQUALS.getText + " " +
          ctx.expression(i).IDENTIFIER(1).getText) :: result
      }
    }


    result
  }

  /**
   * Reads the line's declaration, storing the result in
   * the symbol table.
   */
  override def visitLine(ctx: LineContext): Unit = {
    val decl = ctx.declaration()

    val name: String = if (decl.IDENTIFIER != null) {
      decl.IDENTIFIER().getText
    }
    else {
      decl.VOID().getText
    }
    val token = decl.IDENTIFIER().getSymbol
    reportIfReserved(token)

    val value: String = if (decl.CONSTANT != null) {
      decl.CONSTANT().getText
    }
    else {
      "null"
    }

    val typeRep = readDeclarationType(decl, None, None, isTypeDef=true)

    symbolTable.addType(name, typeRep)
  }

  /**
   * Reads the constant, adding it to the symbol table.
   */
  override def visitConstant_def(ctx: Constant_defContext): Unit = {
    reportIfReserved(ctx.name)

    val name = ctx.name.getText

    val value = ctx.CONSTANT().getText

    val typeName: Option[String] = if (ctx.typeName != null) {
      Some(ctx.typeName.getText)
    } else None

    val typeRep: Option[Rep] = if (ctx.typeName != null) {
      Some(symbolTable.getType[NumericRep](typeName.get))
    } else {
      None
    }

    symbolTable.addConstant(name, new ConstRep(Some(name), value, false, typeName, typeRep))
  }


  /*def unionP(ctx: MercParser.Struct_bodyContext): List[(String, Rep, String)] = {
    var p2: List[(String, Rep, String)] = null
    val i = ctx.declaration().size()
    for (x <- 0 to i - 1) {
      val name = ctx.declaration(x).IDENTIFIER().getText
      val c = if (ctx.declaration(x).children.contains(ctx.declaration(x).CONSTANT())) {
        ctx.declaration(x).CONSTANT().getText
      }
      else {
        "null"
      }
      ctx.declaration(x).getChild(0).getText match {
        case "opaque" =>
          val t = OpaqueRep
          ((name, t, c)) :: p2
        case "string" =>
          val t = StringRep
          ((name, t, c)) :: p2
        case "void" =>
          val name = "void"
          val t = VoidRep
          (name, t, "null")
        case name =>
          val t = IDRep
          ((name, t, c)) :: p2
        case _ => ctx.declaration(x).type_specifier().getText match {
          case "int" =>
            val t = IntRep
            ((name, t, c)) :: p2
          case "unsignedint" =>
            val t = UIntRep
            ((name, t, c)) :: p2
          case "hyper" =>
            val t = HyperRep
            ((name, t, c)) :: p2
          case "unsignedhyper" =>
            val t = UHyperRep
            ((name, t, c)) :: p2
          case "float" =>
            val t = FloatRep
            ((name, t, c)) :: p2
          case "double" =>
            val t = DoubleRep
            ((name, t, c)) :: p2
          case "quadruple" =>
            val t = QuadRep
            ((name, t, c)) :: p2
          case "bool" =>
            val t = BoolRep
            ((name, t, c)) :: p2
          case "string" =>
            val t = StringRep
            ((name, t, c)) :: p2
          case name =>
            val t = IDRep
            ((name, t, c)) :: p2
          case _ => ctx.declaration(x).type_specifier().getChild(0).getText match {
            case "enum" =>
              val t = EnumRep
              val p = enumP(ctx.declaration(x).type_specifier().enum_type_spec()).asInstanceOf[ComponentRep]
              symbolTable.addObjectName(name, t.apply(name, p), "null")
              (name, t.apply(name, p), "null")
            case "struct" =>
              val t = StructRep
              val p = structP(ctx.declaration(x).type_specifier().struct_type_spec().struct_body()).asInstanceOf[ComponentRep]
              symbolTable.addObjectName(name, t.apply(name, p), "null")
              (name, t.apply(name, p), "null")
            case "union" =>
              val t = UnionRep
              val p = unionP(ctx.declaration().type_specifier(x).union_type_spec().union_body()).asInstanceOf[ComponentRep]
              symbolTable.addObjectName(name, t.apply(name, p), "null")
              (name, t.apply(name, p), "null")
          }
        }
      }
    }
    p2
  }*/

}
