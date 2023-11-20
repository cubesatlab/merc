package edu.vermontstate.merc

import edu.vermontstate.merc.TypeRep.{ConstRep, NumericRep, Rep}
import TypeRep.{ConstRep, NumericRep, Rep, UIntRep, anonymousConstant}

/**
 * Contains several helper functions for parsing the MXDR syntax
 * tree into Merc's internal representation.
 */
object MXDRReadingHelpers {
  /**
   * Recursively searches the given symbol table, checking for numeric literals, producing
   * a constant result.
   *
   * @param symbol      Either a numeric literal, or a type name with an optional "'" suffix.
   * @param symbolTable The table to search for the symbol.
   * @tparam T The expected type of the constant.
   * @return A constant of the expected type. If an unsupported suffix, or more than one
   *         suffix is present, throws an error.
   */
  def resolveConstant[T <: Rep](symbol: String, symbolTable: BasicSymbolTable): ConstRep[T] = {
    val hasSuffix = symbol.contains('\'')

    val root = if (hasSuffix) {
      val index = symbol.indexOf('\'')
      symbol.substring(0, index)
    } else symbol

    if (!hasSuffix) {
      if ("(-?[0-9]+)?(\\.[0-9]+)?".r.matches(root)) {
        anonymousConstant[NumericRep](root)
      }
      return symbolTable.getConstant(root)
    }

    val suffix = symbol.substring(root.length, symbol.length)

    // If there is a suffix, then the symbol must represent a ranged type
    val typeRep = symbolTable.getType[NumericRep](root)
    suffix match {
      case "'Last" => typeRep.range.upperBound.asInstanceOf[ConstRep[T]]
      case "'First" => typeRep.range.lowerBound.asInstanceOf[ConstRep[T]]
      case _ => throw new Error(s"Unimplimented suffix on range: $suffix")
    }
  }

  /**
   * Takes an identifier and resolves it to a value. Assumes
   * the symbol has a known numeric value. Can handle 'First
   * 'Last suffixes, assuming there is at most one suffix.
   *
   * @param symbol An IDENTIFIER, possibly including ticks.
   * @return The numeric value of the symbol.
   */
  def resolveValue(symbol: String, symbolTable: BasicSymbolTable): Number = {
    val hasSuffix = symbol.contains('\'')

    val root = if (hasSuffix) {
      val index = symbol.indexOf('\'')
      symbol.substring(0, index)
    } else symbol

    if (!hasSuffix) {
      if ("-?([0-9]+)?(\\.[0-9]+)?".r.matches(root)) {
        if (root.contains('.'))
          return root.toDouble
        return root.toLong
      }
      val value = symbolTable.getConstant(root).value
      return resolveValue(value, symbolTable)
    }

    val suffix = symbol.substring(root.length, symbol.length)

    // If there is a suffix, then the symbol must represent a ranged type
    val typeRep = if (root.toLowerCase == "natural") {
      new UIntRep(None)
    } else symbolTable.getType[NumericRep](root)

    val valueString = suffix match {
      case "'Last" => typeRep.range.upperBound.value
      case "'First" => typeRep.range.lowerBound.value
      case _ => throw new Error(s"Unimplimented suffix on range: $suffix")
    }

    resolveValue(valueString, symbolTable)
  }
}
