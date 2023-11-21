package edu.vermontstate.merc

/**
 * The SemanticAnalyzer walks the parse tree and performs semantic analysis on it. Currently this
 * class is a stub. Some semantic checks are handled by the SymbolTablePopulator. Eventually,
 * this class will perform semantic checks that are more convenient once the symbol table is
 * fully populated; for example, type checking (to the extent it is required by the MXDR
 * language).
 *
 * It is possible that the SymbolTablePopulator should be merged into this class. That would
 * reduce the number of passes over the parse tree, but it would also make the code more
 * complex, particularly as the number of semantic checks increases.
 *
 * @param symbolTable The symbol table populated by the SymbolTablePopulator.
 * @param reporter The reporter used to report semantic errors and warnings.
 */
class SemanticAnalyzer(
  symbolTable: BasicSymbolTable,
  reporter   : Reporter) extends MXDRBaseVisitor[Unit] {

  // TODO: Add type checking and other semantic checks.

}
