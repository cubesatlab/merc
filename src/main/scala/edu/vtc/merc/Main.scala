package edu.vtc.merc

import edu.vtc.merc.AdaGeneratorCommon.processTreeForAda
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

import java.io.PrintStream


/**
 * The main module of the program. It parses the command line, reads the input XDR file, and
 * coordinates the work of the rest of the application.
 */
object Main {

  /**
    * Analyze the command line and extract the switches as well as the name of the source
    * file(s) to be compiled. Each switch has a '-' as a prefix and is represented as a single
    * letter. If the switch has an argument, the text of that argument immediately follows the
    * switch itself (no space) and extends to the next non-quoted space on the command line.
    *
    * @param args The command line as passed to the main method.
    * @return None if there is an error in the command line; otherwise return a set of switches
    *         together with the source file name(s).
    */
  private[merc] def analyzeCommandLine(args: Array[String]): Option[(Map[Char, String], String)] = {
    if (args.length < 1) {
      None
    }
    else {
      val (switchArgs, nonSwitchArgs) = args partition { _.charAt(0) == '-' }

      // Extract the switches.
      // Invalid switches are transformed into the special '-' switch, which isn't used.
      val switchMap: Map[Char, String] = (for (switch: String <- switchArgs.toSet) yield {
        // TODO: Deal with the 'empty' switch consisting of just a single '-'
        val rawSwitch   = switch.charAt(1)
        val rawArgument = if (switch.length == 2) "" else switch.substring(2)
        // TODO: Issue an error message for invalid switches.
        rawSwitch -> rawArgument
      }).toMap

      // For now we support only a single source file name. If no names or multiple names are
      // found, set the name to a blank string, which isn't used.
      // TODO: Add support for multiple source files in a single run?
      val sourceName = if (nonSwitchArgs.length == 1) nonSwitchArgs(0) else ""

      // Decide what to return.
      // TODO: Return None if an invalid switch is found.
      if (sourceName == "") None else Some((switchMap, sourceName))
    }
  }


  def main(args: Array[String]): Unit = {
    // Analyze the command line.
    val commandOption = analyzeCommandLine(args)
    if (commandOption.isEmpty) {
      println("Usage: java -jar Merc.jar [-k] [-tTemplateFolder] mxdr-source-file\n" +
              "   -k : Only check syntax and semantics of MXDR file\n" +
              "   -t : Specifies the folder where package templates are located\n" +
              "   -o : Specifies the output directory for generated api files. Defaults to the current directory\n")
      System.exit(1)
    }
    val Some((switchMap, sourceName)) = commandOption
    val templateFolder = switchMap.getOrElse('t', ".")

    // Create a stream that reads from the specified file.
    val codePointCharStream = CharStreams.fromFileName(sourceName)

    // Parse the input file as MXDR.
    val lexer  = new MXDRLexer(codePointCharStream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new MXDRParser(tokens)
    val tree   = parser.specification()

    val symbolTable = new BasicSymbolTable
    val reporter    = new BasicConsoleReporter

    // Populate the symbol table
    val myTable = new SymbolTablePopulator(sourceName, symbolTable, reporter)
    myTable.visit(tree)

    // Check the tree for semantic errors.
    val myAnalyzer  = new SemanticAnalyzer(sourceName, symbolTable, reporter)
    myAnalyzer.visit(tree)

    val errorCount = reporter.errorCount

    if (switchMap.contains('k')) {
      println(s"*** $errorCount errors found.")
      return
    }
    if (errorCount > 0) {
      println(s"*** $errorCount errors found. Aborting!")
      return
    }

    // Continue to generate API files

    println(s"$sourceName Successfully parsed and analyzed...")
    println("Generating API package...")

    // TODO: This won't work for paths without dots. Should that be ruled out earlier?
    val outputPath = switchMap.getOrElse('o', sourceName.substring(0, sourceName.lastIndexOf('/') + 1))
    val baseFileName = sourceName.substring(sourceName.lastIndexOf('/') + 1, sourceName.lastIndexOf('.'))
    val baseFileNameLower = baseFileName.toLowerCase

    val specificationFileName = baseFileNameLower + "-api.ads"
    val bodyFileName = baseFileNameLower + "-api.adb"
    val specificationFile = new PrintStream(outputPath + specificationFileName)
    val bodyFile = new PrintStream(outputPath + bodyFileName)

    // Pull the module name and prefix from the file name
    var hasDash = false
    val modulePrefix =
      (if (baseFileName.contains("-")) {
        hasDash = true
          baseFileName.substring(0, baseFileName.lastIndexOf('-'))

      } else "")
        .replace('-', '.')

    val moduleName = baseFileName.substring(modulePrefix.length + (if (hasDash) 1 else 0), baseFileName.length)

    val mxdrTree = new MXDRTree()
    symbolTable.getAll().foreach(pair => {
      val (name, entity) = pair
      mxdrTree.add(entity)
    })

    val (processedTree, processedTable) = processTreeForAda(mxdrTree, symbolTable)

    val dependencyReader = new DependencyReader()
    dependencyReader.visit(tree)

    println("Spec file is " + outputPath + specificationFileName)

    new SpecificationGenerator(
        templateFolder,
        modulePrefix,
        moduleName,
        specificationFileName,
        processedTable,
        processedTree,
        dependencyReader,
        specificationFile,
        reporter).generate()

    new BodyGenerator(
        templateFolder,
        modulePrefix,
        moduleName,
        bodyFileName,
        processedTable,
        processedTree,
        bodyFile,
        reporter).generate()

    specificationFile.close()
    bodyFile.close()

  }

}
