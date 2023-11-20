package edu.vermontstate.merc

import scala.collection.mutable
import MXDRParser.*

/**
 * Reads mxdr file, collecting info about dependencies.
 */
class DependencyReader extends MXDRBaseVisitor[Void] {
  /**
   * All the message types this module may receive.
   * Doesn't include "_Msg", only the name of the message type.
   * "Tick_Reply", "Ping_Request".
   */
  val receiveMessages: mutable.Set[String] = mutable.Set[String]()
  /**
   * The other API packages that this MXDR references.
   * These are valid Ada packages.
   * "CubedOS.Time_Server"
   * Doesn't include ".API"
   */
  val dependsOn: mutable.Set[String] = mutable.Set[String]()

  override def visitReceives(ctx: ReceivesContext): Void = {
    // Read the message dependencies into the list
    val count = ctx.IDENTIFIER().size
    for (i <- 0 until count) {
      receiveMessages.add(ctx.IDENTIFIER(i).getSymbol.getText)
    }

    visitChildren(ctx)

    null
  }

  override def visitPackage_name(ctx: Package_nameContext): Void = {
    var result = ctx.IDENTIFIER(0).getSymbol.getText
    val count = ctx.IDENTIFIER().size
    for (i <- 1 until count) {
      result += "." + ctx.IDENTIFIER(i).getSymbol.getText
    }
    dependsOn.add(result)

    visitChildren(ctx)

    null
  }

  /**
   * If the type is a message and it will be received,
   * add it to the receive messages set.
   * @param ctx
   * @return
   */
  override def visitMessage_TypeDef(ctx: MXDRParser.Message_TypeDefContext): Void = {
    val messageName = ctx.IDENTIFIER.getText

    val direction = if (ctx.children.contains(ctx.LARROW)) {
      MessageDirection.Out
    } else MessageDirection.In

    if (direction == MessageDirection.In) { // This message type is received
      receiveMessages.add(messageName)
    }

    visitChildren(ctx)
    null
  }



}
