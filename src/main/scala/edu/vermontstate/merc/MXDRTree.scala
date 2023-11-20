package edu.vermontstate.merc

import edu.vermontstate.merc.TypeRep.{ArrayRep, ConstRep, MXDREntity, Rep, StructRep}
import TypeRep.{ArrayRep, ConstRep, MXDREntity, Rep, StructRep}

import scala.annotation.unused
import scala.collection.mutable.ListBuffer

/**
 * This class stores the parsed content of an MXDR file.
 * It should be used to store top level items, like type
 * definitions and constants. Deeper structures should be
 * composed into the rep of added items.
 */
class MXDRTree {

  private val storage = new ListBuffer[MXDREntity]()

  /**
   * Adds the item to the MXDR file.
   * Items are stored in the order they are added.
   * @param rep The item to add.
   * @param before An item already added to put
   *               the item being added before.
   */
  def add(rep: MXDREntity, before: Option[Rep] = None): Unit = {
    rep match {
      case rep1: Rep => assert(!storage.exists(e => e.isInstanceOf[Rep] && e.asInstanceOf[Rep].typeName == rep1.typeName))
      case _ =>
    }
    if (before.nonEmpty) {
      val index = storage.indexOf(before.get)
      storage.insert(index, rep)
      return
    }
    storage.addOne(rep)
  }

  /**
   * Gets a list of all the items in the MXDR file that
   * are covariant to the given type in the order they were
   * added.
   */
  def getItems[T <: MXDREntity](): List[T] = {
    val result = storage.filter(rep => rep.isInstanceOf[T]).map(rep => rep.asInstanceOf[T]).toList
    assert(result.forall(_.isInstanceOf[T]))
    result
  }

  /**
   * Prints the tree to the console.
   */
  @unused
  def printDebugTree(): Unit = {
    def printElement(entity: MXDREntity, depth: Int): Unit = {
      entity match {
        case r: Rep =>
          for (_ <- 0 until depth) print("   ")
          println(s"${r.typeName.getOrElse("ANON")}: ${r.getClass.getName.substring(r.getClass.getName.lastIndexOf('$') + 1, r.getClass.getName.length)}")

          r match {
            case s: StructRep =>
              for (component <- s.components) {
                printElement(component.typeRep, depth + 1)
              }
            case a: ArrayRep =>
              printElement(a.elementType, depth + 1)
            case _ =>
          }
        case c: ConstRep[Rep] =>
          for (_ <- 0 until depth) print("   ")
          println(c.name)
      }
    }

    println(s"Printing Debug Tree of size ${storage.length}")
    for (entity <- storage) {
      printElement(entity, 0)
    }
  }
}
