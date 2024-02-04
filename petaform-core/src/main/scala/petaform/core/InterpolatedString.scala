package petaform.core

import harness.core.*
import scala.annotation.tailrec

final case class InterpolatedString(
    prefix: String,
    pairs: List[(Interpolation, String)],
) {

  def show: String =
    s"\"${prefix.unesc("")}${pairs.map { case (i, s) => s"${i.show}${s.unesc("")}" }.mkString}\""

}

extension (sc: StringContext) {
  def is(args: Interpolation*): InterpolatedString = {
    @tailrec
    def loop(
        argsQueue: List[Interpolation],
        stringsQueue: List[String],
        partsHead: String,
        rStack: List[(Interpolation, String)],
    ): InterpolatedString =
      (argsQueue, stringsQueue) match {
        case (argsH :: argsT, partsH :: partsT) => loop(argsT, partsT, partsHead, (argsH, partsH) :: rStack)
        case (Nil, Nil)                         => InterpolatedString(partsHead, rStack.reverse)
        case _                                  => throw new RuntimeException("InterpolatedString : this should be impossible... sc.parts/args do not match up")
      }

    sc.parts.toList match {
      case partsH :: partsT => loop(args.toList, partsT, partsH, Nil)
      case Nil              => throw new RuntimeException("InterpolatedString : this should be impossible... missing parts head")
    }
  }
}
