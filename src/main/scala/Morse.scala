package nyc.freeman

import javax.sound.midi._
import scala.jdk.CollectionConverters._
import scala.concurrent.Future
import scala.util.{Success, Try}
import scala.LazyList.#::
import scala.annotation.tailrec
import java.io.InputStream
import java.nio.file.Path
import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.io.Source

/**
 * Implement the ITU standard with javax sound
 * based on https://en.wikipedia.org/wiki/Morse_code
 *
 */
case class Morse(charToCode: Map[Char, String], dotLength:Int = 250) {
  charToCode foreach ((mpg) => {
    val dotsdashes = mpg._2
    require(dotsdashes.nonEmpty, s"Character ${mpg._1} is mapped to the empty string")
    dotsdashes.foreach(dotdash => require(Set('.', '-').contains(dotdash), s"Character ${mpg._1} mapping $dotsdashes contains illegal character $dotdash"))
  })

  val tone = Tone()
  import tone._
  /**
   * International Morse code is composed of five elements:[1]
   *
   * short mark, dot or "dit" (▄▄▄▄): "dot duration" is one time unit long
   * longer mark, dash or "dah" (▄▄▄▄▄▄): three time units long
   * inter-element gap between the dots and dashes within a character: one dot duration or one unit long
   * short gap (between letters): three time units long
   * medium gap (between words): seven time units long
   */
  def dash = toneForMilliseconds(3 * dotLength)

  def dot = toneForMilliseconds(dotLength)

  def gap = silenceForMilliseconds(3 * dotLength)

  def space = silenceForMilliseconds(7 * dotLength)

  @tailrec
  final def playTones(tones: List[Char]): Unit = tones match {
    case Nil =>
      ()
    case head :: tail => head match {
      case '.' => dot
      case '-' => dash
    }
      playTones(tail)
  }

  def charToMorse(char: Char): Unit = {
    char.toUpper match{
      case ' ' => space
      case upper if charToCode.keySet.contains(upper) => playTones(charToCode(upper).toList)
      case _ => ()
    }
  }

  /**
   * Plays the tones for each character in the string for which it has a mapping
   * @param encodable The string to convert
   */
  def toMorse(encodable: LazyList[Char]): LazyList[Unit] = {
    encodable match {
      case empty if empty.isEmpty => LazyList.empty
      case last #:: LazyList() => LazyList(charToMorse(last))
      case head #:: tail =>
        charToMorse(head) #:: gap #:: toMorse(tail)
    }
  }

  def translateToMorse(encodable: String): List[Unit] = {
    val translation = toMorse(encodable = LazyList.from(encodable)).toList
    Thread.sleep(14 * dotLength)
    translation
  }
}

object Morse {
  def loadMapping(resourceName: String): Map[Char, String] = {
    (for (charMappings: Iterator[String] <- getResource("/morseCode.dat")) yield for {
      charMapping <- charMappings if charMapping.trim.nonEmpty
      entry = (charMapping.head.toUpper, charMapping.tail.span(Set('\t', ' ').contains)._2) if entry._1 != '#'
    } yield entry) match {
      case None => // build enough in for an SOS
        Map(
          ('S', "..."),
          ('O', "---")
        )
      case Some(mapping) =>
        mapping.toMap
    }
  }
}
