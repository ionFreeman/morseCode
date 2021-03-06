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
object Morse {

  // funny story... we don't need this function
  def getDeviceAndWhetherWasOpen: (MidiDevice, Boolean) = {
    val devices: List[MidiDevice] = MidiSystem.getMidiDeviceInfo.toList.map(MidiSystem.getMidiDevice(_))
    val devicesWithState = LazyList.from(devices)
      .map(d => (d, d.isOpen))
      .map((deviceWithState) => (deviceWithState._1, deviceWithState._2, Try {
        if (!deviceWithState._2) deviceWithState._1.open
      }))
      .filter(_._3.isSuccess)
      .map(a => (a._1, a._2))

    if (devicesWithState.isEmpty) throw new IllegalStateException("No available devices")
    devicesWithState.head
  }

  def sendShortMessage(receiver: Receiver = MidiSystem.getReceiver)(message: Int, channel: Int = 0, tone: Int = 60, keyVelocity: Int = 93) = {
    receiver.send(new ShortMessage(message, channel, tone, keyVelocity), -1)
  }

  def toneForMilliseconds(dure: Int) = {
    sendShortMessage()(ShortMessage.NOTE_ON)
    Thread.sleep(dure)
    sendShortMessage()(ShortMessage.NOTE_OFF)
  }

  def silenceForMilliseconds(dure: Int) = {
    sendShortMessage()(ShortMessage.NOTE_OFF)
    Thread.sleep(dure)
    sendShortMessage()(ShortMessage.NOTE_OFF)
  }

  /**
   * International Morse code is composed of five elements:[1]
   *
   * short mark, dot or "dit" (▄▄▄▄): "dot duration" is one time unit long
   * longer mark, dash or "dah" (▄▄▄▄▄▄): three time units long
   * inter-element gap between the dots and dashes within a character: one dot duration or one unit long
   * short gap (between letters): three time units long
   * medium gap (between words): seven time units long
   */
  val DOT_LENGTH = 250

  def dash = toneForMilliseconds(3 * DOT_LENGTH)

  def dot = toneForMilliseconds(DOT_LENGTH)

  def gap = silenceForMilliseconds(3 * DOT_LENGTH)

  def space = silenceForMilliseconds(7 * DOT_LENGTH)

  val charToCode = (for {
    charMappings: Iterator[String] <- getResource("/morseCode.dat")
  } yield
    (for (charMapping <- charMappings.toList if charMapping.trim.nonEmpty) yield (charMapping.head.toUpper, charMapping.tail.span(Set('\t', ' ').contains)._2)
      ).filterNot(_._1 == '#')) match {
    case None => // build enough in for an SOS
      Map(
        ('S', "..."),
        ('O', "---")
      )
    case Some(mapping: List[(Char, String)]) => mapping.toMap
  }

  @tailrec
  def playTones(tones: List[Char]): Unit = tones match {
    case Nil =>
      ()
    case head :: tail => head match {
      case '.' => dot
      case '-' => dash
    }
      playTones(tail)
  }

  def charToMorse(char: Char): Unit = {
    val tones = char match {
      case ' ' => space
      case _ => playTones(charToCode(char.toUpper).toList)
    }
  }

  def toMorse(encodable: LazyList[Char]): LazyList[Unit] = {
    encodable match {
      case empty if empty.isEmpty => LazyList.empty
      case last #:: LazyList() => LazyList(charToMorse(last))
      case head #:: tail =>
        val play: Unit = charToMorse(head)
        play #:: gap #:: toMorse(tail)
    }
  }

  def translateToMorse(encodable: String):List[Unit] = {
    val translation = toMorse(encodable = LazyList.from(encodable)).toList
    Thread.sleep(14 * DOT_LENGTH)
    translation
  }


}

