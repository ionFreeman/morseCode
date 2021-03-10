package nyc.freeman

import javax.sound.midi.{MidiDevice, MidiSystem, Receiver, ShortMessage}
import scala.util.Try

case class Tone(channel: Int = 0, tone: Int = 60, keyVelocity:Int = 93){
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

  def sendShortMessage(receiver: Receiver = MidiSystem.getReceiver)(message: Int) = {
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
}
