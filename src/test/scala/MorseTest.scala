package nyc.freeman

import org.junit.jupiter.api.{Test, BeforeAll}
import org.junit.jupiter.api.Assertions._


class MorseTest {
  val morse:Morse = Morse(Morse.loadMapping("morseCode.dat"))

  @Test def sossos() {morse.translateToMorse("SOS SOS")}

  @Test
  def yippityYap() {morse.translateToMorse("Yippity yap!")}

  @Test
  def heyWhoPutThatElephantInThePantryWhatIsThisAZoo() {morse.translateToMorse("Hey! Who put that elephant in the pantry? What is this, a zoo?")}
}