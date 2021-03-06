package nyc

import nyc.freeman.Morse.getClass

import java.io.InputStream
import scala.io.Source

package object freeman {
  def getResource(pathFromResourcesFolder: String): Option[Iterator[String]] = {
    getClass.getResourceAsStream(pathFromResourcesFolder.head match {
      case '/' => pathFromResourcesFolder
      case _ => s"/$pathFromResourcesFolder"
    }) match {
      case nul if nul == null => None
      case resource: InputStream => Option(Source.fromInputStream(resource).getLines())
    }
  }
}
