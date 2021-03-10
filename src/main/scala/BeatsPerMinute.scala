package nyc.freeman
import java.time.temporal.{ChronoUnit, TemporalUnit}
import java.time.{Clock, Duration, Instant}
import java.util.TimeZone
import scala.annotation.tailrec
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

case class BeatsPerMinute(cadence:Int){
  require(cadence > 0 && cadence <= 60000)
  val tone: Tone = Tone()
  val clock: Clock = java.time.Clock.systemUTC()
  val minutes: Clock = java.time.Clock.tickMinutes(TimeZone.getTimeZone("Greenwich").toZoneId)
  @tailrec
  private def everyMinute(cadence:Int)(offset:Long = 0)(thisMinute:Instant):LazyList[Unit] = {
    val interval = 60000/cadence
    val initialIndex = 1 + (60000 - offset)/interval
    for(beat <- 1 to cadence){
      tone.toneForMilliseconds(interval match{
        case long if long > 200 => 100
        case short => short/2
      })
      val sleep = thisMinute.toEpochMilli + beat * interval - clock.millis()
      if (sleep > 0)
        Thread.sleep(sleep)
    }
    everyMinute(cadence)()(thisMinute.plus(1, ChronoUnit.MINUTES))
  }
  // manage the fact that you're probably not starting off at the top of the minute
  val offset = clock.millis() - minutes.millis()
  everyMinute(cadence)(offset)(minutes.instant())

  val start = clock.millis()
}
object BeatsPerMinute extends App {
  BeatsPerMinute(7)
}