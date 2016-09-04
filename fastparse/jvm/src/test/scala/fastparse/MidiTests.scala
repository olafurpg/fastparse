package fastparse

import java.nio.file.{Files, Paths}
import javax.sound.midi.MidiSystem

import utest._

import scala.collection.mutable


object MidiTests extends TestSuite{
  def readResourceBytes(file: String) = {
    Files.readAllBytes(Paths.get(getClass.getResource(file).toURI.getPath))
  }



  def variousParses(bytes: Array[Byte]) = Seq(
    MidiParse.midiParser.parse(bytes).get.value,
    MidiParse.midiParser.parseIterator(bytes.grouped(1)).get.value,
    MidiParse.midiParser.parseIterator(bytes.grouped(4)).get.value,
    MidiParse.midiParser.parseIterator(bytes.grouped(16)).get.value,
    MidiParse.midiParser.parseIterator(bytes.grouped(64)).get.value,
    MidiParse.midiParser.parseIterator(bytes.grouped(256)).get.value,
    MidiParse.midiParser.parseIterator(bytes.grouped(1024)).get.value
  )

  val tests = TestSuite{
    'canon{
      import Midi._
      val bytes = readResourceBytes("/canon.mid")
      for(parsed <- variousParses(bytes)){
        println(parsed.tracks.map(_.length))
        val expectedTrack0 = Seq(
          (0, MetaEvent.TimeSignature(4, 2, 24, 8)),
          (0, MetaEvent.KeySignature(0, false)),
          (0, MetaEvent.TimeSignature(4, 2, 24, 8)),
          (0, MetaEvent.Tempo(750000)),
          (1, MetaEvent.EndOfTrack)
        )
        val channels1 = parsed.tracks(1).collect{ case (dt, MidiEvent(channel, _)) => channel}

        assert(
          parsed.format == 1,
          parsed.tickDiv == Midi.TickDiv.Metric(256),
          parsed.tracks.length == 2,
          parsed.tracks(0) == expectedTrack0,
          // This is a simple midi with only one channel
          channels1.forall(_ == 0),
          parsed.tracks(1).length == 293
        )

        parsed.tracks(1).foreach(println)
      }
    }
    'chronoTrigger{
      import Midi._
      val bytes = readResourceBytes("/ctend.mid")
      for(parsed <- variousParses(bytes)){

        val expectedTrack0 = Seq(
          (0, MetaEvent.TimeSignature(1,2,24,8)),
          (0, MetaEvent.KeySignature(7,false)),
          (0, MetaEvent.Tempo(495867)),
          (120, MetaEvent.TimeSignature(4,2,24,8)),
          (37920, MetaEvent.KeySignature(-1,false)),
          (23040, MetaEvent.Tempo(472440)),
          (0, MetaEvent.EndOfTrack)
        )
        assert(
          parsed.format == 1,
          parsed.tickDiv == Midi.TickDiv.Metric(120),
          parsed.tracks.length == 19,
          // Compare first item separately since == does not work
          // on Array[Byte]
          parsed.tracks(0)(0)._2.isInstanceOf[SysExEvent.Message],
          parsed.tracks(0).drop(1) == expectedTrack0
        )
        parsed.tracks(0).foreach(println)
        assert()

      }
    }
    'tonghua{
      import Midi._
      val bytes = readResourceBytes("/tonghua.mid")

      for(parsed <- variousParses(bytes)) {
        assert(
          parsed.format == 1,
          parsed.tickDiv == Midi.TickDiv.Metric(352),
          parsed.tracks.length == 2,
          parsed.tracks.map(_.length) == Seq(1514, 894)
        )
      }
    }
    'clarity - variousParses(readResourceBytes("/clarity.mid"))
    'go - variousParses(readResourceBytes("/go.mid"))
    'stars - variousParses(readResourceBytes("/stars.mid"))
  }

}
