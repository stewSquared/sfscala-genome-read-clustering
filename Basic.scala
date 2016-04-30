import util.Random.nextLong
import util.Random.nextInt

object Solution {
  type Nucleotide = Char
  type DNA = Vector[Nucleotide]
  type Read = DNA // eventually, this includes quality, other metadata

  case class AlignedRead (read: Read, pos: Int)

  type BAM = Stream[AlignedRead]

  type Pileup = List[(AlignedRead, Nucleotide)]

  def result(files: List[BAM]): Stream[Pileup] = {
    cluster(mergeFiles(files))
  }

  def cluster(bam: BAM): Stream[Pileup] = {
    def cluster(bam: BAM, pos: Int, overlapsPrev: Pileup): Stream[Pileup] = {
      val (newOverlaps: BAM, rest: BAM) = bam.span(_.pos == pos)

      val overlapsHere: Pileup = (
        overlapsPrev.filter{case (ar, n) => (ar.pos + ar.read.length) > pos} ++
        newOverlaps.map(ar => (ar, ar.read(pos - ar.pos))).toList
      )

      overlapsHere #:: cluster(rest, pos+1, overlapsHere)
    }

    cluster(bam, 0, Nil)
  }

  def mergeFiles(files: List[BAM]): BAM = files.reduce(mergeTwoFiles)

  def mergeTwoFiles(fileA: BAM, fileB: BAM): BAM = {
    (fileA, fileB) match {
      case (ha #:: ta, hb #:: tb) =>
        if (ha.pos < hb.pos) ha #:: mergeTwoFiles(ta, fileB)
        else mergeTwoFiles(fileA, tb)
      case (a, Stream.Empty) => a
      case (Stream.Empty, b) => b
      case _ => Stream.empty
    }
  }

  // DEMO Stuff

  val bases = "ACTG"

  def randomDNA: DNA = {
    (nextLong.toBinaryString zip nextLong.toBinaryString).map{
      case (a, b) => bases(a.asDigit*2 + b.asDigit)
    }.toVector
  }

  def randomPos: Int = math.abs(nextInt % 300)

  def randomBam: BAM = {
    val genome = randomDNA ++ randomDNA ++ randomDNA ++ randomDNA ++ randomDNA ++ randomDNA ++ randomDNA
    (1 to 100)
      .map(_ => {
        val p = randomPos
        AlignedRead(genome.slice(p, p+20), p)})
      .sortBy(_.pos)
      .toStream
  }

  def showBam(bam: BAM): Stream[String] = randomBam map { ar => " "*ar.pos + ar.read.mkString}

}
