import java.io.BufferedOutputStream
import java.io.BufferedWriter
import java.io.File
import java.io.FileOutputStream
import java.io.FileWriter
import java.io.IOException
import java.util.concurrent.CountDownLatch

import scala.annotation.tailrec
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.io.Source
import scala.reflect.ClassTag
import scala.util.control.Exception._
import scala.util.Success
import scala.util.Failure

import org.hirosezouen.hzutil.HZLog._
import org.hirosezouen.hzutil.HZIO._

object Motorola_S_record2Data {
    implicit val logger = getLogger(this.getClass.getName)

    def parseRecord[A:ClassTag](srecStr: String, produce: (String) => A): Future[A] = {
        val srec_r = "^S3(.*)".r
        Future {
            srecStr match {
                case srec_r(srec_d) => catching(classOf[NumberFormatException]) opt {
                    java.lang.Long.parseLong(srec_d.take(2), 16)
                } match {
                    case Some(srec_d_l) => {
                        val srec_b = srec_d.drop(2)
                        if((srec_b.length / 2) == srec_d_l) {
                            /* Skip 8 characters(4 octets * 2 characters) because it is an address which is waste. */
                            produce(srec_b.slice(8, srec_b.length-2))
                        } else {
                            throw new IllegalArgumentException(f"invalid length:expected=$srec_d_l%d,actual=${srec_b.length}%d")
                        }
                    }
                    case None => throw new IllegalArgumentException(s"invalid length format:${srec_d.take(2)}")
                }
                case _ => throw new IllegalArgumentException(s"invalid record start:$srecStr")
            }
        }
    }

    @tailrec
    def parseRecords[A:ClassTag](
        srcs: Seq[String],
        dsts: Buffer[Future[A]],
        produce: (String) => A): Seq[Future[A]]
    = srcs match {
        case Seq()               => dsts    /* finish to repeat function execution */
        case Seq(h, t_recs @ _*) => parseRecords(t_recs, dsts += parseRecord(h,produce), produce)
    }

    def parseAndOut[A:ClassTag](
        srcs: Seq[String],
        produce: (String) => A,
        outFunc: (Seq[A]) => Unit): Int
    = {
        val countDownLatch = new CountDownLatch(1)
        val fs = parseRecords(srcs,ListBuffer.empty[Future[A]],produce)
        val fseq = Future.sequence(fs)
        fseq.onComplete {
            case Success(la) => {
                outFunc(la)
                countDownLatch.countDown
            }
            case Failure(th) => {
                log_error(th.getMessage)
                countDownLatch.countDown
            }
        }
        Await.ready(fseq, Duration.Inf)
        countDownLatch.await
        fs.size
    }

    def loadData(file: File): Option[Seq[String]] = {
        catching(classOf[IOException]) either {
            using(Source.fromFile(file))(_.getLines.toList)
        } match {
            case Right(lines) => Some(lines)
            case Left(th: Throwable) => {
                log_error(th)
                None
            }
        }
    }

    object Mode extends Enumeration {
        type Mode = Value
        val Binary, Hexdump = Value
    }
    import Mode.Mode

    object Args {
        var mode: Mode = Mode.Binary
        var column_size: Int = 16
        var inFile: File = null
        var outFile: File = new File("out.bin")

        override def toString = 
            s"""|
                |mode        = $mode
                |column_size = $column_size
                |inFile      = $inFile
                |outFile     = $outFile""".stripMargin
    }

    def setupArgs(args: Array[String]): Option[String] = {
        var ret: Option[String] = None

        val f = new File(args.last)
        if(!f.exists()) return Some("input file not exists")
        else if(!f.isFile()) return Some(s"${f.getName} isn't file")
        else Args.inFile = f

        val itr = args.init.iterator
        var loopFlag = 1
        while((itr.hasNext) && (loopFlag == 1)) {
            def parseError(errMsg: String) = {
                ret = Some(errMsg)
                loopFlag = 0
            }
            def parseParamArgs(p: String, itr2: Iterator[String], setToArgs: (String) => Unit) {
                if(itr2.hasNext) {
                    setToArgs(itr2.next)
                } else
                    parseError(s"$p required more argument")
            }
            itr.next match {
                case "-m" => parseParamArgs("-m", itr, (mode) => mode.toLowerCase match {
                    case "binary" | "bin" | "b" => {
                        Args.mode = Mode.Binary
                        Args.outFile = new File("out.bin")
                    }
                    case "hexdump" | "hex" | "h" => {
                        Args.mode = Mode.Hexdump
                        Args.outFile = new File("out.txt")
                    }
                    case _ => parseError(s"unknown -m argument : $mode")
                })
                case "-c" => parseParamArgs("-c", itr,
                    (cs) => catching(classOf[NumberFormatException]) opt {
                        Args.column_size = Integer.parseInt(cs)
                    } match {
                        case Some(_) => /* OK */
                        case None    => parseError(s"malformed -c argument : $cs")
                    })
                case "-o" => parseParamArgs("-o", itr, (of) => Args.outFile = new File(of))
                case s => parseError(s"unknown parameter : $s")
            }
        }

        log_debug(Args.toString)

        ret
    }

    def printUsage() =
        log_info("""|
                    |Usage:
                    |Motorola_S_record2Data [-m mode] [-c column_size] [-o output_file] input_s_recored_file
                    |  -m mode              output mode("binary","bin","b" or "hexdump","hex","h")
                    |                       Default value is "binary".
                    |  -c column_size       affect only with "-m hexdump". The column size is used
                    |                       to split hex stream. Default value is "16".
                    |  -o output_file       output file name (If not specified, the default value
                    |                       is "out.bin")
                    |  input_s_recored_file S-record file to be converted
                    |""".stripMargin)

    def main(args: Array[String] ) {
        if(args.length == 0) {
            log_error("arguments required")
            printUsage
            sys.exit(1)
        }

        setupArgs(args) match {
            case Some(errMsg) => {
                log_error(errMsg)
                printUsage
                sys.exit(2)
            }
            case None => /* continue */
        }

        val srecords = loadData(Args.inFile) match {
            case Some(d) => d
            case None => sys.exit(2)
        }
        log_info(f"${srecords.length}%d redords found")

        val count = Args.mode match {
            case Mode.Binary => {
                val outStream = new BufferedOutputStream(new FileOutputStream(Args.outFile))
                parseAndOut[Array[Byte]](
                    srecords,
                    (data) => data.grouped(2).map(Integer.parseInt(_,16).toByte).toArray,
                    (lab) => using(outStream)(w => lab.foreach(w.write(_)))
                )
            }
            case Mode.Hexdump => {
                val writer = new BufferedWriter(new FileWriter(Args.outFile))
                parseAndOut[String](
                    srecords,
                    (data) => data,
                    (ls) => using(writer)(_.write(ls.mkString.grouped(Args.column_size*2).mkString(f"%n")))
                )
            }
        }

        log_info(f"$count%d redords executed")
    }
}

