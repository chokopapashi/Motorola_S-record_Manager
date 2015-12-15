import java.io.File
import java.io.FileInputStream
import java.io.FileWriter
import java.io.IOException
import java.io.Writer
import java.util.concurrent.CountDownLatch

import scala.annotation.tailrec
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.control.Exception._
import scala.util.Success
import scala.util.Failure

import org.hirosezouen.hzutil._
import HZLog._
import HZIO._

object Data2Motorola_S_record {
    implicit val logger = getLogger(this.getClass.getName)

    /* 古いチェックサム計算 */
//    def checkSum(s: Short, d: Short): Short = {
//        var r = d + s
//        if(0xFF < r)    /* overflow */
//            r = ((r >> 8) & 0x0FF)
//        r.toShort
//    }

    /* 
     * Workbenchから取得したSRECをバイナリにしてからSRECに戻すとチェックサムが合わない
     * 例)Workbench から取得したデータは"S3150303D2B013012914354100000000092613012914D5"だが
     *    バイナリに戻した後SRECにすると"S3150303D2B0130129143541000000000926130129141B"となる。
     *    ※D5,1Bの違い
     */
    def checkSum(b: Short, sum: Short): Short = {
        var r = sum + b
        if((0xFF00 & r) != 0)    /* overflow */
            r = (r & 0x00FF)
        r.toShort
    }

    def build_S_record(bytes: Array[Byte], addr: Long): Future[String] = {
        Future {
            val srec = StringBuilder.newBuilder
            srec ++= "S3"
            srec ++= "15" /* 15h(21) addr(4) + data(16) + sum(1) */
            srec ++= "%08X".format(addr)

            var sum: Short = 0x15
            sum = checkSum(((addr >> 24) & 0x00FF).toShort, sum)
            sum = checkSum(((addr >> 16) & 0x00FF).toShort, sum)
            sum = checkSum(((addr >> 8)  & 0x00FF).toShort, sum)
            sum = checkSum((addr & 0x00FF).toShort, sum)
            sum = bytes.foldLeft(sum){ (z,b) =>
                srec ++= "%02X".format(b)
                checkSum((b & 0x0ff).toShort, z)
            }
            srec ++= f"${~sum & 0x00ff}%02X"

            srec.mkString
        }
    }

    @tailrec
    def build_S_records(
        srcs: List[Array[Byte]],
        dsts: Buffer[Future[String]],
        addr: Long): Seq[Future[String]]
    = srcs match {
        case Nil         => dsts    /* finish to repeat function execution */
        case h :: t_srcs => build_S_records(t_srcs, dsts += build_S_record(h,addr), addr+16)
    }

    def parseAndOut(
        srcs: Array[Byte],
        baseAddress: Long,
        outFunc: (Seq[String]) => Unit): Int
    = {
        val countDownLatch = new CountDownLatch(1)
        val fs = build_S_records(srcs.grouped(16).toList, ListBuffer.empty[Future[String]], baseAddress)
        val fseq = Future.sequence(fs)
        fseq.onComplete {
            case Success(ls) => {
                outFunc(ls)
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

    def loadBinaryData(file: File): Option[Array[Byte]] = {
        catching(classOf[IOException]) either {
            using(new FileInputStream(file)){ in =>
                val buff = new Array[Byte](file.length.toInt)
                in.read(buff)
                buff
            }
        } match {
            case Right(ab) => Some(ab)
            case Left(th: Throwable) => {log_error(th) ; None}
        }
    }

    def loadHexdecimalTextData(file: File): Option[Array[Byte]] = {
        catching(classOf[IOException]) either {
            val hexStr = using(Source.fromFile(file))(_.getLines.mkString)
            if((hexStr.length & 0x00000001) != 0)
                throw new IllegalArgumentException("malformed hexadecimal text (total length is odd number).")
            hexStr.grouped(2).map(Integer.parseInt(_,16).toByte).toArray
        } match {
            case Right(ab) => Some(ab)
            case Left(th:IllegalArgumentException) => {log_error(th.getMessage) ; None}
            case Left(th: Throwable) => {log_error(th) ; None}
        }
    }

    object Mode extends Enumeration {
        type Mode = Value
        val Binary, Hexadecimal = Value
    }
    import Mode.Mode

    object Args {
        var mode: Mode = Mode.Binary
        var startAddress: Long = 0
        var inFile: File = null
        var outFile: File = new File("out.srec")

        override def toString = 
            f"""|
                |mode         = $mode%s
                |startAddress = $startAddress%08X
                |inFile       = $inFile%s
                |outFile      = $outFile%s""".stripMargin
    }

    def setupArgs(args: Array[String]): Option[String] = {
        var ret: Option[String] = None

        val f = new File(args.last)
        if(!f.exists()) return Some("input file not exists")
        else if(!f.isFile()) return Some("%s isn't file".format(f.getName))
        else Args.inFile = f

        val itr = args.init.iterator
        var loopFlag = 1
        while((itr.hasNext) && (loopFlag == 1)) {
            def parseError(errMsg: String) = {
                ret = Some(errMsg)
                loopFlag = 0
            }
            def parseParamArgs(p: String, itr2: Iterator[String], setToArgs: (String) => Unit) {
                if(itr2.hasNext)
                    setToArgs(itr2.next)
                else
                    parseError(s"$p required more argument")
            }
            itr.next match {
                case "-m" => parseParamArgs("-m", itr, (mode) =>
                    mode.toLowerCase match {
                        case "binary" | "bin" | "b" => Args.mode = Mode.Binary
                        case "hexadecimal" | "hex" | "h" => Args.mode = Mode.Hexadecimal
                        case _ => parseError(s"unknown -m argument : $mode")
                    })
                case "-a" => parseParamArgs("-a", itr, (as) =>
                    catching(classOf[NumberFormatException]) opt java.lang.Long.parseLong(as,16) match {
                        case Some(x) => Args.startAddress = x
                        case None => parseError("%s is invalid Address".format(as))
                    })
                case "-o" => parseParamArgs("-o", itr, (of) => Args.outFile = new File(of))
                case s => parseError("unknown optioin : %s".format(s))
            }
        }

        log_debug("%s".format(Args))

        ret
    }

    def printUsage() =
        log_info("""|Usage:
                    |Data2Motorola_S_record [-a <start_address] [-o output_file] input_data_file
                    |  -m mode          input mode("binary","bin","b" or "hexadecimal","hex","h")
                    |                   Default value -s "binary"
                    |  -a start_address start address represented with hexadecimal
                    |  -o output_file   output file name (If not specified, the default value
                    |                   is "out.srec")
                    |  input_data_file  data file which is formated binary or hexadecimal
                    |                   to be converted
                    |""".stripMargin)

    def main(args: Array[String] ) {
        if(args.length == 0) {
            println("error : arguments required.")
            printUsage
            sys.exit(1)
        }

        setupArgs(args) match {
            case Some(errMsg) => {
                println("error : %s.".format(errMsg))
                sys.exit(2)
            }
            case None => /* continue */
        }

        val data = Args.mode match {
            case Mode.Binary => loadBinaryData(Args.inFile) match {
                case Some(d) => d
                case None => sys.exit(3)
            }
            case Mode.Hexadecimal => loadHexdecimalTextData(Args.inFile) match {
                case Some(d) => d
                case None => sys.exit(4)
            }
        }
            
        parseAndOut(data,
                    Args.startAddress,
                    (ls) => using(new FileWriter(Args.outFile))(_.write(ls.mkString(f"%n")))
        )
    }
}

