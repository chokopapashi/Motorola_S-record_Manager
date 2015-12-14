
import java.io.BufferedOutputStream
import java.io.File
import java.io.FileOutputStream
import java.io.IOException
import java.io.OutputStream

import scala.io.Source
import scala.util.control.Exception._

import org.hirosezouen.hzutil.HZLog._
import org.hirosezouen.hzutil.HZIO._

object Motorola_S_record2Data {
    implicit val logger = getLogger(this.getClass.getName)

    var executed_record_count = 0

    def parseAndOut(seq: Seq[String], outFunc: (String) => Unit) = {
        val record_r = "^S3(.*)".r
        for(rec <- seq) {
            rec match {
                case record_r(n1) => catching(classOf[NumberFormatException]) opt {
                    java.lang.Long.parseLong(n1.take(2), 16)
                } match {
                    case Some(length) => {
                        val n2 = n1.drop(2)
                        if((n2.length / 2) == length) {
                            executed_record_count += 1
                            /* 8(4 octet * 2 character) is address skiped */
                            for(b <- n2.slice(8, n2.length-2).grouped(2))
                                outFunc(b)
                        } else
                            log_error(f"invalid length:expected=$length%d,actual=${n2.length}%d")
                    }
                    case None => log_error(s"invalid length:${n1.take(2)}")
                }
                case r => log_error(s"invalid record start:$r")
            }
        }
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

    def initArgs(args: Array[String]): Option[String] = {
        var ret: Option[String] = None

        val f = new File(args.last)
        if(!f.exists()) return Some("input file not exists")
        else if(!f.isFile()) return Some(s"${f.getName} isn't file")
        else Args.inFile = f

        val itr = args.init.iterator
        var loopFlag = 1
        while((itr.hasNext) && (loopFlag == 1)) {
            def parseError(errMsg: String) = {ret = Some(errMsg) ; loopFlag = 0}
            itr.next match {
                case "-m" =>
                    if(itr.hasNext) {
                        val t = itr.next
                        l_t(s"-m $t")
                        t.toLowerCase match {
                            case "binary" | "bin" | "b" => {
                                Args.mode = Mode.Binary
                                Args.outFile = new File("out.bin")
                            }
                            case "hexdump" | "hex" | "h" => {
                                Args.mode = Mode.Hexdump
                                Args.outFile = new File("out.txt")
                            }
                            case _ => parseError(s"unknown -m argument : $t")
                        }
                    } else
                        parseError("-m required more argument")
                case "-c" =>
                    if(itr.hasNext) {
                        val cs = itr.next
                        l_t(s"-c $cs")
                        Args.column_size = Integer.parseInt(cs)
                    } else
                        parseError("-c required more argument")
                case "-o" =>
                    if(itr.hasNext) {
                        val of = itr.next
                        l_t(s"-o $of")
                        Args.outFile = new File(of)
                    } else
                        parseError("-o required more argument")
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

        initArgs(args) match {
            case Some(errMsg) => {
                log_error(errMsg)
                printUsage
                sys.exit(2)
            }
            case None => /* continue */
        }

        val records = loadData(Args.inFile) match {
            case Some(d) => d
            case None => sys.exit(2)
        }
        log_info(f"${records.length}%d redords found")

        val outStream = new BufferedOutputStream(new FileOutputStream(Args.outFile))

        ultimately {
            outStream.flush
            outStream.close
        } {
            Args.mode match {
                case Mode.Binary  => parseAndOut(records, (b) => outStream.write(Integer.parseInt(b,16)))
                case Mode.Hexdump => {
                    var cnt = 0
                    parseAndOut(records, (b) => {
                        outStream.write(b.getBytes)
                        cnt += 1
                        if((cnt % Args.column_size) == 0)
                            outStream.write(f"%n".getBytes)
                    })
                }
            }
        }

        log_info(f"${executed_record_count}%d redords executed")
    }
}

