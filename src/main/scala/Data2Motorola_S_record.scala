import java.io.File
import java.io.FileInputStream
import java.io.FileWriter
import java.io.IOException
import java.io.Writer

import scala.io.Source
import scala.util.control.Exception._

import org.hirosezouen.hzutil._
import HZLog._
import HZIO._

object Data2Motorola_S_record {
    implicit val logger = getLogger(this.getClass.getName)

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

    def execute_S_Format(seq: IndexedSeq[Byte], baseAddress: Long, writer: Writer) = {

        val recStr = new  StringBuilder()

        var addr = baseAddress
        var loopFlag = true
        val itr = seq.grouped(16)
        while(itr.hasNext && loopFlag) {
            val bytes = itr.next
//            l_t("%s".format(arrayToString(bytes.toArray[Byte])))
            recStr.clear
            recStr ++= "S3"
            recStr ++= "15" /* 15h(21) addr(4) + data(16) + sum(1) */
            recStr ++= "%08X".format(addr)

            var sum: Short = 0x15
            sum = checkSum(((addr >> 24) & 0x00FF).toShort, sum)
            sum = checkSum(((addr >> 16) & 0x00FF).toShort, sum)
            sum = checkSum(((addr >> 8)  & 0x00FF).toShort, sum)
            sum = checkSum((addr & 0x00FF).toShort, sum)

            for(byte <- bytes) {
                recStr ++= "%02X".format(byte)
                sum = checkSum((byte & 0x0ff).toShort, sum)
            }

            recStr ++= f"${~sum & 0x00ff}%02X"
            recStr ++= f"%n"
            catching(classOf[IOException]) either writer.write(recStr.mkString) match {
                case Right(_) => /* OK, continue */
                case Left(th: Throwable) => {
                    log_error(th)
                    loopFlag = false
                }
            }

            addr += 16
        }
    }

    def loadBinaryData(file: File): Option[IndexedSeq[Byte]] = {
        val buff = new Array[Byte](file.length.toInt)
        var inStream = new FileInputStream(file)
        catching(classOf[IOException]) andFinally {
            inStream.close
        } either {
            inStream = new FileInputStream(file)
            inStream.read(buff)
        } match {
            case Right(_) => Some(buff)
            case Left(th: Throwable) => {
                log_error(th)
                None
            }
        }
    }

    def loadHexdecimalTextData(file: File): Option[IndexedSeq[Byte]] = {
        val buff = new Array[Byte](file.length.toInt)
        catching(classOf[IOException]) either {
            val hexStr = using(Source.fromFile(file))(_.getLines.mkString)
            if((hexStr.length & 0x00000001) != 0)
                throw new IllegalArgumentException("malformed hexadecimal text (total length is odd number).")
            hexStr.grouped(2).map(java.lang.Byte.parseByte(_,16))
        } match {
            case Right(_) => Some(buff)
            case Left(th: Throwable) => {
                log_error(th)
                None
            }
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

    def initArgs(args: Array[String]): Option[String] = {
        var ret: Option[String] = None

        val f = new File(args.last)
        if(!f.exists()) return Some("input file not exists")
        else if(!f.isFile()) return Some("%s isn't file".format(f.getName))
        else Args.inFile = f

        val itr = args.init.iterator
        var loopFlag = 1
        while((itr.hasNext) && (loopFlag == 1)) {
            def parseError(errMsg: String) = ret = Some(errMsg) ; loopFlag = 0
            itr.next match {
                case "-m" =>
                    if(itr.hasNext) {
                        val t = itr.next
                        l_t(s"-m $t")
                        t.toLowerCase match {
                            case "binary" | "bin" | "b" => {
                                Args.mode = Mode.Binary
                            }
                            case "hexadecimal" | "hex" | "h" => {
                                Args.mode = Mode.Hexadecimal
                            }
                            case _ => parseError(s"unknown -m argument : $t")
                        }
                    } else
                        parseError("-m required more argument")
                case "-a" => if(itr.hasNext) {
                    val addr = itr.next
                    l_t("-a %s".format(addr))
                    catching(classOf[NumberFormatException]) opt java.lang.Long.parseLong(addr,16) match {
                        case Some(x) => Args.startAddress = x
                        case None => parseError("%s is invalid Address".format(addr))
                    }
                } else
                    parseError("-a required more option")
                case "-o" => if(itr.hasNext) {
                    val of = itr.next
                    l_t("-o %s".format(of))
                    Args.outFile = new File(of)
                } else
                    parseError("-o required more option")
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

        initArgs(args) match {
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
            
        val witer = new FileWriter(Args.outFile)
        ultimately {
            witer.flush
            witer.close
        } {
            execute_S_Format(data, Args.startAddress, witer)
        }
    }
}

