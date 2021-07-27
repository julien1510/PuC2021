import java.io.File
import java.io.InputStream

class _FileReader() {

    fun StringToUnicode(str: String): IntArray {
        var res = intArrayOf()
        for (o in str) {
            res += o.toInt()
        }
        return res
    }

    fun readFileToUnicode(path: String): IntArray {
        return StringToUnicode(File(path).readText())
    }

    fun readFile(path: String) : String{
        return File(path).readText()
    }
}