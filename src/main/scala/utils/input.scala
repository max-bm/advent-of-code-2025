package util

object Input:
    def load(day: String, filename: String = "input.txt"): Seq[String] =
        val path: os.Path = os.pwd / "inputs" / day / filename
        return os.read.lines(path)

    def loadString(day: String, filename: String = "input.txt"): String =
        val path: os.Path = os.pwd / "inputs" / day / filename
        return os.read(path)
