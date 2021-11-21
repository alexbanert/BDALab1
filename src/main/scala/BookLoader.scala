import scala.collection.mutable.ListBuffer
import scala.io.Source

class BookLoader (val books:  List[(String, String)]){
  def load(): List[(String, Map[String, Int], List[String])] = {
    val booksList = ListBuffer[(String, Map[String, Int], List[String])]()

    books.foreach(book => {
      val words = loadWordsFromFile(book._2)
      val counts = words.foldLeft(Map.empty[String, Int]){
        (count, word) => count + (word -> (count.getOrElse(word, 0) + 1))
      }
      val result = (book._1, counts, words.toList)
      booksList += result
    })
    booksList.toList
  }

  def loadWordsFromFile(filename: String) = {
    val stopwordsList = getStopwords()
    var wordsList = ListBuffer[String]()
    val source = Source.fromFile(filename)
    for(line <- source.getLines()) {
      val split = line.split(' ').toList
      for(word <- split) {
        val cleanWord = word.replaceAll("[^A-Za-z]+", "").toLowerCase
        if (cleanWord.nonEmpty && !stopwordsList.contains(cleanWord))
          wordsList += cleanWord
      }

    }
    wordsList
  }

  def getStopwords() = {
    val stopWordsFilepath = "src/main/scala/resources/stopwords.txt"
    val sourceFile = Source.fromFile(stopWordsFilepath)
    val stopWords = try sourceFile.getLines().toList finally sourceFile.close()
    stopWords
  }
}
