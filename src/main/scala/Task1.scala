import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer

object Task1 {
  def main(args: Array[String]): Unit = {

    val books = List[(String, String)](
      ("book1", "src/main/scala/resources/sample1.txt"),
      ("book2", "src/main/scala/resources/sample2.txt"),
      ("book3", "src/main/scala/resources/sample3.txt")
    )

    val bookLoader = new BookLoader(books)
    val booksLoaded = bookLoader.load()

    printFrequentWords(5, booksLoaded)

    val booksTfidf = tfidf(booksLoaded)

    printTfidf(booksTfidf)
  }


  def printFrequentWords(n: Int, books: List[(String, Map[String, Int], _)]): Unit = {
    books.foreach(
      book => {
        println("Book: " + book._1)
        ListMap(book._2.toSeq.sortWith(_._2 > _._2): _*).take(n).foreach(p => println(f"${p._1}: ${p._2}"))
      }
    )
  }

  def printTfidf(booksTfidf: Map[String, Map[String, Double]]) = {
    booksTfidf.foreach(item =>{
      println("Book name: " + item._1)
      println(ListMap(item._2.toSeq.sortBy(_._1):_*))
      println()
    })
  }

  def tfidf(books: List[(String, Map[String, Int], List[String])]): Map[String, Map[String, Double]] = {
    val idfs = idf(books)

    var tfidfMap = Map[String, Map[String, Double]]()

    books.foreach(book => {

      var tfidfMapForBook = Map[String, Double]()

      book._2.foreach(wordCount => {
        val word = wordCount._1
        val tf = wordCount._2.toDouble / book._3.length
        val idf = idfs(word)

        tfidfMapForBook += (word -> tf * idf)

      })
      tfidfMap += (book._1, tfidfMapForBook)
    })
    tfidfMap
  }

  def idf(books: List[(String, Map[String, Int], List[String])]): Map[String, Double] = {
    var wordsIdf = Map[String, Double]()
    val listOfWordsSets = ListBuffer[Set[String]]()
    var allWordsSet = Set[String]()
    books.foreach(book => {
      val wordsSet = book._3.toSet
      listOfWordsSets.addOne(wordsSet)
      allWordsSet = allWordsSet.++(wordsSet)
    })

    allWordsSet.foreach(word => {
      wordsIdf += (word -> Math.log(books.length.toDouble / (documentFrequencyForWord(word, listOfWordsSets.toList))))
    })
    wordsIdf
  }

  def documentFrequencyForWord(word: String, listOfWordsSets: List[Set[String]]): Int = {
    var count = 0
    listOfWordsSets.foreach( set => {
      if(set.contains(word))
        count+=1
    })
    count
  }



}