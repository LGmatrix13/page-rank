import scala.math.log
import scala.collection.parallel.CollectionConverters._

object PageSearch {
    /**
     * @param pages  a list of RankedWebPage objects to be searched
     * @param query  a list of search terms to be counted in those pages
     * @return       a list of the number of times any of the terms appeared in each page in the same order as given
     */
        
    private def termsContained(page: RankedWebPage, query: List[String]): Int = {
        val words = page.text.split("\\s+")
        (for word <- query yield words.count(_.equalsIgnoreCase(word))).sum
    }
    
    def count(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        for page <- pages yield termsContained(page, query)
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the term-frequency of the occurrences of those terms in each page in the same order given
     */
    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        val countResults = count(pages, query)
        (for i <- 0 to countResults.length yield countResults(i) / pages(i).text.length).toList
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the TF-IDF score for each page in the same order given
     */
    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        val tfResults = tf(pages, query)
        (for i <- 0 to tfResults.length yield tfResults(i) * pages.filter(page => page != pages(i)).map(page => termsContained(page, query)).sum).toList
    }
}