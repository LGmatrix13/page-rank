import scala.math.log
import scala.collection.parallel.CollectionConverters._

object PageSearch {
    /**
     * @param pages  a list of RankedWebPage objects to be searched
     * @param query  a list of search terms to be counted in those pages
     * @return       a list of the number of times any of the terms appeared in each page in the same order as given
     */
    private def termsWithin(page: RankedWebPage, query: List[String]): Int = {
        (for term <- query yield page.text.sliding(term.length).count(window => window.toLowerCase == term.toLowerCase)).sum
    }

    def count(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        for page <- pages yield termsWithin(page, query)
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
        def idf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
            pages.map(page => {
                val d = termsWithin(page, query)
                if (d != 0) Math.log(pages.length.toDouble / d.toDouble) else 0
            })
        }
        val tfResults = tf(pages, query)
        val idfResults = idf(pages, query)
        tfResults.zipWithIndex.map { case (tfResult, i) => tfResult * idfResults(i) }
    }
}