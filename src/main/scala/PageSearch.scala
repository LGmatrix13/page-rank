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
        pages.par.map(page => termsWithin(page, query).toDouble).seq.toList
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the term-frequency of the occurrences of those terms in each page in the same order given
     */
    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        val countResults = count(pages, query)
        countResults.zipWithIndex.par.map { case (countResult, index) => countResult / pages(index).text.length}.seq.toList
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the TF-IDF score for each page in the same order given
     */
    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        def idf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
            pages.par.map(page => {
                val d = termsWithin(page, query)
                if (d != 0) Math.log(pages.length.toDouble / d.toDouble) else 0
            }).seq.toList
        }
        val tfResults = tf(pages, query)
        val idfResults = idf(pages, query)
        tfResults.zipWithIndex.par.map { case (tfResult, index) => (tfResult * idfResults(index)) / pages(index).text.length }.seq.toList
    }
}