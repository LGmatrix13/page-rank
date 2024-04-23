import scala.annotation.tailrec
import scala.util.Random
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.ParMap

object PageRank {
    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return      A map of page.id to a weight of 1.0 for those same WebPage objects
     */
    def equal(pages: Map[String, WebPage]): Map[String, Double] = {
        for (pageId, page) <- pages yield pageId -> 1.0
    }

    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return A map of page.id to a weight that is a simple count of the number of pages linking to that page
     */
    def indegree(pages: Map[String, WebPage]): Map[String, Double] = {
        // should we filter out id so it doesn't count itself?
        pages.map {
            case (pageId, page) => pageId -> pages.filter(_._2.id != pageId).values.count(_.links.contains(pageId)).toDouble
        }
    }

    import scala.annotation.tailrec
    import scala.util.Random

    def pagerank(pages: Map[String, WebPage]): Map[String, Double] = {
        // Simulate random walks using tail recursion
        def simulateRandomWalks(): Map[String, Double] = {
            val randomPage = Random.shuffle(pages.keys).head
            val walks = 100
            val results: List[String] = (0 to 10000).par.map(user => simulateRandomWalk(pages, randomPage, walks)).seq.toList
            val numbers = results.groupBy(item => item).map(item => item._1 -> item._2.size)
            for (pageId, weight) <- pages yield if results.contains(pageId) then (pageId -> (numbers(pageId) + 1).toDouble / (10000 + pages.size).toDouble) else (pageId -> (0.0 + 1) / (10000 + pages.size).toDouble)
        }

        // Simulate a single random walk using tail recursion
        @tailrec
        def simulateRandomWalk(pages: Map[String, WebPage], currentPage: String, remainingSteps: Int): String = {
            // is this the right base case?
            if (remainingSteps <= 0) {
                currentPage
            } else if (pages(currentPage).links.isEmpty) {
                val newPage = Random.shuffle(pages.keys).head
                simulateRandomWalk(pages, newPage, remainingSteps - 1)
            } else if (Random.nextDouble() <= .85) {
                val nextPage = Random.shuffle(pages(currentPage).links).head
                simulateRandomWalk(pages, nextPage, remainingSteps - 1)
            } else {
                val newPage = Random.shuffle(pages.keys).head
                simulateRandomWalk(pages, newPage, remainingSteps - 1)
            }
        }

        // Start the simulation
        simulateRandomWalks()
    }
}