import scala.annotation.tailrec
import scala.util.Random
import scala.collection.parallel.CollectionConverters.*

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
        pages.map {
            case (pageId, page) => pageId -> pages.filter(_._2.id != pageId).values.count(_.links.contains(pageId)).toDouble
        }
    }

    import scala.annotation.tailrec
    import scala.util.Random

    def pagerank(pages: Map[String, WebPage]): Map[String, Double] = {
        val dampingFactor = 0.85
        val numSteps = 100
        val numUsers = 10000

        // Initialize page ranks
        val initialPageRanks = pages.keys.map(_ -> (1.0 / pages.size)).toMap

        // Simulate random walks using tail recursion
        @tailrec
        def simulateRandomWalks(pageRanks: Map[String, Double], remainingSteps: Int): Map[String, Double] = {
            if (remainingSteps <= 0) {
                return pageRanks
            }
            val numUsers = 1000
            val newPageRanks = (1 to numUsers).par.foldLeft(Map[String, Double]().withDefaultValue(0.0)) { (acc, _) =>
                val randomPage = Random.shuffle(pages.keys).head
                val finalPage = simulateRandomWalk(pages, pageRanks, randomPage, 100)
                acc + (finalPage -> (acc(finalPage) + (1.0 / numUsers)))
            }.map { case (page, rank) => page -> (rank / numUsers) }
            // Normalize page ranks
            val totalWalks = numUsers * numUsers
            val normalizedPageRanks = newPageRanks.map(page => (page._2 + 1) / (totalWalks + pages.size))
            simulateRandomWalks(newPageRanks, remainingSteps - 1)
        }

        // Simulate a single random walk using tail recursion
        @tailrec
        def simulateRandomWalk(pages: Map[String, WebPage], pageRanks: Map[String, Double], currentPage: String, remainingSteps: Int): String = {
            if (remainingSteps <= 0 || !pages.contains(currentPage) || Random.nextDouble() > dampingFactor || pages(currentPage).links.isEmpty) {
                return currentPage
            }
            val nextPage = Random.shuffle(pages(currentPage).links).head
            simulateRandomWalk(pages, pageRanks, nextPage, remainingSteps - 1)
        }

        // Start the simulation
        simulateRandomWalks(initialPageRanks, 100)
    }
}