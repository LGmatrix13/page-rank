import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.Sorting

@main def main(): Unit = {
        println("=============================================================")
        println("   _____          __      __      __.__.__  .__  .__ ")
        println("  /  _  \\   _____|  | __ /  \\    /  \\__|  | |  | |__| ____  ")
        println(" /  /_\\  \\ /  ___/  |/ / \\   \\/\\/   /  |  | |  | |  |/ __ \\")
        println("/    |    \\___ \\|     <   \\        /|  |  |_|  |_|  \\  ___/ ")
        println("\\____|__  /____  >__|_ \\   \\__/\\  / |__|____/____/__|\\___  >")
        println("        \\/     \\/     \\/        \\/                       \\/")
        println("=============================================================")

        // Load WebPage.id -> WebPage map to better handle graph
        val pages: Map[String, WebPage] = mapWebPages(loadWebPages()) // completed for you

        // TODO: Measure the importance of each page using one of the functions in PageRank
        val rankedPages: List[RankedWebPage] = List() // call PageRank.???? here

        // Get user input then perform search until ":quit" is entered
        var query: String = ""
        var terms: List[String] = List()
        while {
            // get search query from the user
            query = readLine("search> ")
            // split the users search query into separate terms based on spaces
            terms = query.trim.split(' ').toList
            // this is the last line in the expression i.e. the condition of our while loop
            terms != List(":quit")
        } do {
          // TODO: Measure the textual match of each page to these terms using one of the functions in PageSearch
          val searchedPages: List[SearchedWebPage] = List() // call PageSearch.???? here
          // normalize the ranges for weight and textmatch on these pages
          val pageArray = SearchedWebPageNormalize.normalize(searchedPages).toArray
          // sort this array based on the chosen averaging scheme i.e.
          //    (ArithmeticOrdering || GeometricOrdering || HarmonicOrdering)
          Sorting.quickSort(pageArray)(NameOrdering) // TODO: change this from name ordering to something else!!!
          // Print the top ranked pages in descending order
          for p <- pageArray.reverse.slice(0, 10) do println(f"${p.name}%-15s  ${p.url}")
          // print a divider to make reading the results easier
          println("=============================================================")
        }
    }

// Load a List of WebPage objects from the packaged prolandwiki.csv file
def loadWebPages(): List[WebPage] = {
    // create an input stream to the proglangwiki.csv
    val fh = Source.fromInputStream(
        getClass.getClassLoader.getResourceAsStream("proglangwiki.csv"))
    // load all pages from the file line by line
    val pages = (for line <- fh.getLines yield {
        val id::name::url::text::links = line.split(",").toList // warning, but will work
        new WebPage(id, name, url, text, links)
    }).toList
    fh.close
    pages
}

// Convert a List[WebPage] to a Map[String, WebPage]
def mapWebPages(pages: List[WebPage]): Map[String, WebPage] = (for page <- pages yield (page.id, page)).toMap