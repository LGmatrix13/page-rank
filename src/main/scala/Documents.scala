import scala.math.Ordered

// Simple class representing the basic elements of a WebPage
class WebPage(val id: String, val name: String, val url: String,
              val text: String, val links: List[String])


// WebPage subclass added weight attribute
class RankedWebPage(id: String, name: String, url: String, text: String, links: List[String], val weight: Double)
                    extends WebPage(id,name,url,text,links) {
    
    // Auxiliary Constructor that supports a WebPage and a Weight
    def this(wp: WebPage, weight: Double) = this(wp.id, wp.name, wp.url, wp.text, wp.links, weight)
}

class SearchedWebPage(id:String, name:String, url:String, text:String, links:List[String], weight:Double, val textmatch:Double)
                    extends RankedWebPage(id,name,url,text,links,weight) {
    
    // Auxiliary Constructor that supports a RankedWebPage and a textmatch
    def this(wp: RankedWebPage, textmatch: Double) = this(wp.id, wp.name, wp.url, wp.text, wp.links, wp.weight, textmatch)


    // Auxiliary Constructor that supports a WebPage, weight, and textmatch
    def this(wp: WebPage, weight: Double, textmatch: Double) = this(new RankedWebPage(wp, weight), textmatch)
}