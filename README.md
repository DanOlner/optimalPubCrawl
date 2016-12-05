# Pub crawl optimiser

RProject for finding the optimal pub crawl route using [openstreetmap pub data](http://wiki.openstreetmap.org/wiki/Tag:amenity%3Dpub), the [TSP package](https://cran.r-project.org/web/packages/TSP/index.html) and, if you want to account for hilliness, [querying google services](https://developers.google.com/maps/documentation/distance-matrix/). (Limited to a max of 50 pubs per day for a 2500 cell matrix if keeping to free limits. Which is probably enough for one pub crawl.)

![Starting at the Red Deer, Sheffield](https://github.com/DanOlner/optimalPubCrawl/blob/master/images/optimal.png)