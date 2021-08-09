# Time-varying effects of the shale boom.

In this repository, I explore the time-varying effects of the shale boom on communities in the U.S. My interest in this general topic is twofold: (1) I suspect that the economic effects of the shale boom - whether focused on labor, housing markets, etc., - have changed over time; and (2) I am the sort that requires an application to dive into the nuts-and-bolts of a new series of methods. 

For the latter point, I am interested in learning about the methods discussed in the flood of research on difference-in-differences variants with time-varying treatment. I'm particularly interested in the decomposition method of Goodman-Bacon (2021). The shale boom did not occur in all communities in the U.S. at a single time period. Instead, the treatment was staggered from 2005-2012 across the states. 

At this point, my interest is in learning the underlying methods, rather than the applied work. 

In fact, I don't believe that it's realistic to think about the shale boom in a binary fashion. 

* There is a ton of variation in the level of shale development over time, including a slow build-up in many places. It's convenient to believe that shale development went from 0 to 60 in a few seconds flat. But this was generally not the case.
* Many shale plays had substantial non-shale development prior to the boom. In these places, non-shale development decreased while shale development ramped up. The characteristics of drilling certainly changed and production grew but the underlying phenomena of oil and gas extraction had been going on before the shale boom. 

For now, I will continue in this line of thinking/modeling, as the principal reason for this work is my own learning. It is also my understanding that there are variants in development of time-varying treatment effects methods that allow for treatments measured in intensity, so perhaps these issues won't be relevant in the coming months.

# General structure of code.
* Data gathering & cleaning: 1-...
  * __1-download-all-data.R__: Downloads all/most data used in paper from the internet
  * __1-generate-all-spatial-data.py__: Generates all spatial connection between data-points
* Exploration of data: 2-...
  * __2-explore-data.R__: Explore general characteristics of data (e.g., distributions of key variables; treatment timing assumptions)
* Analysis of data: 3-...
* Additional exports of data, as neeeded: 4-...

