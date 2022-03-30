# var2020 <- load_variables(2020, "pl")

var2010 <- load_variables(2010, "pl")

# Get demographic data for Census block groups to aggregate/apportion to beats geography
blocks <- get_decennial(geography = "block", 
                        year = 2020,
                        sumfile="pl",
                        output = 'wide',
                        variables = "P2_001N", 
                        state = "TX",
                        county = c("Harris County","Fort Bend","Montgomery"),
                        geometry = TRUE)
# Transform tidycensus data to match beats projection
blocks <- st_transform(blocks, 4326)
# Grab just the population and geometry from blockgroups from tidycensus
popblocks <- blocks["P2_001N"] # keep only the values to be transferred

# Calculate the estimated population of beat geographies/interpolate with tidycensus bgs
# Reminder: ext=true SUMS the population during interpolation
beats_withpopblocks <- st_interpolate_aw(popblocks, beats, ext = TRUE)



# Binds that new population column to the table
beats <- cbind(beats,beats_withpopblocks) %>% select(1:13)
# Two quick checks that we've got this right
# Checking that the geographies merged correct
# ifelse(beats$geometry == beats$geometry.1,"yes","no")
# Check total population assigned/estimated across all beats
sum(beats$P2_001N) # tally is 2.29M # city's reported pop is 2.30M in 2019; this works for our purposes
# Trim and clean beats file to just what we need
beats <- beats %>%
  rename("population"="P2_001N","beat"="Beats") %>%
  select(beat,Area_sq_mi,population,geometry) %>% 
  janitor::clean_names()
beats$population <- round(beats$population,0)
# beats$population <- ifelse(beats$population<2500,0,beats$population)
# rm(beats_withpopblocks)
sum(beats$population)



houston_murder_map <- leaflet(murders_by_beat, options = leafletOptions(zoomControl = FALSE)) %>%
  htmlwidgets::onRender("function(el, x) {
L.control.zoom({ position: 'topright' }).addTo(this)
}") %>%
  setView(-95.45, 29.75, zoom = 10) %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(color = "white", popup = murderlabel, weight = 0.5, smoothFactor = 0.5,
              opacity = 0.6, fillOpacity = 0.5,
              fillColor = ~murderpal(rate_multiyear)) %>% 
  addLegend(pal = murderpal, 
            opacity = 0.6,
            values = murders_by_beat$rate_multiyear, 
            position = "bottomleft", 
            title = "Homicides Per 100K people<br1>
            Over Last 3 Years (2019-2021)<br>
See also:<br><a href='https://abcotvdata.github.io/safetytracker_houston/sexualassault_rate.html'>Sexual Assault</a><br>
<a href='https://abcotvdata.github.io/safetytracker_houston/autothefts_rate.html'>Auto Thefts</a>") %>%
  addControl( html = "<style>
h5 {
  font-size: 28px;
  margin-top: 0;
  margin-bottom: 0;
  color: #0058f6;
}</style><h5>Headline for this map</h5>", position = "topleft", className = "fieldset {
    border: 0;
}")
houston_murder_map




## TESTING ONLY BELOW THIS POINT, IGNORE FOR NOW

Here's a quick look at a Flourish chart embedded within...

<div class="flourish-embed flourish-map" data-src="visualisation/4154506?174120"><script src="https://public.flourish.studio/resources/embed.js"></script></div>

Here's a quick look at an ABC OTV video embedded within...

<iframe width="500" height="300" src="https://abc7.com/video/embed/?pid=7762859" frameborder="0" allowfullscreen></iframe>
  
  Here's a quick look at a You Tube video from FiveThirtyEight embedded within...

<iframe width="500" height="300" src="https://www.youtube.com/embed/3gBe0rVhpHE" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>


Here's a quick look at an image embedded from a URL within...

```{r, echo=FALSE}
# Define variable containing url
url <- "https://cdn.abcotvs.com/dip/images/7750200_110720-cc-ap-biden-kamala-sat-pm-img.jpg"
```
## Some presidential types
<center><img src="`r url`"></center>
  
  Here's a quick look at a GIF animated image embedded within...

<center><img src="https://media4.giphy.com/media/j2pWZpr5RlpCodOB0d/giphy.gif"></center>





https://datawrapper.dwcdn.net/T23TE/3/

However, in the vast majority of neighborhoods in the city, the rate is much lower. In 17 of 118 of the Houston Police Department's, the murder rate was zero last year. In about half of the neighborhoods citywide, the murder rate is less than 10 per 100,000 residents - or less than half of the citywide rate.

Embed responsive
<iframe title="Houston Neighborhood Crime Test Table" aria-label="Table" id="datawrapper-chart-T23TE" src="https://datawrapper.dwcdn.net/T23TE/3/" scrolling="no" frameborder="0" style="width: 0; min-width: 100% !important; border: none;" height="732" data-external="1"></iframe><script type="text/javascript">!function(){"use strict";window.addEventListener("message",(function(e){if(void 0!==e.data["datawrapper-height"]){var t=document.querySelectorAll("iframe");for(var a in e.data["datawrapper-height"])for(var r=0;r<t.length;r++){if(t[r].contentWindow===e.source)t[r].style.height=e.data["datawrapper-height"][a]+"px"}}}))}();
</script>
  
  However, in the vast majority of neighborhoods in the city, the rate is much lower. In 17 of 118 of the Houston Police Department's, the murder rate was zero last year. In about half of the neighborhoods citywide, the murder rate is less than 10 per 100,000 residents - or less than half of the citywide rate.

Embed iframe
<iframe title="Houston Neighborhood Crime Test Table" aria-label="Table" id="datawrapper-chart-T23TE" src="https://datawrapper.dwcdn.net/T23TE/3/" scrolling="no" frameborder="0" style="border: none;" width="600" height="732"></iframe>


