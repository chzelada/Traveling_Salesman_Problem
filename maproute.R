library(ggmap)
library(gepaf)
library(readr)
library(tidyverse)
library(httr)
library(jsonlite)

a<-"gv`xAv`qgPRw@b@sB"
b<-"ot`xAj{pgPF@F?DAF?FADCDCDCBCBEDE@G?E?G?E?GAECEAECECCECCCGAMAI?I@IBG@GDA?g@Bi@ImEgAiAi@gA_AgA{Ak@oAk@eBw@aBe@k@g@[aA]e@YUg@yAaE_J}EA?kAs@"
c<-"e`bxAfsogPi@S_@SOKSQIMGIMQ[e@EMES"
d<-"{ebxAnmogPb@[jH{FfA{@FELQZk@\\k@p@qBb@qADMBIL]?SFo@Lc@?ARk@Pq@Nu@RmAHiAHiAF_AHy@Jy@@QHe@Fi@TeAz@iC~@cDhAsD"
e<-"cgaxArnmgPW?mIcA}Fg@MAkGu@wGq@cGq@kGm@_I_AEv@"
f<-"ofcxAtdmgPwD[wD]_DQqDYyCSA?f@_GN?"
g<-"u_dxAzxlgPh@kF"
h<-"k~cxAnqlgPjBD"
api_key <- "AIzaSyDFQ8BdKSVzqKQU-ZVB_-1NTzGrvyL3FBs"
ruta <- decodePolyline("gw`xAl`qgP^HRw@b@sBF@LANATOHK@MA[MUIGUCS@QDIDg@Bi@ImEgAiAi@gA_AgA{Ak@oAk@eBw@aBe@k@g@[aA]e@YUg@yAaEaJ}EkAs@i@So@_@]_@U[a@s@ESb@[rJwHTWx@wAtAcEHWL]?SFo@Lc@Rm@`@gBRmAHiAPiCTsBJw@\\oBzBmHhAsDW?mIcA}Fg@yGw@{OcBkGm@_I_AEv@wD[wD]_DQkIm@d@_GN?h@kFjBD")
ruta2<-rbind(decodePolyline(a),
      decodePolyline(b),
      decodePolyline(c),
      decodePolyline(d),
      decodePolyline(e),
      decodePolyline(f),
      decodePolyline(g),
      decodePolyline(h))

qmap('Aeropuerto la aurora',zoom=14,api_key = api_key) +
  geom_line(aes(x=lon, y=lat), data = ruta, colour= "red" )



url <- "https://maps.googleapis.com/maps/api/directions/json?origin=14.5912608,-90.5339537&destination=14.63975,-90.52187&key=AIzaSyDFQ8BdKSVzqKQU-ZVB_-1NTzGrvyL3FBs"
response <-GET(url)
resp_json <- fromJSON(content(response, as="text") )
ruta <- resp_json$routes$overview_polyline$points
ruta <- decodePolyline(ruta)



qmap('universidad galileo',zoom=17,api_key = api_key)

qmap('Aeropuesto la aurora',zoom=13,api_key = api_key) +
  geom_point(aes(x=lon, y=lat), data = ruta, colour= "red" )


ggmap(get_googlemap(center=c(lon=-90.5278, lat = 14.625 ),
                    zoom =13,
                    maptype = "roadmap")
      ) + 
  geom_point(aes(x=lon, y=lat), data = ruta, colour= "red" )



