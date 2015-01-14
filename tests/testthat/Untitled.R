library(tiq)
library(dplyr)

tiqtest_dir = "/Volumes/bob/Development/tiq-test-Summer2014"

setRootPath(file.path(tiqtest_dir, "data"))

getAvailableDates("raw", "public_outbound")

outbound.ti = loadTI("raw", "public_outbound", "20140701")

outbound.ti %>%
  select(entity, type, direction, source, date)

inbound.novelty = noveltyTest("public_inbound", "20140615", "20140630",
                              select.sources=c("alienvault", "blocklistde",
                                               "dshield", "charleshaley"))

plotNoveltyTest(inbound.novelty)

overlap = overlapTest("public_inbound", "20140715", "enriched", select.sources=NULL)

overlap.plot = plotOverlapTest(overlap, title="Overlap Test - Inbound Data - 20140715")
print(overlap.plot)


outbound.pop = extractPopulationFromTI("public_outbound", "country",
                                       date = "20140711",
                                       select.sources=NULL, split.ti=F)
inbound.pop = extractPopulationFromTI("public_inbound", "country",
                                      date = "20140711",
                                      select.sources=NULL, split.ti=F)

complete.pop = loadPopulation("mmgeo", "country")
plotPopulationBars(c(inbound.pop, outbound.pop, complete.pop), "country")

outbound.aging = agingTest("public_outbound", "20140615", "20140715")
plotAgingTest(outbound.aging)

