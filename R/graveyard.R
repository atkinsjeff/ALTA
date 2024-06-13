# graveyard
# Load data ----
dat <- read.csv("./data/WordlClimCostaRica.csv")

# make dat long
dat %>%
  pivot_longer(
    cols = Tmin:Tavg,
    names_to = "var",
    #names_prefix = "wk",
    values_to = "value",
    values_drop_na = FALSE
  ) %>%
  data.frame() -> temp

# reformat
temp$Year <- as.integer(temp$Year)
temp$date <- as.Date(temp$date)

write.csv(temp, "./data/ACGTemp.csv", row.names = FALSE)

# reformat
dat %>%
  dplyr::group_by(siteID, Year) %>%
  summarize(P = sum(P, na.rm = TRUE),
            Tavg = mean(Tavg, na.rm = TRUE)) %>%
  data.frame() -> AnnualMeans


# make anomalies
dat %>%
  dplyr::group_by(siteID) %>%
  summarize(TavgNorm = mean(Tavg, na.rm = TRUE)) %>%
  data.frame() -> normals1

AnnualMeans %>%
  group_by(siteID) %>%
  summarize(PNorm = mean(P)) %>%
  data.frame() -> normals2

# make all normals
normals <- merge(normals1, normals2)

ClimateData <- merge(AnnualMeans, normals)
# make Anomalies
ClimateData$TAnom <- ClimateData$TavgNorm - ClimateData$Tavg
ClimateData$PAnom <- ClimateData$PNorm - ClimateData$P

# create positive and negative
ClimateData %>%
  mutate(TempSign = TAnom >= 0,
         PrecipSign = PAnom >= 0) %>%
  data.frame() -> ClimateData

write.csv(ClimateData, "./data/ClimateNormals.csv", row.names = FALSE)
