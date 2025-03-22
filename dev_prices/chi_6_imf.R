# imf
library(readxl)
library(zoo)
imf <- read_excel("~/Documents/Data/unctad/imf.xls")
head(imf)

View(t(imf[1,]))


# PWOOLC
# PWOOLF
# PSALM
# PHIDE
# PRAWM (maybe hide and skins?)
# 92

cols <- colnames(imf)

# a few are not named properly
which(cols == 'PWOOLC')
which(cols == 'PWOOLF')
which(cols == 'PSALM')
# which(cols == 'PRAWM')
which(cols == 'PHIDE')

# check if 92 is indeed manganese
imf[1, 92] 
cols[92] # manganese
# give it a name, PMANG

# now some cleaning
process_imf <- function(data){
  
  dimf <- data
  # change name for readability
  colnames(dimf)[1] <- 'yearmonth'
  colnames(dimf)[2] <- 'commodity_price'
  # only take from row 4
  dimf <- dimf[4:nrow(dimf),] 
  dimf$year <- as.numeric(substr(dimf$yearmonth, 1, 4))
  dimf$period <- substr(dimf$yearmonth, 5, 7)
  dimf <- filter(dimf, year >=1995)
  
  dimf$dtime <- as.Date(as.yearmon(dimf$yearmonth, "%YM%m"))
  # change data type for price
  dimf$price <- as.numeric(dimf$commodity_price)
  
  return(dimf)
}



# wool -----
# need to know whether it's coarse or fine

imf_woolc <- imf[,c(1, which(cols == 'PWOOLC'))]
imf_woolf <- imf[,c(1, which(cols == 'PWOOLF'))]


# make a function to process the data

woolf <- process_imf(imf_woolf)

plot(dimf$dtime, dimf$price)


# check AWI source

awi <- read_excel("~/Documents/Data/unctad/8001_AWI.xlsx", sheet = "ready for SAM")

awi$CommodityProduct |> table()
# 510100.02 is coarse
# 510100.03 is fine
# large amount of missing




# salmon ----
# 030212

# old source: ssb Norge
ssb <- read.csv("~/Documents/Data/unctad/7801_StatNorway_from_API.csv")

imf_salmon <- imf[,c(1, which(cols == 'PSALM'))]
imf_salmon

salmon_imf <- process_imf(imf_salmon)
plot(salmon_imf$dtime, salmon_imf$price)

nrow(imf_salmon)

head(ssb)
plot(ssb$Value, col = 'red')
points(salmon_imf$price[1:360]) # mostly same




# hide and skin ----
imf_hide <- imf[,c(1, which(cols == 'PHIDE'))]
imf_hide

hide_imf <- process_imf(imf_hide)
plot(hide_imf$price)

# what about rawm 
imf_rawm <- imf[,c(1, which(cols == 'PRAWM'))]
imf_rawm

rawm_imf <- process_imf(imf_rawm)
plot(rawm_imf$price)

# very different 



# manganese ----
# 260200.01
# 260200.02 (more likely to be this one)
# from metalbulletin


imf_mang <- imf[,c(1, 92)]
imf_mang

mang_imf <- process_imf(imf_mang)
plot(mang_imf$dtime, mang_imf$price)

mang_imf
filter(mang_imf, !is.na(price)) # only from 2012.6

# from mb
# source metabulletin data first
# manganese <- list('260200.02', 'Manganese', NA)
d2 <- filter(mb, CommodityProduct == '260200.02') |> select(dtime, Value)

plot(mang_imf$price, type = 'l')
lines(d2$Value, col = 'red')

# more or less
# from imf, only available from 2012.6














