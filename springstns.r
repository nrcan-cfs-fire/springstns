library(cffdrs)
library(data.table)

fwiSpring <- function(input, init=data.frame(ffmc=85,dmc=6,dc=15,lat=55), batch=TRUE, out= "all", lat.adjust=TRUE, uppercase=TRUE)
{
  SPRING_BUI <- 6
  fwi.out1 <- data.table(fwi(input, init=init, batch=batch, out=out, lat.adjust=lat.adjust, uppercase=TRUE))
  fwi.out1 <- fwi.out1[, -c("DMC", "DC", "BUI")]
  fwi.out1[, FWI := Vectorize(cffdrs:::.fwiCalc)(ISI, SPRING_BUI)]
  fwi.out1[, DSR := 0.0272 * (FWI^1.77)]
  if (!uppercase) {
    names(fwi.out1) <- tolower(names(fwi.out1))
  }
  return(data.frame(fwi.out1))
}

data("test_fwi")
spring <- fwiSpring(test_fwi)
