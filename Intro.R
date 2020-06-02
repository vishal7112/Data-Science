#step1
height <- c(59,60,61,58,67,72,70)
weight <- c(150,140,180,220,160,140,130)
a <- 150
mean(height)
mean(weight)
length(height)
length(weight)
sum(height)
sum(weight)
sum(height)/length(height)
sum(weight)/length(weight)

#Step2
maxH <- max(height) 
maxH
minW <- min(weight)
minW

#step3
weight2 <- weight +5
weight2
wh <- weight2/height
wh

#Step4
if (maxH > 60 ) {
  "yes"
} else {
  "no"
}

if (minW > a) {
  "yes"
} else {
  "no"
}

