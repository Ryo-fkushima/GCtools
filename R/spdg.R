########## Import "sysdata.rda" at first !!! ##########

########## Parameters ##########
#elist <- c("La","Ce","Pr","Nd","Sm","Eu","Gd","Tb","Dy","Ho","Er","Tm","Yb","Lu")## elements of interest
elist <- c("Cs","Rb","Ba","Th","U","Nb","K","La","Ce","Pb","Pr","Sr","P","Nd","Zr","Sm","Eu","Ti","Dy","Y","Yb","Lu")## elements of interest
s <- external## compositional matrix of a natural sample (dataframe) (ppm)
nml <- "PM_MS95"## "NMORB_SM89", "EMORB_SM89", "OIB_SM89", "CI_MS95", "PM_MS95"
output <- T

########## Warning ##########
if(ncol(s) >= 8){
  cat("Warning: TOO MANY SAMPLES!! \n")
}

if(all(elist %in% standards$X1) == F){
  cat("Error: Unexpected element(s) in the 'elist' vector \n")
  return()
}
########## Extracting data ##########
colnames(s) = c("element", colnames(s)[2:ncol(s)])

for(i in 1:length(elist)){
  if(elist[i] %in% s$element){
  }else{
    s[(nrow(s) + 1),1] <- elist[i]
  }
}

s_new <- subset(s, s$element %in% elist)
stds <- subset(standards, standards$X1 %in% elist)

rownames(s_new) <- s_new$element
rownames(stds) <- stds$X1

for(i in 1:length(elist)){
  s_new[elist[i],"element"] <- i
  stds[elist[i], "X1"] <- i
}

s_new$element <- as.integer(s_new$element)
stds$X1 <- as.integer(stds$X1)

s_new_o <- s_new[order(s_new$element),]
stds_o <- stds[order(stds$X1),]


s_new_o[is.na(s_new_o)] <- 0

s_calc <- as.matrix(s_new_o[,2:ncol(s_new_o)])
stds_calc <- as.matrix(stds_o[,nml])

plotdata <- matrix(0, nrow = nrow(s_calc), ncol = ncol(s_calc))

for(i in 1:ncol(plotdata)){
  plotdata[,i] <- s_calc[,i] / stds_calc
}
rownames(plotdata) <- elist
colnames(plotdata) <- colnames(s)[2:ncol(s)]
########## max, min detection ##########

plotdata[plotdata == 0] <- 9999999999

order_min <- -5
while(min(plotdata) > 10^(order_min)){
  order_min <- order_min + 1
}
order_min <- order_min - 1


plotdata[plotdata == 9999999999] <- 0


order_max <- 5
while(max(plotdata) < 10^(order_max)){
  order_max <- order_max - 1
}
order_max <- order_max + 1

plotdata[plotdata == 0] <- NA

########## Axes definition ##########
p <- c(1,length(elist))
q <- c(10^(order_min), 10^(order_max))
plot(p,q,xaxt = "n", log = "y", ylim = c(10^(order_min), 10^(order_max)), xlab = "", ylab = paste(nml, " normalized"), las = 1, type = "n")
axis(1, at=1:length(elist), labels=elist)
for(i in -5:order_max){
  abline(h = 10^(i), lwd=0.2)
}
########## drawing lines ##########
for(i in 1:ncol(plotdata)){
  lines(plotdata[,i], type = "b", pch = 20, col=rainbow(6)[((i - 1) %% 6) + 1])
  if(((i - 1) %% 6) + 1 == 1){cat("Sample",i,"(",colnames(s)[i + 1],")",":", "red \n")}
  if(((i - 1) %% 6) + 1 == 2){cat("Sample",i,"(",colnames(s)[i + 1],")",":", "yellow \n")}
  if(((i - 1) %% 6) + 1 == 3){cat("Sample",i,"(",colnames(s)[i + 1],")",":", "green \n")}
  if(((i - 1) %% 6) + 1 == 4){cat("Sample",i,"(",colnames(s)[i + 1],")",":", "cyan \n")}
  if(((i - 1) %% 6) + 1 == 5){cat("Sample",i,"(",colnames(s)[i + 1],")",":", "blue \n")}
  if(((i - 1) %% 6) + 1 == 6){cat("Sample",i,"(",colnames(s)[i + 1],")",":", "pink \n")}
}
########## Data output ##########
if(output ==T){write.table(plotdata, file = "output.txt")}


