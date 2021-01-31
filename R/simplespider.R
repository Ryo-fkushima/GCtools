########## Import "sysdata.rda" at first !!! ##########

########## Parameters ##########
#elist <- c("La","Ce","Pr","Nd","Sm","Eu","Gd","Tb","Dy","Ho","Er","Tm","Yb","Lu")## elements of interest
#elist <- c("Cs","Rb","Ba","Th","U","Nb","K","La","Ce","Pb","Pr","Sr","P","Nd","Zr","Sm","Eu","Ti","Dy","Y","Yb","Lu")## elements of interest
#s <- example_data_GCtools## compositional matrix of a natural sample (dataframe) (ppm)
#nml <- "PM_MS95"## "NMORB_SM89", "EMORB_SM89", "OIB_SM89", "CI_MS95", "PM_MS95"
#output <- F


simplespider <- function(s, ymin, ymax, output){

if(ncol(s) >= 21){
   cat("Warning: TOO MANY SAMPLES!! \n")
}


p <- c(1,nrow(s))
q <- c(ymin, ymax)
plot(p,q,xaxt = "n", log = "y", ylim = c(ymin, ymax), xlab = "", ylab = "", las = 1, type = "n")
axis(1, at=1:nrow(s), labels=rownames(s))


color_list <- c("black","red","blue","forestgreen","gold","darkviolet","darkorange","skyblue","sienna","hotpink")

for(i in 1:ncol(s)){
  if(i <= 10){
    lines(s[,i], type = "b", pch = 20, lty=1, col= color_list[i])
    if(i == 1){cat("Sample",i,"(",colnames(s)[i],")",":", "black \n")}
    if(i == 2){cat("Sample",i,"(",colnames(s)[i],")",":", "red \n")}
    if(i == 3){cat("Sample",i,"(",colnames(s)[i],")",":", "blue \n")}
    if(i == 4){cat("Sample",i,"(",colnames(s)[i],")",":", "green \n")}
    if(i == 5){cat("Sample",i,"(",colnames(s)[i],")",":", "yellow \n")}
    if(i == 6){cat("Sample",i,"(",colnames(s)[i],")",":", "violet \n")}
    if(i == 7){cat("Sample",i,"(",colnames(s)[i],")",":", "orange \n")}
    if(i == 8){cat("Sample",i,"(",colnames(s)[i],")",":", "skyblue \n")}
    if(i == 9){cat("Sample",i,"(",colnames(s)[i],")",":", "brown \n")}
    if(i == 10){cat("Sample",i,"(",colnames(s)[i],")",":", "pink \n")}
  }
  if(i > 10 && i <= 20){
    lines(s[,i], type = "b", pch = 20, lty=2, col= color_list[i])
    if(i == 11){cat("Sample",i,"(",colnames(s)[i],")",":", "black (dashed)\n")}
    if(i == 12){cat("Sample",i,"(",colnames(s)[i],")",":", "red (dashed)\n")}
    if(i == 13){cat("Sample",i,"(",colnames(s)[i],")",":", "blue (dashed)\n")}
    if(i == 14){cat("Sample",i,"(",colnames(s)[i],")",":", "green (dashed)\n")}
    if(i == 15){cat("Sample",i,"(",colnames(s)[i],")",":", "yellow (dashed)\n")}
    if(i == 16){cat("Sample",i,"(",colnames(s)[i],")",":", "violet (dashed)\n")}
    if(i == 17){cat("Sample",i,"(",colnames(s)[i],")",":", "orange (dashed)\n")}
    if(i == 18){cat("Sample",i,"(",colnames(s)[i],")",":", "skyblue (dashed)\n")}
    if(i == 19){cat("Sample",i,"(",colnames(s)[i],")",":", "brown (dashed)\n")}
    if(i == 20){cat("Sample",i,"(",colnames(s)[i],")",":", "pink (dashed)\n")}
  }
  if(i > 20){
    lines(s[,i], type = "b", pch = 20, col= "grey60")
    cat("Sample",i,"(",colnames(s)[i + 1],")",":", "gray \n")
  }

}


#for(i in 1:ncol(s)){
#  lines(MG[,i], type = "b", pch = 20, col=rainbow(6)[((i - 1) %% 6) + 1])
#  if(((i - 1) %% 6) + 1 == 1){cat("Sample",i,"(",colnames(MG)[i],")",":", "red \n")}
#  if(((i - 1) %% 6) + 1 == 2){cat("Sample",i,"(",colnames(MG)[i],")",":", "yellow \n")}
#  if(((i - 1) %% 6) + 1 == 3){cat("Sample",i,"(",colnames(MG)[i],")",":", "green \n")}
#  if(((i - 1) %% 6) + 1 == 4){cat("Sample",i,"(",colnames(MG)[i],")",":", "cyan \n")}
#  if(((i - 1) %% 6) + 1 == 5){cat("Sample",i,"(",colnames(MG)[i],")",":", "blue \n")}
#  if(((i - 1) %% 6) + 1 == 6){cat("Sample",i,"(",colnames(MG)[i],")",":", "pink \n")}
#}


########## Data output ##########
if(output ==T){write.table(s, file = "output_simplespider.txt")}

}

