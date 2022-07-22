<<<<<<< HEAD
fname       <- read.table("../../data/paper/aeronet_2019.csv", sep=",", header=TRUE, strip.white=TRUE)
fname1      <- read.table("../../data/paper/suominet_2019.csv", sep=",")

fname2      <- read.table("../../data/paper/appendixB.csv", sep=",")

d <- as.Date(fname1[,2], "%Y-%m-%d")
A <- fname1[,3]
A <- replace(A, A<0, NaN)
names(A) <- d
l <- tapply(unlist(A, use.names=FALSE), rep(names(A), lengths(A)), FUN = c)

avg_l <- list()
for (i in 1:length(l)){
  avg_l <- append(avg_l, Reduce("+", unlist(l[i]))/length(unlist(l[i])))
}
  print(unlist(avg_l))
d1 <- as.Date(fname[,1], "%d:%m:%Y")
l1 <- fname[,26]

d2 <- as.Date(fname2[,1], "%m/%d/%Y")
Su <- fname2[,2]
Ae <- fname2[,3]

dates <- seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by=1)
final_date <- final_aero <- final_suomi <- list()
for (m in dates){
  t1 <- which(unique(d) %in% m)
  t2 <- which(d1 %in% m)
  final_date <- append(final_date, m)
  if (length(t1) > 0){
    final_suomi <- append(final_suomi, avg_l[[t1]])
  } else {
    final_suomi <- append(final_suomi, NaN)
  }
  if (length(t2) > 0){
    final_aero <- append(final_aero, l1[t2]*10)
  } else {
    final_aero <- append(final_aero, NaN)
  }
}
for (i in 1:length(d2)){
  final_date <- append(final_date, as.numeric(d2[i]))
  final_aero <- append(final_aero, as.numeric(Ae[i])*10)
  final_suomi <- append(final_suomi, as.numeric(Su[i])*10)
}
t <- data.frame(list(date=unlist(final_date), suomi_pw=unlist(final_suomi), aero_pw=unlist(final_aero)))
t <- within(t, {
       date <- as.Date(date, "%Y-%m-%d", origin="1970-01-01")
       suomi_pw  <- sprintf("%6.3f",suomi_pw)
       aero_pw  <-sprintf("%6.3f",aero_pw)
       })

write.table(t, file="../../data/paper/appendixB_2019.txt", quote=FALSE, row.names=FALSE, sep="\t")
#print(t)

=======
fname       <- read.table("../../data/paper/aeronet_2019.csv", sep=",", header=TRUE, strip.white=TRUE)
fname1      <- read.table("../../data/paper/suominet_2019.csv", sep=",")

fname2      <- read.table("../../data/paper/appendixB.csv", sep=",")

d <- as.Date(fname1[,2], "%Y-%m-%d")
A <- fname1[,3]
A <- replace(A, A<0, NaN)
names(A) <- d
l <- tapply(unlist(A, use.names=FALSE), rep(names(A), lengths(A)), FUN = c)

avg_l <- list()
for (i in 1:length(l)){
  avg_l <- append(avg_l, Reduce("+", unlist(l[i]))/length(unlist(l[i])))
}
  print(unlist(avg_l))
d1 <- as.Date(fname[,1], "%d:%m:%Y")
l1 <- fname[,26]

d2 <- as.Date(fname2[,1], "%m/%d/%Y")
Su <- fname2[,2]
Ae <- fname2[,3]

dates <- seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by=1)
final_date <- final_aero <- final_suomi <- list()
for (m in dates){
  t1 <- which(unique(d) %in% m)
  t2 <- which(d1 %in% m)
  final_date <- append(final_date, m)
  if (length(t1) > 0){
    final_suomi <- append(final_suomi, avg_l[[t1]])
  } else {
    final_suomi <- append(final_suomi, NaN)
  }
  if (length(t2) > 0){
    final_aero <- append(final_aero, l1[t2]*10)
  } else {
    final_aero <- append(final_aero, NaN)
  }
}
for (i in 1:length(d2)){
  final_date <- append(final_date, as.numeric(d2[i]))
  final_aero <- append(final_aero, as.numeric(Ae[i])*10)
  final_suomi <- append(final_suomi, as.numeric(Su[i])*10)
}
t <- data.frame(list(date=unlist(final_date), suomi_pw=unlist(final_suomi), aero_pw=unlist(final_aero)))
t <- within(t, {
       date <- as.Date(date, "%Y-%m-%d", origin="1970-01-01")
       suomi_pw  <- sprintf("%6.3f",suomi_pw)
       aero_pw  <-sprintf("%6.3f",aero_pw)
       })

write.table(t, file="../../data/paper/appendixB_2019.txt", quote=FALSE, row.names=FALSE, sep="\t")
#print(t)

>>>>>>> 47addb531525c32ecd60a78ae867ca064c77857a
