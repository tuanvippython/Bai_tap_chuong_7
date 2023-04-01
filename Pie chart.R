###7.1 The Pie Chart: biểu đồ tròn
setwd("D:/Code R/Dataset")
#Sử dụng một nguồn dữ liệu BFCases
BFCases = read.table(file = "Birdflucases.txt",header = TRUE)

###Vẽ biểu đồ tròn
#Biểu đồ tròn chuẩn
Cases = rowSums(BFCases[2:16])
names(Cases) = BFCases[,1]
Cases
par(mfrow = c(2,2), mar = c(3,3,2,1))
pie(Cases,
    main = "Ordinary pie chart") #A

#biểu đồ tròn với các phần được hiển thị theo hướng kim đồng hồ
pie(Cases,
    col = gray(seq(0.4,1.0,length = 6)),
    clockwise = TRUE,
    main ="Grey colours") #B

#biểu đồ tròn với màu sắc cầu vồng
pie(Cases,
    col = rainbow(6),
    clockwise = TRUE,
    main = "Rainbow colours") #C

#kích hoạt thư viện plotrix
library(plotrix)

#Biểu đồ hình tròn ba chiều
pie3D(Cases,
      labels = names(Cases),
      explode = 0.1,
      main = "3D pie chart",
      labelcex = 0.6) #D

###7.2 Biểu đồ cột và biểu đồ dải

setwd("D:/Code R/Dataset")
#sử dụng một nguồn dữ liệu BFDeaths
BFDeaths = read.table(file = "BirdFluDeaths.txt", header = TRUE ) # đọc hàng đầu tiên trong dữ liệu chứa tên cột

#Tính tổng số liệu từ cột 2 đến cột 16 trong BFDeaths và gán tên cho các giá trị tổng của số liệu bằng cột 1 của BFDeaths
Deaths = rowSums(BFDeaths[, 2:16]) 
names(Deaths) = BFDeaths[, 1]
Deaths

#Thiết lập tham số đồ họa(gồm 4 bảng 2 hàng và 2 cột) và thiết lập khoảng cách giữa các biểu đồ
par(mfrow = c(2, 2), mar = c(3, 3, 2, 1))

#Vẽ biểu đồ với tiêu đề "Bird flu cases"
barplot(Cases , main = "Bird flu cases") #A

Counts <- cbind(Cases, Deaths)
barplot(Counts) #B
barplot(t(Counts), col = gray(c(0.5, 1))) #C
barplot(t(Counts), beside = TRUE) #D

#Vẽ biểu đồ cột thể hiện giá trị trung bình với độ lệch chuẩn

setwd("D:/Code R/Dataset")
# sử dụng một nguồn dữ liệu có tên "RIKZ2.txt"
Benthic =  read.table(file = "RIKZ2.txt",
                      header = TRUE)

#tạo một vecto có tên "Bent.M" - hàm tapply dùng để tính giá trị trung bình bằng cách chia dữ liệu thành nhiều phần dựa trên Beach
Bent.M = tapply(Benthic$Richness,
                INDEX = Benthic$Beach,
                FUN = mean)
#tạo một vecto có tên "bent.sd" 
Bent.sd = tapply(Benthic$Richness,
                 INDEX = Benthic$Beach,
                 FUN = sd)

#gộp hai vecto "bent.M" và "bent.sd" thành một ma trận và hiển thị ra màn hình
MSD = cbind(Bent.M, Bent.sd)

MSD
#vẽ biểu đồ cột với giá trị trên trục y - tên "richness"  và tên "Beach" trên trục x, col để thiết lập màu cho các cột
barplot(Bent.M, xlab = "Beach",
        ylim = c(0, 20),
        ylab = "Richness",
        col = rainbow(9))

#gán biến bp cho biểu đồ trên
bp = barplot(Bent.M, xlab = "Beach",
             ylim = c(0,20),
             ylab = "Richness",
             col = rainbow(9))

#Thêm các đường mũi tên trên biểu đồ
arrows(bp, Bent.M, bp,
       Bent.M + Bent.sd,
       lwd = 1.5,
       angle = 90,
       length = 0.1)
#vẽ một khung viền trên biểu đồ
box()

bp

#Vẽ biểu đồ dải cho dữ liệu Benthic
Benth.le = tapply(Benthic$Richness,
                   INDEX = Benthic$Beach,
                   FUN = length)

Bent.se = Bent.sd / sqrt(Benth.le)

stripchart(Benthic$Richness ~ Benthic$Beach,
           vert = TRUE,
           pch = 1,
           method = "jitter",
           jit = 0.05,
           xlab = "Beach",
           ylab = "Richness") 

points(1:9, Bent.M, pch = 16, cex = 1.5)

arrows(1:9, Bent.M,1:9,
       Bent.M + Bent.se,
       lwd = 1.5,
       angle = 90, length = 0.1)

arrows(1:9, Bent.M,1:9, Bent.M - Bent.se,
       lwd = 1.5,
       angle = 90, length = 0.1)

#7.3 Vẽ biểu đồ hộp
setwd("D:/Code R/Dataset")
#sử dụng nguồn dữ liệu tên "Owls.txt"
Owls = read.table(file = "Owls.txt", header = TRUE)
#Thực hiện vẽ biểu đồ với nguồn dữ liệu cột "NegPerChick"
boxplot(Owls$NegPerChick)
#Thiết lập tham số đồ họa(gồm 4 bảng 2 hàng và 2 cột) và thiết lập khoảng cách giữa các biểu đồ
par(mfrow = c(2,2),
    mar = c(3, 3, 2, 1))
#Vẽ biểu đồ cột "NegPerChick" phụ thuộc vào cột "SexParent"
boxplot(NegPerChick ~ SexParent,
        data = Owls)
#Vẽ biểu đồ cột "NegPerChick" phụ thuộc vào cột "FoodTreatment"
boxplot(NegPerChick ~ FoodTreatment,
        data = Owls)
#Vẽ biểu đồ cột "NegPerChick" phụ thuộc vào hai cột "sexparent" và FoodTreatment
boxplot(NegPerChick ~ SexParent * FoodTreatment,
        data = Owls)
#Vẽ biểu đồ cột "NegPerChick" phụ thuộc vào hai cột "sexparent" và FoodTreatment - thiết lập lại tên các nhóm giá trị trên trục x của biểu đồ
boxplot(NegPerChick ~ SexParent * FoodTreatment,
          names = c("F/Dep", "M/Dep", "F/Sat", "M/Sat"),
          data = Owls)
#Vẽ biểu đồ cột "NegPerChick" phụ thuộc vào cột "Nest" lấy dữ liệu từ biến "Owls"
boxplot(NegPerChick ~ Nest, data = Owls)
#Thiết lập khoảng cách đồ thị
par(mar = c(2,2,3,3))

boxplot(NegPerChick ~ Nest,
        data = Owls,
        axes = FALSE,
        ylim = c(-3,8.5))

axis(2,at = c(0,2,4,6,8))

text(x = 1:27,
     y = -2,
     labels = levels(Owls$Nest),
     cex = 0.75,
     srt = 65)

#Vẽ biểu đồ hình hộp hiển thị dữ liệu Benthic
Benthic = read.table(file = "RIKZ2.txt",
                      header= TRUE)

Bentic.n = tapply(Benthic$Richness,
                  Benthic$Beach,
                   FUN = length) 
Bentic.n

boxplot(Richness ~ Beach,
        data = Benthic,
        col = "grey",
        xlab = "Beach",
        ylab = "Richness")

BP.info = boxplot(Richness ~ Beach,
                  data = Benthic,
                   col = "grey",
                  xlab = "Beach",
                   ylab = "Richness")

BP.midp = BP.info$stats[2, ] +
  (BP.info$stats[4, ] - BP.info$stats[2,])/2

text(1:9, BP.midp, Bentic.n, col = "white", font = 2)

#7.4 Vẽ đồ thị chấm tròn
#Sử dụng nguồn dữ liệu tên "Deer.txt"
Deer = read.table("Deer.txt", header = TRUE)
#Vẽ biểu đồ dữ liệu "Deer" với cột "LCT" và thiết lập tên trục x và tên trục y
dotchart(Deer$LCT,
         xlab = "Length (cm)",
         ylab = "Observation number")
#dùng hàm is.na để kiểm tra các giá trị trong dữ liệu có phải giá trị NA hay không
Isna <- is.na(Deer$Sex)
#Vẽ biểu đồ Dữ liệu "Deer" trong cột "LCT" và loại bỏ những giá trị NA, chuyển dữ liệu vecto sang factor
dotchart(Deer$LCT[!Isna],
         groups = factor(Deer$Sex[!Isna]),
         xlab = "Length (cm)",
         ylab = "Observation number grouped by sex")
#Thêm giá trị trung bình vào dotplot
Benthic = read.table(file = "RIKZ2.txt",
                      header = TRUE)

Benthic$fBeach = factor(Benthic$Beach)

par(mfrow = c(1, 2))

dotchart(Benthic$Richness,
         groups = Benthic$fBeach,
         xlab = "Richness", ylab = "Beach")

Bent.M = tapply(Benthic$Richness,
                Benthic$Beach,
                 FUN = mean)

dotchart(Benthic$Richness,
         groups = Benthic$fBeach,
         gdata = Bent.M, gpch = 19, 
         xlab = "Richness",
         ylab = "Beach")
#Hàm legend dùng để chú thích cho đồ thị
legend("bottomright", c("values", "mean"),
         pch = c(1, 19), bg = "white")

#7.5 tái khám phá hàm plot
methods(plot)

Benthic = read.table(file = "RIKZ2.txt",
                      header = TRUE)

Benthic$fBeach = factor(Benthic$Beach)
#Vẽ cột "Richness" phụ thuộc cột "fBeach" trong dữ liệu Benthic
plot(Benthic$Richness ~ Benthic$fBeach)

#Các chức năng khác của hàm plot
plot(y = Benthic$Richness,
     x = Benthic$NAP,
     xlab = "Mean high tide (m)",
     ylab = "Species richness",
     main = "Benthic data")
#Hàm lm() dùng để phân thích  hồi quy tuyến tính giữa một biến phụ thuộc và một biến độc lập
M0 = lm(Richness ~ NAP, data = Benthic)
#Hàm abline dùng  để vẽ đường thẳng trên đồ thị
abline(M0)

plot(y = Benthic$Richness,
     x = Benthic$NAP,
     xlab = "Mean high tide (m)",
     ylab = "Species richness",
     xlim = c(-3, 3), ylim = c(0,20))

plot(y = Benthic$Richness,
     x = Benthic$NAP,
     type = "n", axes = F,
     xlab = "Mean high tide",
     ylab = "Species richness")

points(y = Benthic$Richness, x = Benthic$NAP)

plot(y = Benthic$Richness,
     x = Benthic$NAP,
     type = "n",
     axes = FALSE,
     xlab = "Mean high tide",
     ylab = "Species richness",
     xlim = c(-1.75,2),
     ylim = c(0,20))

points(y = Benthic$Richness,
       x = Benthic$NAP)

axis(2, at = c(0, 10, 20), tcl = 1)
axis(1, at = c(-1.75, 0,2),
       labels = c("Sea", "Water line", "Dunes"))

#Vẽ biểu đồ dùng hàm legend
Birds = read.table(file = "loyn.txt", header = TRUE)

Birds$LOGAREA = log10(Birds$AREA)

plot(x = Birds$LOGAREA,
     y = Birds$ABUND,
     xlab = "Log transformed AREA",
     ylab = "Bird abundance")

M0 = lm(ABUND~ LOGAREA + GRAZE, data = Birds)
summary(M0)

LAR = seq(from = -1, to = 3, by = 1)
LAR

ABUND1 = 15.7 + 7.2 * LAR
ABUND2 = 16.1 + 7.2 * LAR
ABUND3 = 15.5 + 7.2 * LAR
ABUND4 = 14.1 + 7.2 * LAR
ABUND5 = 3.8 + 7.2 * LAR

lines(LAR, ABUND1, lty = 1, lwd = 1, col =1)
lines(LAR, ABUND2, lty = 2, lwd = 2, col =2)
lines(LAR, ABUND3, lty = 3, lwd = 3, col =3)
lines(LAR, ABUND4, lty = 4, lwd = 4, col =4)
lines(LAR, ABUND5, lty = 5, lwd = 5, col =5)

legend.txt = c("Graze 1", "Graze 2",
                
                "Graze 3", "Graze 4", "Graze 5")

legend("topleft",
       legend = legend.txt,
         col = c(1, 2, 3, 4, 5),
         lty = c(1, 2, 3, 4, 5),
         lwd = c(1, 2, 3, 4, 5),
         bty = "o", cex = 0.8)

#xác định điểm
plot(y = Benthic$Richness, x = Benthic$NAP,
     xlab = "Mean high tide (m)",
     ylab = "Species richness", main = "Benthic data")
identify(y = Benthic$Richness, x = Benthic$NAP)

#Thay đổi phông chữ và cỡ chữ
title("Bird abundance", cex.main = 2,
      family = "serif", font.main = 1)

#Thêm kí tự đặc biệt
Whales = read.table(file="TeethNitrogen.txt",
                     header = TRUE)

N.Moby = Whales$X15N[Whales$Tooth == "Moby"]
Age.Moby = Whales$Age[Whales$Tooth == "Moby"]
plot(x = Age.Moby, y = N.Moby, xlab = "Age",
       ylab = expression(paste(delta^{15}, "N")))

#7.6 vẽ đồ thị tương quan đôi
Benthic = read.table(file = "RIKZ2.txt",
                      header = TRUE)

pairs(Benthic[, 2:9])

#7.7 The Coplot
#A Coplot with a Single Conditioning Variable
Benthic = read.table(file = "RIKZ2.txt",
                      header = TRUE)

coplot(Richness ~ NAP | as.factor(Beach),
       pch=19,
         data = Benthic)

coplot(Richness ~ NAP | grainsize
       , pch=19,
       data = Benthic)
#The Coplot with Two Conditioning Variables
pHEire = read.table(file = "SDI2003.txt",
                     header = TRUE)
pHEire$LOGAlt = log10(pHEire$Altitude)

pHEire$fForested = factor(pHEire$Forested)

#Jazzing Up the Coplot

pHEire$Temp2 = cut(pHEire$Temperature, breaks = 2)
pHEire$Temp2.num = as.numeric(pHEire$Temp2)
cut(pHEire$Temperature, breaks = 2)

#7.8 Combining Types of Plots*

MyLayOut = matrix(c(2, 0, 1, 3), nrow = 2, ncol=2,
                   
                   byrow = TRUE)

MyLayOut

nf = layout(mat = MyLayOut, widths = c(3, 1),
             heights = c(1, 3), respect = TRUE)

layout.show(nf)

xrange <- c(min(Benthic$NAP), max(Benthic$NAP))
yrange <- c(min(Benthic$Richness),
              max(Benthic$Richness))

#First graph
par(mar = c(4, 4, 2, 2))
plot(Benthic$NAP, Benthic$Richness, xlim = xrange,
       ylim = yrange, xlab = "NAP", ylab = "Richness")
#Second graph
par(mar = c(0, 3, 1, 1))
boxplot(Benthic$NAP, horizontal = TRUE, axes = FALSE,
          frame.plot = FALSE, ylim = xrange, space = 0)
#Third graph
par(mar = c(3, 0, 1, 1))
boxplot(Benthic$Richness, axes = FALSE,
          ylim = yrange, space = 0, horiz = TRUE)
