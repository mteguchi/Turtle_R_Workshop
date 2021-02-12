# Notes for workshop


# we are going to import data files

data1 <- read.table(file = "Growth data Nov 2008.txt",
                    header = TRUE, 
                    sep = "\t")

data2 <- read.table(file = "Growth data Nov 2008 cleaned.csv",
                    header = TRUE, 
                    sep = ",")

bigturtles <- subset(data2, SCL > 65)

bigturtles2 <- data2[data2$SCL > 65,]

z <- mean(bigturtles$Weight,na.rm = T)

hist(bigturtles$SCL, 
     xlab = "SCL (cm)", 
     main = "SCL of big turtles (> 65 cm)")

turtle_3030 <- data2[data2$Turtle.ID.Tag == "3030",]
turtle_3030 <- subset(data2, Turtle.ID.Tag == "3030")

plot(turtle_3030$Date.Caught, turtle_3030$SCL)

turtle_3030$Date <- strptime(turtle_3030$Date.Caught, 
                             format = "%m/%d/%Y")

plot(turtle_3030$Date, turtle_3030$SCL,
     type = "b",   # lines and points
     bty = "l")    # get rid of upper and right borders


data2withNoNA <- na.omit(data2)
summary(data2withNoNA)
summary(data2)

dim(data2withNoNA)
dim(data2)

rm(list=ls())


bone_data <- read.table(file = "Bone_SI.csv",
                        header = TRUE, 
                        sep = ",")

head(bone_data)
dim(bone_data)

varNames <- names(bone_data)

#extracting data between 2003 and 2008
dat2003_2008 <- subset(bone_data, Year>=2003 & Year<=2008)

summary(dat2003_2008$Year)

meanCCL <- mean(dat2003_2008$Est_CCL, na.rm = TRUE)
summary(dat2003_2008)

# get rid of NAs from Est_CCL variable here and assign
# results into a new variable called noNaCCL
noNaCCL <- na.omit(dat2003_2008$Est_CCL)

# the length of this resulting vector is the sample size
nCCL <- length(noNaCCL)

# SE is defined as SD/sqrt(n)
seCCL <- sd(noNaCCL)/sqrt(nCCL)

dat_LagA <- subset(bone_data, LAG == "LAG_A")
mean(dat_LagA$Est_Age)

plot(bone_data$d15N, 
     bone_data$d13C, 
     pch = 19,
     xlab = "d15N", ylab = "d13C",
     main = "d15N vs. d13C",
     col = "#6495ED")
#"coral1")
boneData <- bone_data
plot(boneData$d15N, boneData$d13C, 
     pch = 19,
     xlab = "d15N", ylab = "d13C",
     main = "d15N vs. d13C",
     cex = 5,
     col = rgb(255, 114, 86, 
               alpha = 0.4*255, 
               maxColorValue = 255))

plot(boneData$d15N, boneData$d13C, 
     pch = 19, 
     cex = 2 + boneData$Est_Age/mean(boneData$Est_Age),
     xlab = "d15N", ylab = "d13C",
     main = "d15N vs. d13C",
     col = rgb(155, 48, 255, 
               alpha = 0.4*255, 
               maxColorValue = 255))

# extracted the aprpoprate sizes of all points from the
# previous line and assigned them to a variable; scaledAge
scaledAge <- 2 + boneData$Est_Age/mean(boneData$Est_Age)

plot(boneData$d15N, boneData$d13C, 
     pch = 19, 
     cex = scaledAge,
     xlab = expression(d[15]*"N"), 
     ylab = expression(d[13]*"C"),
     main = expression(d[15]*"N vs. d"[13]*"C"),
     col = rgb(72, 118, 255, 
               alpha = 0.4*255, 
               maxColorValue = 255),
     bty = "l")


df1_Cali <- na.omit(data.frame(d15N = boneData$d15N, 
                               d13C = boneData$d13C))

plot1 <- ggplot(data = df1_Cali, 
                aes(x = d15N, y = d13C)) + 
  geom_point(colour = "coral1", 
             size = 4, 
             alpha = 0.6) + 
  xlab("d15N") + ylab("d13C") + 
  theme(axis.text = element_text(size = 12))

plot1
print(plot1)

df2_Cali <- na.omit(data.frame(d15N = boneData$d15N, 
                               d13C = boneData$d13C,
                               Age = boneData$Est_Age))

plot2 <- ggplot(data = df2_Cali, 
                aes(x = d15N, y = d13C)) + 
  geom_point(colour = "firebrick1", 
             aes(size = Age), 
             alpha = 0.6) + 
  xlab(expression(d[15]*"N")) + 
  ylab(expression(d[13]*"C")) + 
  theme(axis.text = element_text(size = 12),
        legend.position = c(0.8, 0.3)) + 
  theme_bw()

plot2

plot2

