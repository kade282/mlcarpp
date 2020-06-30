#ładowanie bibliotek
library(tm)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(car)
library(MASS)
library(gridExtra)

#zmiana katalogu roboczego
workDir <- "C:\\Users\\kdylewska\\studia\\MLCarPredPrice"
setwd(workDir)

#załadowanie pliku
Auto <- read.csv("CarPrice.csv")
str(Auto) 
View(Auto)

### Usunięcie niepotrzebnych kolumn i wierszy
head(Auto, 5) # sprawdzenie pierwszych 5 wierszy
tail(Auto, 5) # nsprawdzenie ostatnich 5 wierszy
sum(duplicated(Auto)) # sprawdzenie duplikatów
sum(is.na(Auto)) # sprawdzenie brakujące wartości
str(Auto) # sprawdzenie formatu kolumn

Auto <- separate(Auto, CarName, into = c("carCompany", "carModel"), sep = " ", extra = 'merge') #odłączenie carCompany od CarName
data.frame(carcount = summary(as.factor(Auto$carCompany))) #sprawdzenie poprawności pisowni nazw firm

Auto$carCompany[which(Auto$carCompany == "maxda")] <- "mazda"
Auto$carCompany[which(Auto$carCompany == "Nissan")] <- "nissan"
Auto$carCompany[which(Auto$carCompany == "porcshce")] <- "porsche"
Auto$carCompany[which(Auto$carCompany == "toyouta")] <- "toyota"
Auto$carCompany[which(Auto$carCompany == "vokswagen")] <- "volkswagen" 
Auto$carCompany[which(Auto$carCompany == "vw")] <- "volkswagen"

Auto$carCompany <- factor(Auto$carCompany)
data.frame(carcount = summary(Auto$carCompany)) # ponowne sprawdzenie poprawności nazw firm

## Analiza

#1. Dystrybucja symboli (kolumna 1)

Auto$symboling <- factor(Auto$symboling)

plot1 <- ggplot(Auto, aes(x = symboling)) + 
  geom_bar() + theme_light() + scale_y_continuous(limits = c(0,70)) +
  geom_text(stat = "count", aes(label =..count..), vjust = -1) 

plot2 <- ggplot(Auto, aes(x = symboling, y = price)) + 
  geom_boxplot() + theme_light()

grid.arrange(plot1, plot2, nrow = 2)

#2. Zależność pomiędzy producentem marki, a ceną

plot1 <- ggplot(Auto, aes(x = carCompany)) + 
  geom_bar() + theme_light() + scale_y_continuous(limits = c(0,35)) +
  geom_text(stat = "count", aes(label =..count..), vjust = -1) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

plot2 <- ggplot(Auto, aes(x = carCompany, y = price)) + 
  geom_boxplot() + theme_light() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

grid.arrange(plot1, plot2, nrow = 2)

# 3. Zależność pomiędzy ceną auta a rodzajem paliwa napędowego

plot1 <- ggplot(Auto, aes(x = fueltype)) + 
  geom_bar() + theme_light() +
  geom_text(stat = "count", aes(label =..count..), vjust = -1)

plot2 <- ggplot(Auto, aes(x = fueltype, y = price)) + 
  geom_boxplot() + theme_light()

grid.arrange(plot1, plot2, nrow = 2)


#4. Zależność między ceną, a wyposażeniem auto w turbosprężarkę

plot1 <- ggplot(Auto, aes(x = aspiration)) + 
  geom_bar() + theme_light() +
  geom_text(stat = "count", aes(label =..count..), vjust = -1)

plot2 <- ggplot(Auto, aes(x = aspiration, y = price)) + 
  geom_boxplot() + theme_light()

grid.arrange(plot1, plot2, nrow = 2)

#5. Zależność pomiędzy ceną, a ilością drzwi (dataset nie uwzględnia, iż produkowane pojazdy wychodzą w zestawieniu 5-cio a nie 4-rodrzwiowe lub 3-rzy a nie 4-rodrzwiowe)

plot1 <- ggplot(Auto, aes(x = doornumber)) + 
  geom_bar() + theme_light() +
  geom_text(stat = "count", aes(label =..count..), vjust = -1)


plot2 <- ggplot(Auto, aes(x = doornumber, y = price)) + 
  geom_boxplot() + theme_light()

grid.arrange(plot1, plot2, nrow = 2)


#6. Zależność między ceną, a rodzajem nadwozia

plot1 <- ggplot(Auto, aes(x = carbody)) + 
  geom_bar() + theme_light() +
  geom_text(stat = "count", aes(label =..count..), vjust = -1)

plot2 <- ggplot(Auto, aes(x = carbody, y = price)) + 
  geom_boxplot() + theme_light()

grid.arrange(plot1, plot2, nrow = 2)


#7. Zależność między ceną, a rodzajem osi napędu

plot1 <- ggplot(Auto, aes(x = drivewheel)) + 
  geom_bar() + theme_light() +
  geom_text(stat = "count", aes(label =..count..), vjust = -1)

plot2 <- ggplot(Auto, aes(x = drivewheel, y = price)) + 
  geom_boxplot() + theme_light()

grid.arrange(plot1, plot2, nrow = 2)


#8. Zależność między ceną, a umiejscowieniem silnika w karoserii

plot1 <- ggplot(Auto, aes(x = enginelocation)) + 
  geom_bar() + theme_light() +
  geom_text(stat = "count", aes(label =..count..), vjust = -1)

plot2 <- ggplot(Auto, aes(x = enginelocation, y = price)) + 
  geom_boxplot() + theme_light()

grid.arrange(plot1, plot2, nrow = 2)


#9. Zależność między ceną, a rozstawem osi

plot1 <- ggplot(Auto, aes(x = wheelbase)) + 
  geom_histogram() + theme_light()

plot2 <- ggplot(Auto, aes(x = wheelbase, y = price)) + 
  geom_point() + theme_light()

plot3 <- ggplot(Auto, aes(x = factor(0), y = wheelbase)) + 
  geom_boxplot() + theme_light() + xlab(NULL) + 
  scale_x_discrete(breaks = NULL)

grid.arrange(plot1, plot2, plot3, layout_matrix = rbind(c(1,1,3), c(2,2,3)))


#10. Zależność między ceną, a długością auta

plot1 <- ggplot(Auto, aes(x = carlength)) + 
  geom_histogram() + theme_light()

plot2 <- ggplot(Auto, aes(x = carlength, y = price)) + 
  geom_point() + theme_light()

plot3 <- ggplot(Auto, aes(x = factor(0), y = carlength)) + 
  geom_boxplot() + theme_light() + xlab(NULL) + 
  scale_x_discrete(breaks = NULL)

grid.arrange(plot1, plot2, plot3, layout_matrix = rbind(c(1,1,3), c(2,2,3)))


#11. Zależność między ceną, a szerokością auta

plot1 <- ggplot(Auto, aes(x = carwidth)) + 
  geom_histogram() + theme_light()

plot2 <- ggplot(Auto, aes(x = carwidth, y = price)) + 
  geom_point() + theme_light()

plot3 <- ggplot(Auto, aes(x = factor(0), y = carwidth)) + 
  geom_boxplot() + theme_light() + xlab(NULL) + 
  scale_x_discrete(breaks = NULL)

grid.arrange(plot1, plot2, plot3, layout_matrix = rbind(c(1,1,3), c(2,2,3)))


#12. Zależność między ceną, a wysokością auta

plot1 <- ggplot(Auto, aes(x = carheight)) + 
  geom_histogram() + theme_light()

plot2 <- ggplot(Auto, aes(x = carheight, y = price)) + 
  geom_point() + theme_light()

plot3 <- ggplot(Auto, aes(x = factor(0), y = carheight)) + 
  geom_boxplot() + theme_light() + xlab(NULL) + 
  scale_x_discrete(breaks = NULL)

grid.arrange(plot1, plot2, plot3, layout_matrix = rbind(c(1,1,3), c(2,2,3)))


#13. Zależność między ceną, a masą auta

plot1 <- ggplot(Auto, aes(x = curbweight)) + 
  geom_histogram() + theme_light()

plot2 <- ggplot(Auto, aes(x = curbweight, y = price)) + 
  geom_point() + theme_light()

plot3 <- ggplot(Auto, aes(x = factor(0), y = curbweight)) + 
  geom_boxplot() + theme_light() + xlab(NULL) + 
  scale_x_discrete(breaks = NULL)

grid.arrange(plot1, plot2, plot3, layout_matrix = rbind(c(1,1,3), c(2,2,3)))


#14. Zależność między ceną, a typem silnika

plot1 <- ggplot(Auto, aes(x = enginetype)) + 
  geom_bar() + theme_light() +
  geom_text(stat = "count", aes(label =..count..), vjust = -1)

plot2 <- ggplot(Auto, aes(x = enginetype, y = price)) + 
  geom_boxplot() + theme_light()

grid.arrange(plot1, plot2, nrow = 2)


#15. Zależność między ceną, a liczbą cylindra

plot1 <- ggplot(Auto, aes(x = cylindernumber)) + 
  geom_bar() + theme_light() +
  geom_text(stat = "count", aes(label =..count..), vjust = -1)

plot2 <- ggplot(Auto, aes(x = cylindernumber, y = price)) + 
  geom_boxplot() + theme_light()

grid.arrange(plot1, plot2, nrow = 2)


#16. Zależność między ceną, a wielkością silnika

plot1 <- ggplot(Auto, aes(x = enginesize)) + 
  geom_histogram() + theme_light()

plot2 <- ggplot(Auto, aes(x = enginesize, y = price)) + 
  geom_point() + theme_light()

plot3 <- ggplot(Auto, aes(x = factor(0), y = enginesize)) + 
  geom_boxplot() + theme_light() + xlab(NULL) + 
  scale_x_discrete(breaks = NULL)

grid.arrange(plot1, plot2, plot3, layout_matrix = rbind(c(1,1,3), c(2,2,3)))


#17. Zależność między ceną, a pojemność skokowa silnika

plot1 <- ggplot(Auto, aes(x = fuelsystem)) + 
  geom_bar() + theme_light() +
  geom_text(stat = "count", aes(label =..count..), vjust = -1)

plot2 <- ggplot(Auto, aes(x = fuelsystem, y = price)) + 
  geom_boxplot() + theme_light()

grid.arrange(plot1, plot2, nrow = 2)


#20. Distribution by compression ratio

plot1 <- ggplot(Auto, aes(x = compressionratio)) + 
  geom_histogram() + theme_light()

plot2 <- ggplot(Auto, aes(x = compressionratio, y = price)) + 
  geom_point() + theme_light()

plot3 <- ggplot(Auto, aes(x = factor(0), y = compressionratio)) + 
  geom_boxplot() + theme_light() + xlab(NULL) + 
  scale_x_discrete(breaks = NULL)

grid.arrange(plot1, plot2, plot3, layout_matrix = rbind(c(1,1,3), c(2,2,3)))

# There are some very clear outlier values in compression ratio
# These all happen to be diesel cars, since there are very few disel cars to begin with
# we cannot eliminate these values, impact of these outliers can be investigated if this variable turns out to be significant

#21. Distribution by horsepower

plot1 <- ggplot(Auto, aes(x = horsepower)) + 
  geom_histogram() + theme_light()

plot2 <- ggplot(Auto, aes(x = horsepower, y = price)) + 
  geom_point() + theme_light()

plot3 <- ggplot(Auto, aes(x = factor(0), y = horsepower)) + 
  geom_boxplot() + theme_light() + xlab(NULL) + 
  scale_x_discrete(breaks = NULL)

grid.arrange(plot1, plot2, plot3, layout_matrix = rbind(c(1,1,3), c(2,2,3)))

# general increasing trend of price with horespower, there are some outliers
# the outliers all happen to be porsche cars, hence this data is significant and cant be removed

#22. Distribution by peakrpm

plot1 <- ggplot(Auto, aes(x = peakrpm)) + 
  geom_histogram() + theme_light()

plot2 <- ggplot(Auto, aes(x = peakrpm, y = price)) + 
  geom_point() + theme_light()

plot3 <- ggplot(Auto, aes(x = factor(0), y = peakrpm)) + 
  geom_boxplot() + theme_light() + xlab(NULL) + 
  scale_x_discrete(breaks = NULL)

grid.arrange(plot1, plot2, plot3, layout_matrix = rbind(c(1,1,3), c(2,2,3)))

# No clear trend of price with peakrpm

#23. Distribution by citympg

plot1 <- ggplot(Auto, aes(x = citympg)) + 
  geom_histogram() + theme_light()

plot2 <- ggplot(Auto, aes(x = citympg, y = price)) + 
  geom_point() + theme_light()

plot3 <- ggplot(Auto, aes(x = factor(0), y = citympg)) + 
  geom_boxplot() + theme_light() + xlab(NULL) + 
  scale_x_discrete(breaks = NULL)

grid.arrange(plot1, plot2, plot3, layout_matrix = rbind(c(1,1,3), c(2,2,3)))

# An interesting decreasing trend of price with city mpg, 
# There are outliers but price is pretty constant at that range of highway mpg so no need to remove

#24. Distribution by highway mpg

plot1 <- ggplot(Auto, aes(x = highwaympg)) + 
  geom_histogram() + theme_light()

plot2 <- ggplot(Auto, aes(x = highwaympg, y = price)) + 
  geom_point() + theme_light()

plot3 <- ggplot(Auto, aes(x = factor(0), y = highwaympg)) + 
  geom_boxplot() + theme_light() + xlab(NULL) + 
  scale_x_discrete(breaks = NULL)

grid.arrange(plot1, plot2, plot3, layout_matrix = rbind(c(1,1,3), c(2,2,3)))

# An interesting decreasing trend of price with highway mpg, similar to city mpg
# There are outliers but price is pretty constant at that range of highway mpg so no need to remove

#25. Distribution of price

plot1 <- ggplot(Auto, aes(x = price)) + 
  geom_histogram() + theme_light()

plot3 <- ggplot(Auto, aes(x = factor(0), y = price)) + 
  geom_boxplot() + theme_light() + xlab(NULL) + 
  scale_x_discrete(breaks = NULL)

grid.arrange(plot1, plot3, layout_matrix = rbind(c(1,1,3)))

# There are some outlier values - super expensive cars, removing these entries will remove prediction
# power of these pricey brands (jaguar, porsche, buick)

## Creating correlation matrix of continous variables

cormat <- cor(Auto[ , names(Auto) %in% c("wheelbase", "carlength", "carwidth", "carheight",
                                         "curbweight", "enginesize", "boreration", "stroke", 
                                         "compressionratio", "horsepower", "peakrpm", 
                                         "citympg", "highwaympg", "price")])

View(round(cormat, 2))
