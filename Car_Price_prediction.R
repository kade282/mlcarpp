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

############################ Przygotowanie danych pod proces budowy modelu regresji ######################################

## Utworzenie nowej ramki danych

carPrice <- Auto
str(carPrice)

## Usunięcie niepotrzebnych kolumn

data.frame(cbind(names(carPrice), 1:ncol(carPrice)))

carPrice <- carPrice[ , c(-1,-4)] 

# Usuwanie identyfikatora samochodu z analizy, ponieważ jest to unikalny identyfikator i nie zawiera żadnych informacji
# Po usunięciu nazwy modelu samochodu, tylko firma samochodowa zostanie uwzględniona w analizie

# Zmienne dummies

# Zmiene z dwoma poziomami

levels(carPrice$fueltype) <- c(1,0)
carPrice$fueltype <- as.numeric(levels(carPrice$fueltype))[carPrice$fueltype]

levels(carPrice$aspiration) <- c(1,0)
carPrice$aspiration <- as.numeric(levels(carPrice$aspiration))[carPrice$aspiration]

levels(carPrice$doornumber) <- c(1,0)
carPrice$doornumber <- as.numeric(levels(carPrice$doornumber))[carPrice$doornumber]

levels(carPrice$enginelocation) <- c(1,0)
carPrice$enginelocation <- as.numeric(levels(carPrice$enginelocation))[carPrice$enginelocation]

# Zmienne z więcej niż dwoma poziomami

dummy_symboling <- data.frame(model.matrix( ~symboling, data = carPrice))
dummy_symboling <- dummy_symboling[,-1]
carPrice <- cbind(carPrice[,!names(carPrice) == "symboling"], dummy_symboling)

dummy_carCompany <- data.frame(model.matrix( ~carCompany, data = carPrice))
dummy_carCompany <- dummy_carCompany[,-1]
carPrice <- cbind(carPrice[,!names(carPrice) == "carCompany"], dummy_carCompany)

dummy_carbody <- data.frame(model.matrix( ~carbody, data = carPrice))
dummy_carbody <- dummy_carbody[,-1]
carPrice <- cbind(carPrice[,!names(carPrice) == "carbody"], dummy_carbody)

dummy_drivewheel <- data.frame(model.matrix( ~drivewheel, data = carPrice))
dummy_drivewheel <- dummy_drivewheel[,-1]
carPrice <- cbind(carPrice[,!names(carPrice) == "drivewheel"], dummy_drivewheel)

dummy_enginetype <- data.frame(model.matrix( ~enginetype, data = carPrice))
dummy_enginetype <- dummy_enginetype[,-1]
carPrice <- cbind(carPrice[,!names(carPrice) == "enginetype"], dummy_enginetype)

dummy_cylindernumber <- data.frame(model.matrix( ~cylindernumber, data = carPrice))
dummy_cylindernumber <- dummy_cylindernumber[,-1]
carPrice <- cbind(carPrice[,!names(carPrice) == "cylindernumber"], dummy_cylindernumber)

dummy_fuelsystem <- data.frame(model.matrix( ~fuelsystem, data = carPrice))
dummy_fuelsystem <- dummy_fuelsystem[,-1]
carPrice <- cbind(carPrice[,!names(carPrice) == "fuelsystem"], dummy_fuelsystem)

str(carPrice)

data.frame(cbind(names(carPrice), 1:ncol(carPrice))) # 68 niezależnych zmiennych

## Dane pochodne

# Tworzenie zmiennej mocy silnika
carPrice$enginepower <- carPrice$horsepower/carPrice$enginesize #powszechnie stosowane wskaźniki branżowe

##################################### Proces budowy modelu regresji ###################################

## Tworzenie testowego i treningowego datasetu

set.seed(100)
trainindices = sample(1:nrow(carPrice), 0.7*nrow(carPrice))
train = carPrice[trainindices,]
test = carPrice[-trainindices,]

## Proces budowy modelu

model1 <- lm(price ~ ., data = train)
summary(model1)

# Good Rsquared values but very large number of variables
# Very few significant varibales
# Some NA coefficients due to singularities i.e strong correlations seen

## Using StepAIC

stepAIC(model1, direction = "both")

# Several variables have been eliminated

model2 <- lm(price ~ aspiration + enginelocation + carlength + 
               carwidth + curbweight + enginesize + stroke + peakrpm + citympg + 
               symboling.1 + symboling0 + symboling3 + carCompanybmw + carCompanybuick + 
               carCompanydodge + carCompanyhonda + carCompanyjaguar + carCompanymazda + 
               carCompanymercury + carCompanymitsubishi + carCompanynissan + 
               carCompanypeugeot + carCompanyplymouth + carCompanyporsche + 
               carCompanyrenault + carCompanysaab + carCompanysubaru + carCompanytoyota + 
               carCompanyvolkswagen + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + drivewheelrwd + enginetypeohc + 
               enginetyperotor + cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi, data = train)
summary(model2)

# lot more significant variables can be seen in model 2

data.frame(vif(model2))

# There are several variables with high VIF
# carlenght, carwidth, curbweight, enginesize are correlated variables with high VIF
# carlenght has highest p value, lets remove

model3 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + enginesize + stroke + 
               peakrpm + citympg + 
               symboling.1 + symboling0 + symboling3 + carCompanybmw + carCompanybuick + 
               carCompanydodge + carCompanyhonda + carCompanyjaguar + carCompanymazda + 
               carCompanymercury + carCompanymitsubishi + carCompanynissan + 
               carCompanypeugeot + carCompanyplymouth + carCompanyporsche + 
               carCompanyrenault + carCompanysaab + carCompanysubaru + carCompanytoyota + 
               carCompanyvolkswagen + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + drivewheelrwd + enginetypeohc + 
               enginetyperotor + cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi, data = train)
summary(model3)

data.frame(vif(model3))

# didnt really help, let's remove some other insignificant variables
# citympg and all symboling dummies are insignificant

# removing citympg, highest p-value
model4 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + enginesize + stroke + 
               peakrpm + carCompanybmw + carCompanybuick +  symboling.1 + symboling0 + symboling3 +
               carCompanydodge + carCompanyhonda + carCompanyjaguar + carCompanymazda + 
               carCompanymercury + carCompanymitsubishi + carCompanynissan + 
               carCompanypeugeot + carCompanyplymouth + carCompanyporsche + 
               carCompanyrenault + carCompanysaab + carCompanysubaru + carCompanytoyota + 
               carCompanyvolkswagen + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + drivewheelrwd + enginetypeohc + 
               enginetyperotor + cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi, data = train)
summary(model4)

data.frame(vif(model4))

# Lets remove fuelsystemmpfi, it has highest p-value

model5 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + enginesize + stroke + 
               peakrpm + carCompanybmw + carCompanybuick +  symboling.1 + symboling0 + symboling3 +
               carCompanydodge + carCompanyhonda + carCompanyjaguar + carCompanymazda + 
               carCompanymercury + carCompanymitsubishi + carCompanynissan + 
               carCompanypeugeot + carCompanyplymouth + carCompanyporsche + 
               carCompanyrenault + carCompanysaab + carCompanysubaru + carCompanytoyota + 
               carCompanyvolkswagen + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + drivewheelrwd + enginetypeohc + 
               enginetyperotor + cylindernumberfive + fuelsystem2bbl, data = train)
summary(model5)

data.frame(vif(model5))

# Remove fuelsystem2bbl. highest p-value

model6 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + enginesize + stroke + 
               peakrpm + carCompanybmw + carCompanybuick +  symboling.1 + symboling0 + symboling3 +
               carCompanydodge + carCompanyhonda + carCompanyjaguar + carCompanymazda + 
               carCompanymercury + carCompanymitsubishi + carCompanynissan + 
               carCompanypeugeot + carCompanyplymouth + carCompanyporsche + 
               carCompanyrenault + carCompanysaab + carCompanysubaru + carCompanytoyota + 
               carCompanyvolkswagen + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + drivewheelrwd + enginetypeohc + 
               enginetyperotor + cylindernumberfive, data = train)
summary(model6)

data.frame(vif(model6))

# remove symboling0, highest p-value

model7 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + enginesize + stroke + 
               peakrpm + carCompanybmw + carCompanybuick +  symboling.1 + symboling3 +
               carCompanydodge + carCompanyhonda + carCompanyjaguar + carCompanymazda + 
               carCompanymercury + carCompanymitsubishi + carCompanynissan + 
               carCompanypeugeot + carCompanyplymouth + carCompanyporsche + 
               carCompanyrenault + carCompanysaab + carCompanysubaru + carCompanytoyota + 
               carCompanyvolkswagen + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + drivewheelrwd + enginetypeohc + 
               enginetyperotor + cylindernumberfive, data = train)
summary(model7)

data.frame(vif(model7))

# remove symboling1, highest p-value

model8 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + enginesize + stroke + 
               peakrpm + carCompanybmw + carCompanybuick + symboling3 +
               carCompanydodge + carCompanyhonda + carCompanyjaguar + carCompanymazda + 
               carCompanymercury + carCompanymitsubishi + carCompanynissan + 
               carCompanypeugeot + carCompanyplymouth + carCompanyporsche + 
               carCompanyrenault + carCompanysaab + carCompanysubaru + carCompanytoyota + 
               carCompanyvolkswagen + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + drivewheelrwd + enginetypeohc + 
               enginetyperotor + cylindernumberfive, data = train)
summary(model8)

data.frame(vif(model8))

# remove car company mercury, highest p-value, has only one data point anyway

model9 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + enginesize + stroke + 
               peakrpm + carCompanybmw + carCompanybuick + symboling3 +
               carCompanydodge + carCompanyhonda + carCompanyjaguar + carCompanymazda + 
               carCompanymitsubishi + carCompanynissan + 
               carCompanypeugeot + carCompanyplymouth + carCompanyporsche + 
               carCompanyrenault + carCompanysaab + carCompanysubaru + carCompanytoyota + 
               carCompanyvolkswagen + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + drivewheelrwd + enginetypeohc + 
               enginetyperotor + cylindernumberfive, data = train)
summary(model9)

data.frame(vif(model9))

# remove symboling 3, highest p-value

model10 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + enginesize + stroke + 
                peakrpm + carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + carCompanyjaguar + carCompanymazda + 
                carCompanymitsubishi + carCompanynissan + 
                carCompanypeugeot + carCompanyplymouth + carCompanyporsche + 
                carCompanyrenault + carCompanysaab + carCompanysubaru + carCompanytoyota + 
                carCompanyvolkswagen + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd + enginetypeohc + 
                enginetyperotor + cylindernumberfive, data = train)
summary(model10)

data.frame(vif(model10))

# remove cylinder number five, highest p-value

model11 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + enginesize + stroke + 
                peakrpm + carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + carCompanyjaguar + carCompanymazda + 
                carCompanymitsubishi + carCompanynissan + 
                carCompanypeugeot + carCompanyplymouth + carCompanyporsche + 
                carCompanyrenault + carCompanysaab + carCompanysubaru + carCompanytoyota + 
                carCompanyvolkswagen + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd + enginetypeohc + 
                enginetyperotor, data = train)
summary(model11)

data.frame(vif(model11))

# remove carbody sedan, high p value and high VIF

model12 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + enginesize + stroke + 
                peakrpm + carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + carCompanyjaguar + carCompanymazda + 
                carCompanymitsubishi + carCompanynissan + 
                carCompanypeugeot + carCompanyplymouth + carCompanyporsche + 
                carCompanyrenault + carCompanysaab + carCompanysubaru + carCompanytoyota + 
                carCompanyvolkswagen + carbodyhatchback + 
                carbodyhardtop + carbodywagon + drivewheelrwd + enginetypeohc + 
                enginetyperotor, data = train)
summary(model12)

data.frame(vif(model12))

# Issue of high VIF in car body variables resolved, but carbody variables are now insignificant
# removing carbody hardtop

model13 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + enginesize + stroke + 
                peakrpm + carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + carCompanyjaguar + carCompanymazda + 
                carCompanymitsubishi + carCompanynissan + 
                carCompanypeugeot + carCompanyplymouth + carCompanyporsche + 
                carCompanyrenault + carCompanysaab + carCompanysubaru + carCompanytoyota + 
                carCompanyvolkswagen + carbodyhatchback + 
                carbodywagon + drivewheelrwd + enginetypeohc + 
                enginetyperotor, data = train)
summary(model13)

data.frame(vif(model13))

# both remaining carbody variables are insignificant
# removing carbody wagon

model14 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + enginesize + stroke + 
                peakrpm + carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + carCompanyjaguar + carCompanymazda + 
                carCompanymitsubishi + carCompanynissan + 
                carCompanypeugeot + carCompanyplymouth + carCompanyporsche + 
                carCompanyrenault + carCompanysaab + carCompanysubaru + carCompanytoyota + 
                carCompanyvolkswagen + carbodyhatchback + 
                + drivewheelrwd + enginetypeohc + 
                enginetyperotor, data = train)
summary(model14)

data.frame(vif(model14))

# removing the last carbody variable, carbody hatchback, insignificant

model15 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + enginesize + stroke + 
                peakrpm + carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + carCompanyjaguar + carCompanymazda + 
                carCompanymitsubishi + carCompanynissan + 
                carCompanypeugeot + carCompanyplymouth + carCompanyporsche + 
                carCompanyrenault + carCompanysaab + carCompanysubaru + carCompanytoyota + 
                carCompanyvolkswagen + 
                + drivewheelrwd + enginetypeohc + 
                enginetyperotor, data = train)
summary(model15)

data.frame(vif(model15))

# removing curbweight variable, high pvalue and high VIF

model16 <- lm(price ~ aspiration + enginelocation + carwidth + enginesize + stroke + 
                peakrpm + carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + carCompanyjaguar + carCompanymazda + 
                carCompanymitsubishi + carCompanynissan + 
                carCompanypeugeot + carCompanyplymouth + carCompanyporsche + 
                carCompanyrenault + carCompanysaab + carCompanysubaru + carCompanytoyota + 
                carCompanyvolkswagen + 
                + drivewheelrwd + enginetypeohc + 
                enginetyperotor, data = train)
summary(model16)

data.frame(vif(model16))

# removing carcompany saab, high p value

model17 <- lm(price ~ aspiration + enginelocation + carwidth + enginesize + stroke + 
                carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + carCompanyjaguar + carCompanymazda + 
                carCompanymitsubishi + carCompanynissan + 
                carCompanypeugeot + carCompanyplymouth + carCompanyporsche + 
                carCompanyrenault + carCompanysubaru + carCompanytoyota + 
                carCompanyvolkswagen + peakrpm
              + drivewheelrwd + enginetypeohc + 
                enginetyperotor, data = train)
summary(model17)

data.frame(vif(model17))

# removing engine type ohc, high p value

model18 <- lm(price ~ aspiration + enginelocation + carwidth + enginesize + stroke + 
                carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + carCompanyjaguar + carCompanymazda + 
                carCompanymitsubishi + carCompanynissan + 
                carCompanypeugeot + carCompanyplymouth + carCompanyporsche + 
                carCompanyrenault + carCompanysubaru + carCompanytoyota + 
                carCompanyvolkswagen + peakrpm
              + drivewheelrwd + 
                enginetyperotor, data = train)
summary(model18)

data.frame(vif(model18))

# removing carcompany peugeot, high p value

model19 <- lm(price ~ aspiration + enginelocation + carwidth + enginesize + stroke + 
                carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + carCompanyjaguar + carCompanymazda + 
                carCompanymitsubishi + carCompanynissan + 
                carCompanyplymouth + carCompanyporsche + 
                carCompanyrenault + carCompanysubaru + carCompanytoyota + 
                carCompanyvolkswagen + peakrpm
              + drivewheelrwd + 
                enginetyperotor, data = train)
summary(model19)

data.frame(vif(model19))

# removing carcompany porsche, high p value

model20 <- lm(price ~ aspiration + enginelocation + carwidth + enginesize + stroke + 
                carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + carCompanyjaguar + carCompanymazda + 
                carCompanymitsubishi + carCompanynissan + 
                carCompanyplymouth +  
                carCompanyrenault + carCompanysubaru + carCompanytoyota + 
                carCompanyvolkswagen + peakrpm
              + drivewheelrwd + 
                enginetyperotor, data = train)
summary(model20)

data.frame(vif(model20))

# removing carcompany volkswagen, high p value

model21 <- lm(price ~ aspiration + enginelocation + carwidth + enginesize + stroke + 
                carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + carCompanyjaguar + carCompanymazda + 
                carCompanymitsubishi + carCompanynissan + 
                carCompanyplymouth +  
                carCompanyrenault + carCompanysubaru + carCompanytoyota + 
                + peakrpm + drivewheelrwd + 
                enginetyperotor, data = train)
summary(model21)

data.frame(vif(model21))

# removing carcompany honda, high p value

model22 <- lm(price ~ aspiration + enginelocation + carwidth + enginesize + stroke + 
                carCompanybmw + carCompanybuick + carCompanydodge + carCompanyjaguar + carCompanymazda + 
                carCompanymitsubishi + carCompanynissan + 
                carCompanyplymouth +  
                carCompanyrenault + carCompanysubaru + carCompanytoyota + 
                + peakrpm + drivewheelrwd + 
                enginetyperotor, data = train)
summary(model22)

data.frame(vif(model22))

# removing carcompany renault, high p value

model23 <- lm(price ~ aspiration + enginelocation + carwidth + enginesize + stroke + 
                carCompanybmw + carCompanybuick + carCompanydodge + carCompanyjaguar + carCompanymazda + 
                carCompanymitsubishi + carCompanynissan + 
                carCompanyplymouth +carCompanysubaru + carCompanytoyota + 
                + peakrpm + drivewheelrwd + 
                enginetyperotor, data = train)
summary(model23)

data.frame(vif(model23))

# removing carcompany toyota, high p value

model24 <- lm(price ~ aspiration + enginelocation + carwidth + enginesize + stroke + 
                carCompanybmw + carCompanybuick + carCompanydodge + carCompanyjaguar + carCompanymazda + 
                carCompanymitsubishi + carCompanynissan + 
                carCompanyplymouth +carCompanysubaru + peakrpm + drivewheelrwd + 
                enginetyperotor, data = train)
summary(model24)

data.frame(vif(model24))

# removing carcompany nissan, high p value

model25 <- lm(price ~ aspiration + enginelocation + carwidth + enginesize + stroke + 
                carCompanybmw + carCompanybuick + carCompanydodge + carCompanyjaguar + carCompanymazda + 
                carCompanymitsubishi + carCompanyplymouth +carCompanysubaru + peakrpm + drivewheelrwd + 
                enginetyperotor, data = train)
summary(model25)

data.frame(vif(model25))

# removing carcompany mazda, high p value

model26 <- lm(price ~ aspiration + enginelocation + carwidth + enginesize + stroke + 
                carCompanybmw + carCompanybuick + carCompanydodge + carCompanyjaguar + 
                carCompanymitsubishi + carCompanyplymouth +carCompanysubaru + peakrpm + drivewheelrwd + 
                enginetyperotor, data = train)
summary(model26)

data.frame(vif(model26))

# removing carcompany plymouth, high p value

model27 <- lm(price ~ aspiration + enginelocation + carwidth + enginesize + stroke + 
                carCompanybmw + carCompanybuick + carCompanydodge + carCompanyjaguar + 
                carCompanymitsubishi + carCompanysubaru + peakrpm + drivewheelrwd + 
                enginetyperotor, data = train)
summary(model27)

data.frame(vif(model27))

# removing carcompany dodge, high p value

model28 <- lm(price ~ aspiration + enginelocation + carwidth + enginesize + stroke + 
                carCompanybmw + carCompanybuick + carCompanyjaguar + 
                carCompanymitsubishi + carCompanysubaru + peakrpm + drivewheelrwd + 
                enginetyperotor, data = train)
summary(model28)

data.frame(vif(model28))

# removing drivewheel rwd, high p value

model29 <- lm(price ~ aspiration + enginelocation + carwidth + enginesize + stroke + 
                carCompanybmw + carCompanybuick + carCompanyjaguar + 
                carCompanymitsubishi + carCompanysubaru + peakrpm +  
                enginetyperotor, data = train)
summary(model29)

data.frame(vif(model29))

# removing carcompany mitsubishi, high p value

model30 <- lm(price ~ aspiration + enginelocation + carwidth + enginesize + stroke + 
                carCompanybmw + carCompanybuick + carCompanyjaguar + 
                + carCompanysubaru + peakrpm +  
                enginetyperotor, data = train)
summary(model30)

data.frame(vif(model30))

# All variables are now highly significant, VIF values are also low

## Model evaluation

data.frame(names(test), 1:ncol(test)) # price is in column number 18
test$test_price <- predict(model30, test[ , -18]) # running the model on test dataset

rsquared <- cor(test$price,test$test_price)^2
rsquared

# Rsquared from test dataset is 0.84 and from training is 0.95, this is a reasonably accurate model

############################################## Conclusions ###########################################

# Model 30 predicts car price with sufficent accuracy, contains only highly significant and has little
# to no multicollinearity

# Key variables used for car price prediction
# 1. Engine location - cars with rear engines are significantly costlier
# 2. luxury car brand - bmw, buick and jaguar have significantly higher prices than other cars with same specs
# 3. engine stroke, aspiration and stroke are key engine parameters controlling price
