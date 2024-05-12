# 0. Feladat: Working directory:
setwd('C:/Users/Bálint/OneDrive/Dokumentumok/BCE 2023-24/Adatelemzés és vizualizáció R-ben/BEADANDÓ')

# 1. Feladat: Adatok beolvasása  + Hiányzó adatok kiszûrése + Változók átalakítása + Outlierszûrés + Leíró statisztikai elemzés:
utazas <- read.csv2("1. Utazás elõrejelzése.csv", sep = ',')
utazas <- as.data.frame(utazas) # Átalakítás Data Frame-mé.
str(utazas)
# View(utazas)

# Az adatbázis változók neveinek az átírása a könyebb érthetõség miatt:
colnames(utazas) <- c('Ugyfel_ID','Adasvetel','Eletkor', 'Kontakt_tipusa','Varos_fejlettsege', 'Pitch_hossza','Foglalkozas','Ugyfel_neme', 'Utazok_szama', 'Egyezetetesek_szama','Utazasi_csomag','Preferalt_szinvonal', 'Csaladi_allapot','Utazasok_szama','Utlevel','Pitch_ertekelese','Sajat_auto','Utazo_gyerekek_szama','Betoltott_pozicio','Havi_jovedelem')
str(utazas) # Szuper. 

# Kezdeti adatbázis kiválasztása (12 darab változó: 1 eredményváltozó - 11 magyarázóváltozó) :
utazas <- utazas[, c('Adasvetel', 'Eletkor', 'Kontakt_tipusa', 'Pitch_hossza', 'Ugyfel_neme', 'Utazok_szama', 'Egyezetetesek_szama', 
                 'Preferalt_szinvonal', 'Csaladi_allapot', 'Utazasok_szama', 'Pitch_ertekelese', 'Havi_jovedelem')]
# 11 magyarázóváltozó: 6 darab mennyiségi - 5 darab minõségi
# Kihagyott változók (8 darab): Ugyfel_ID, Varos_fejlettsege, Foglalkozas, Utazasi_csomag, Utlevel, Sajat_auto, Utazo_gyerekek_szama, Betoltott_pozicio
str(utazas)
View(utazas)

# Hiányzó adatok kiszûrése (NA-k kiszûrése):
sapply(utazas, function(x) sum(is.na(x))) # Hiányzó adatok kimutatása változónkként.
utazas <- utazas[complete.cases(utazas),] # Összes sor kiszûrése, ami tartalmaz NA-t.
sum(is.na(utazas)) # Ellenõrzés.
str(utazas)

# Szükséges változók átalakítása faktorrá + kategóriák magyar névvé alakítása (itt is ékezetek nélkül):
utazas [,c('Adasvetel','Kontakt_tipusa', 'Ugyfel_neme', 'Preferalt_szinvonal', 'Csaladi_allapot','Pitch_ertekelese')] <- lapply(utazas [,c('Adasvetel','Kontakt_tipusa', 'Ugyfel_neme', 'Preferalt_szinvonal', 'Csaladi_allapot','Pitch_ertekelese')], as.factor)
str(utazas)

levels(utazas$Kontakt_tipusa) # Viszonyítási alap megfelel.
utazas$Kontakt_tipusa <- factor(utazas$Kontakt_tipusa,
                             levels = c('Company Invited', 'Self Enquiry'),
                             labels = c('Iroda','Sajat'))

levels(utazas$Csaladi_allapot) # Viszonyítási alap az Egyedülálló legyen.
utazas$Csaladi_allapot <- factor(utazas$Csaladi_allapot,
                             levels = c('Divorced', 'Married','Single','Unmarried'),
                             labels = c('Elvalt','Hazas','Egyedulallo','Kapcsolatban'))
utazas$Csaladi_allapot <- relevel(utazas$Csaladi_allapot, ref = 'Egyedulallo') # Késõbbi használatra.

levels(utazas$Ugyfel_neme) # Viszonyítási alap megfelel.
utazas$Ugyfel_neme <- factor(utazas$Ugyfel_neme,
                      levels = c('Fe Male', 'Female', 'Male'),
                      labels = c('No', 'No', 'Ferfi')) # Az adatbázisban a Female elírás miatt kétszer szerepelt.
str(utazas) # Ellenõrzés.

# Outlierszûrés :
# Az adatok kiszûrésénél maximum 1% az elfogadható:
4194*0.01 # =41,94, ami azt jelenti, hogy maximum 41-42 adatot szûrhetünk/dobhatunk ki.

# Outlierek keresése :
summary(utazas)
boxplot(utazas$Eletkor) # Nincsen outlier.
boxplot(utazas$Pitch_hossza) # Van 1 outlier, 120-nál kisebb értéket kell meghatározni, hogy kiszûrjük.
boxplot(utazas$Utazok_szama) # Nincsen outlier.
boxplot(utazas$Egyezetetesek_szama) # Nincsen outlier.
boxplot(utazas$Utazasok_szama) # Van 4 outlier, 15-nél kisebb értéket kell meghatározni, hogy kiszûrjük.
boxplot(utazas$Havi_jovedelem) # Van 3 outlier (1 felül, 2 alul), ezek pontos értékei nem láthatóak. Megnézzük head és tail fügvénnyel.
tail(sort(utazas$Havi_jovedelem), 30) # 40000 fölött 1 outlier, 40000-nél kisebb értéket kell meghatározni, hogy kiszûrjük.
head(sort(utazas$Havi_jovedelem), 30) # 5000 alatt 2 outlier, 5000-nél nagyobb értéket kell meghatározni, hogy kiszûrjük.

# Outlierek kiszûrése
utazas <- utazas[utazas$Pitch_hossza < 120, ]
utazas <- utazas[utazas$Utazasok_szama < 15, ]
utazas <- utazas[utazas$Havi_jovedelem < 40000 & utazas$Havi_jovedelem > 5000, ]
summary(utazas) # Ellenõrzés.
# A kezdeti 4194-ról 4186-ra csökkentettük le a megfigyelések számát, ezzel 8 megfigyelést szûrtünk le/dobtunk ki. 
# Ez a 8 megfigyelés a 41-42-es megengedhetõ tûréshatáron belül van.
4186/4194 # Ez azt jelenti, hogy 99,8%-át megtartottuk az adatoknak. Ez szuper és elfogadható.

# Leíró statisztikai elemzés:
library(psych)
str(utazas)
describe(utazas[c('Eletkor', 'Pitch_hossza', 'Utazok_szama', 'Egyezetetesek_szama', 'Utazasok_szama', 'Havi_jovedelem' )])
summary(utazas[c('Eletkor', 'Pitch_hossza', 'Utazok_szama', 'Egyezetetesek_szama', 'Utazasok_szama', 'Havi_jovedelem' )])

# 2. Feladat: Egyváltozós + Kátváltozós + Többváltozós adatvizualizáció:
### Eredményváltozó és magyarázó változók páronkénti kapcsolatai 
# Odds adásvétel = P (megvette)/ 1-P (nem vette meg)
# az adásvétel esélyét növelõ magyarázóváltozók esetén pozitív, az azt csökkentõk esetén negatív bétát várunk

# Egyváltozós vizualizáció:
# Minõségi változók: Oszlopdiagrammok
library(ggplot2)
str(utazas)
# Adásvétel:
a <- barplot(prop.table(table(utazas$Adasvetel)),
             ylim = c(0,1),
             main = 'Adásvétel megoszlása',
             ylab = 'Relatív gyakoriság',
             col = 'pink3')
text(a, prop.table(table(utazas$Adasvetel)),
     table(utazas$Adasvetel), pos = 3)

# Kontakt típusa:
b <- barplot(prop.table(table(utazas$Kontakt_tipusa)),
             ylim = c(0,1),
             main = 'Kontakt típásának megoszlása ',
             ylab = 'Relatív gyakoriság',
             col = 'pink3')
text(a, prop.table(table(utazas$Kontakt_tipusa)),
     table(utazas$Kontakt_tipusa), pos = 3)

# Ügyfél neme:
c <- barplot(prop.table(table(utazas$Ugyfel_neme)),
             ylim = c(0,1),
             main = 'Ügyfél nemek megoszlása',
             ylab = 'Relatív gyakoriság',
             col = 'pink3')
text(c, prop.table(table(utazas$Ugyfel_neme)),
     table(utazas$Ugyfel_neme), pos = 3)

# Preferált színvonal:
d <- barplot(prop.table(table(utazas$Preferalt_szinvonal)),
             ylim = c(0,1),
             main = 'Preferált színvonal megoszlása',
             ylab = 'Relatív gyakoriság',
             col = 'pink3')
text(d, prop.table(table(utazas$Preferalt_szinvonal)),
     table(utazas$Preferalt_szinvonal), pos = 3)

# Családi állapot:
e <- barplot(prop.table(table(utazas$Csaladi_allapot)),
             ylim = c(0,1),
             main = 'Családi állapot megoszlása',
             ylab = 'Relatív gyakoriság',
             col = 'pink3')
text(e, prop.table(table(utazas$Csaladi_allapot)),
     table(utazas$Csaladi_allapot), pos = 3)

# Pitch értékelése:
f <- barplot(prop.table(table(utazas$Pitch_ertekelese)),
             ylim = c(0,1),
             main = 'Pitch értékelések megoszlása',
             ylab = 'Relatív gyakoriság',
             col = 'pink3')
text(f, prop.table(table(utazas$Pitch_ertekelese)),
     table(utazas$Pitch_ertekelese), pos = 3)

# Mennyiségi változók: Hisztogrammok
# Életkor
str(utazas)
hist(utazas$Eletkor)
ggplot(utazas, aes(x=Eletkor)) + geom_histogram(color="white", fill="pink3", binwidth = 5) +
  scale_x_continuous(name = 'Életkor [Év]') +
  scale_y_continuous(name = 'Gyakoriság [Fõ]') +
  ggtitle('Életkor megoszlása')

# Pitch hossza:
hist(utazas$Pitch_hossza)
ggplot(utazas, aes(x=Pitch_hossza)) + geom_histogram(color="white", fill="pink3", binwidth = 1.9) +
  scale_x_continuous(name = 'Pitch hossza [Perc]') +
  scale_y_continuous(name = 'Gyakoriság') +
  ggtitle('Pitch hosszának a megoszlása')

# Utazók száma:
hist(utazas$Utazok_szama)
ggplot(utazas, aes(x=Utazok_szama)) + geom_histogram(color="white", fill="pink3", binwidth = 1) +
  scale_x_continuous(name = 'Utazók száma [Fõ]') +
  scale_y_continuous(name = 'Gyakoriság') +
  ggtitle('Utazók számának a megoszlása')

# Egyeztetések száma:
hist(utazas$Egyezetetesek_szama)
ggplot(utazas, aes(x=Egyezetetesek_szama)) + geom_histogram(color="white", fill="pink3", binwidth = 1) +
  scale_x_continuous(name = 'Egyeztetések száma [Darab]') +
  scale_y_continuous(name = 'Gyakoriság') +
  ggtitle('Egyeztetések számának a megoszlása')

# Utazások száma:
hist(utazas$Utazasok_szama)
ggplot(utazas, aes(x=Utazasok_szama)) + geom_histogram(color="white", fill="pink3", binwidth = 1) +
  scale_x_continuous(name = 'Utazások száma [Darab]') +
  scale_y_continuous(name = 'Gyakoriság') +
  ggtitle('Átlagos évi utazásszám megoszlása')

# Havi jövedelem:
hist(utazas$Havi_jovedelem)
ggplot(utazas, aes(x=Havi_jovedelem)) + geom_histogram(color="white", fill="pink3", binwidth = 1910) +
  scale_x_continuous(name = 'Havi jövedelem [USD]') +
  scale_y_continuous(name = 'Gyakoriság') + 
  ggtitle('Havi jövedelem megoszlása')

# Kétváltozós vizualizáció:
# Kereszttábla a kategória típusú változókra
round(prop.table(table(utazas$Preferalt_szinvonal, utazas$Adasvetel), margin=1), 3)
# 3-4 csillagosok azonos arányban kerültek megvételre, 5 csillagosok nagyobb arányban.
# Pozitív bétát várunk, referencia kategória a 3 csillag.
round(prop.table(table(utazas$Pitch_ertekelese, utazas$Adasvetel), margin=1), 3)
round(prop.table(table(utazas$Pitch_ertekelese)), 3)
# 3-ast adnak leggyakrabban, azért mert ez a semleges értékelés.
# 4-est kevesebben adnak, mert ha valami tetszik, akkor vagy 5-öst adsz vagy semlegesre értékeled.
# Nem teljesen konzisztensen nõ a siker aránya a magasabb értékelést kapott pitchek között.
# Enyhén pozitív bétát várunk vagy nem szignifikáns változót, referencia kategória az 1-es.
# Ez jól mutatja ezt.
round(prop.table(table(utazas$Ugyfel_neme, utazas$Adasvetel), margin=1), 3)
# Nincs jelentõs eltérés, hasonló arányban utasították el illetve vették meg az utat a nõk és férfiak.
# Nem biztos, hogy szignifikáns lesz a változó.
round(prop.table(table(utazas$Csaladi_allapot, utazas$Adasvetel), margin=1), 3)
# Legmagasabb arányban sikeres csoport a Egyedulallo volt, mivel ez a referencia kategória, így negatív bétát várunk.
round(prop.table(table(utazas$Kontakt_tipusa, utazas$Adasvetel), margin=1), 3)
# Iroda nagyobb arányban sikeres, ez a referencia kategória -> negatív bétát várunk.
# Ha magadtól mész van már egy preferenciád, ha ezt nem sikerül megtalálni nem fogadod el, veszed meg. Ha neked pitchelnek, akkor lehet megtetszik és megveszed úgy, hogy nem is terveztél utazni.

# Páronkénti dobozábra a vegyes kapcsolatok szemléltetésére
boxplot(Eletkor~Adasvetel, data=utazas)
# Sikeres kimenetel inkább a fiatalabb korosztályból került ki, idõsebbek inkább elálltak.
# Negatív bétát várunk.
# Ggplot-tal:
ggplot(utazas, aes(x = Adasvetel, y = Eletkor)) + 
  geom_boxplot(color="black", fill="pink3") +
  xlab('Adásvétel') +
  ylab('Életkor [Év]') +
  ggtitle('Életkor és Adásvétel kapcsolata')
  
boxplot(Pitch_hossza ~ Adasvetel, data=utazas)
# Sikeres kimenetelt eredményeztek a hosszabb pitchek, sikeres pitchek hosszának a mediánja magasabb némileg mint a nem sikereseké.
# Enyhén pozitív bétát várunk.
# Ggplot-tal:
ggplot(utazas, aes(x = Adasvetel, y = Pitch_hossza)) + 
  geom_boxplot(color="black", fill="pink3") +
  xlab('Adásvétel') +
  ylab('Pitch hossza [Perc]') +
  ggtitle('Pitch hossza és Adásvétel kapcsolata')

boxplot(Egyezetetesek_szama ~ Adasvetel, data=utazas)
# Volt aki 1 egyeztetés után rögtön elutasította az ajánlatot és volt aki 6 után lépett vissza.
# Medián mindkét esetben 4.
# Enyhén pozitív bétát várunk.
# Ggplot-tal:
ggplot(utazas, aes(x = Adasvetel, y = Egyezetetesek_szama)) + 
  geom_boxplot(color="black", fill="pink3") +
  xlab('Adásvétel') +
  ylab('Egyeztetések száma') +
  ggtitle('Egyeztetések száma és Adásvétel kapcsolata')

boxplot(Utazasok_szama ~ Adasvetel, data=utazas)
# Nem biztos, hogy ez a változó szignifikáns lesz.
# Ggplot-tal:
ggplot(utazas, aes(x = Adasvetel, y = Utazasok_szama)) + 
  geom_boxplot(color="black", fill="pink3") +
  xlab('Adásvétel') +
  ylab('Utazások száma') +
  ggtitle('Utazások száma és Adásvétel kapcsolata')

boxplot(Havi_jovedelem ~ Adasvetel, data=utazas)
# A vevõk között kisebb a mediánbér mint az elutasítók között, ez érdekes.
# Enyhén negatív bétát várunk.
# Ez összekapcsolható az Eletkor-Adasvetel boxplottal, aki fiatalabb inkább elfogadta és a korrelációs mátrixból látszik, h 0,4-el korrelál egymással.
# EZ logikus is, aki fiatalabb, annak még kisebb a jövedelme. Kevesebb tapasztalat stb...
# Ggplot-tal:
ggplot(utazas, aes(x = Adasvetel, y = Havi_jovedelem)) + 
  geom_boxplot(color="black", fill="pink3") +
  xlab('Adásvétel') +
  ylab('Havi jövedelem (USD)') +
  ggtitle('Havi jövedelem és Adásvétel kapcsolata')

boxplot(Utazok_szama ~ Adasvetel, data=utazas)
# Nem biztos, hogy ez a változó szignifikáns lesz.
# Ggplot-tal:
ggplot(utazas, aes(x = Adasvetel, y = Utazok_szama)) + 
  geom_boxplot(color="black", fill="pink3") +
  xlab('Adásvétel') +
  ylab('Utazók száma') +
  ggtitle('Utazók száma és Adásvétel kapcsolata')

# Multikollinearitás esetleges elõrejelzése a magyarázóváltozók között:
# I. Családi állapot és együtt utazók száma:
round(prop.table(table(utazas$Csaladi_allapot, utazas$Utazok_szama),margin=1),3)
# Nincs lényegi különbség a csoportok között.
by(utazas[, "Utazok_szama"], utazas$Csaladi_allapot, summary)
# Minden csoportnál 3 fõ együtt utazó a medián.
ggplot(utazas, aes(x = Csaladi_allapot, y = Utazok_szama)) + 
  geom_boxplot(color="black", fill="pink3") +
  xlab('Családi állapot') +
  ylab('Utazók száma') +
  ggtitle('Utazók száma és Családi állapot kapcsolata')

# II. Pitch elégedettség és egyeztetések száma:
round(prop.table(table(utazas$Pitch_ertekelese, utazas$Egyezetetesek_szama),margin=1),3)
by(utazas[, "Egyezetetesek_szama"], utazas$Pitch_ertekelese, summary)
# Nincs különbség.
ggplot(utazas, aes(x =Pitch_ertekelese, y = Egyezetetesek_szama)) + 
  geom_boxplot(color="black", fill="pink3") +
  xlab('Pitch értékelések') +
  ylab('Egyeztetések száma') +
  ggtitle('Egyeztetések száma és Pitch értékelések kapcsolata')

# III. Preferált minõség és havi jövedelem:
boxplot(utazas$Havi_jovedelem~utazas$Preferalt_szinvonal)
# Nagyon egyenlõek az eloszlások, úgy látszik, nem befolyásolta a jövedelem a minõséget illetõ választást.
by(utazas[, "Havi_jovedelem"], utazas$Preferalt_szinvonal, summary)
ggplot(utazas, aes(x =Preferalt_szinvonal, y = Havi_jovedelem)) + 
  geom_boxplot(color="black", fill="pink3") +
  xlab('Preferált színvonal') +
  ylab('Havi jövedelem') +
  ggtitle('Havi jövedelem és Preferált színvonal kapcsolata')

# IV. Elégedettség és pitch hossza:
boxplot(utazas$Pitch_hossza~utazas$Pitch_ertekelese)
by(utazas[, "Pitch_hossza"], utazas$Pitch_ertekelese, summary)
# Szintén nincs nagy eltérés.
ggplot(utazas, aes(x =Pitch_ertekelese, y = Pitch_hossza)) + 
  geom_boxplot(color="black", fill="pink3") +
  xlab('Pitch értékelése') +
  ylab('Pitch hossza') +
  ggtitle('Pitch hossza és Pitch értékelés kapcsolata')

# Többváltozós vizualizáció:
# Korrelációs mátrix a mennyiségi változókra:
korrelacios_matrix <- round(cor(utazas[ ,c( "Pitch_hossza", "Egyezetetesek_szama","Utazasok_szama", "Havi_jovedelem", "Eletkor", "Utazok_szama")]), 3)
library(corrplot)
corrplot(korrelacios_matrix, method= "circle")
# Életkor és havi jövedelem korrelál leginkább, pozitívan kb 0,4-es erõsségû.
# Többi magyarázó változó között legfeljebb 0,2-es korreláció van -> nem számítunk magas vif mutatóra.
# install.packages('ggcorrplot')
library(ggcorrplot)
ggcorrplot(korrelacios_matrix) 
ggcorrplot(korrelacios_matrix, method = 'circle') +
  ggtitle('Korrelációs mátrix a mennyiségi változókra')

# 3. Feladat:  Logisztikus regressziós modell + modellszelekció:
# Logisztikus regresszió futtatása az összes változóval:
modell_alap <- glm(Adasvetel ~ ., data = utazas, family = binomial(link = 'logit'))
summary(modell_alap)

# Multikollinearitás:
# install.packages('car')
library(car)
vif(modell_alap) # Nincs multikollinearitás. Igazolja a sejtésünk.

# Kézi és gépi modellszelekció:
# Kézi modellszelekció:
BetaTabla <- summary(modell_alap)$coefficients
BetaTabla[order(BetaTabla[,4]),] 
# Az legutolsó 3 változót szedtük ki. Ezek a következõ változók: Pitch_ertekelese, Utazok_szama, Ugyfel_neme
# Erre készítettünk egy modellt:
utazas_szukitett_kezi <- utazas[, c('Adasvetel', 'Eletkor', 'Kontakt_tipusa', 'Pitch_hossza', 'Preferalt_szinvonal', 'Egyezetetesek_szama', 
                'Csaladi_allapot', 'Utazasok_szama', 'Havi_jovedelem')]

modell_szukitett_kezi <- glm(Adasvetel ~ ., data = utazas_szukitett_kezi, family = binomial(link = 'logit'))
summary(modell_szukitett_kezi)
# Minden szokásos szignifikanciaszinten szignifikánsak a változóink. (Alfa =1%, 5% és 10% is egyaránt.)

# Gépi modellszelekció (Stepwise): 
library(MASS)
# A full modell az nekünk az a model_alap, ezt nem nevezzük át. Ezzel dolgozunk a backward eljárásnál.
step(object = modell_alap, direction = 'backward', k = log(4186))
# A gép 4 változót dobna ki: Pitch_ertekelese, Utazok_szama, Ugyfel_neme, Utazasok_szama
# Az elsõ 3 kidobott változót mi is kidobnánk, de a gép még egy negyediket is kidobna ezek után.
# Nézzük meg számokban a modellt.

utazas_szukitett_gepi <- utazas[, c('Adasvetel', 'Eletkor', 'Kontakt_tipusa', 'Pitch_hossza', 'Preferalt_szinvonal', 'Egyezetetesek_szama', 
                           'Csaladi_allapot', 'Havi_jovedelem')]
modell_szukitett_gepi <- glm(Adasvetel ~ ., data = utazas_szukitett_gepi, family = binomial(link = 'logit'))
summary(modell_szukitett_gepi)

# A 3 modell összehasonlítása:
# AIC, BIC alapján:
AIC(modell_alap, modell_szukitett_kezi, modell_szukitett_gepi)
BIC(modell_alap, modell_szukitett_kezi, modell_szukitett_gepi)
# AIC alapján a sorrend: modell_alap > modell_szukitett_kezi > modell_szukitett_gepi
# BIC alapján a sorrend: modell_alap < modell_szukitett_kezi < modell_szukitett_gepi

# McFadden féle Pszeudo R^2 alapján:
# install.packages('DescTools')
library(DescTools)
PR2_modell_alap <- PseudoR2(modell_alap, which = "McFadden")
PR2_modell_szukitett_kezi <- PseudoR2(modell_szukitett_kezi, which = "McFadden")
PR2_modell_szukitett_gepi <- PseudoR2(modell_szukitett_gepi, which = "McFadden")
(PR2_modell_alap - PR2_modell_szukitett_kezi)*100 # 0,54%-ponttal csökkent az R2 az nagyon kevés.
# Összegzés:
# Mi ezeknek ellenére is a modell__szukitett_kezi -vel akarunk továbbmenni. 
# Oka: A gépi szelekciónál a negyedik változó elhagyása (Utazasok_szama) egy minden szignifikanciaszinten is szignifikáns változó.
# A BIC is mutatja, hogy nagyon minimális a különbség, ha ezt a változót kiszedjük. Ezek okán mi szeretnénk megtartani a Utazasok_szama változót.
# Szóval a modell_szukitett_kezi lesz az, amivel dolgozni szeretnénk. Legyen ennek a neve modell_vegleges
modell_vegleges <- modell_szukitett_kezi
utazas_vegleges <- utazas_szukitett_kezi
PR2_modell_vegleges <- PR2_modell_szukitett_kezi

# Végsõ modell:
summary(modell_vegleges)

# 4. Feladat: McFadden-féle pszeudo R-négyzet + Globális Khi-négyzet próba  + VIF mutató
# Mcfadden:
PR2_modell_vegleges
PR2_modell_alap

# Ez egy éppenhogy közepes magyarázóerõ, ami összecseng a ROC görbe alatti terület méretével.
deviance(modell_alap) - deviance(modell_vegleges)
anova(modell_alap, modell_vegleges)

# Hipotézisvizsgálat: GOF teszt- Goodness of Fit teszt
# H0: B1=B2=B3...Bk == 0 , azaz a sokaságban összeomlik a minta magyarázóereje.
# H1: van legalább egy Bj, ami nem 0, azaz szignifikáns. Ez azt jelenti, hogy kiterjeszthetõ a modell a sokaságra (a valóságra).
summary(modell_vegleges)
# Nulldeviancia: 4089.9, ez azt jelenti, hogy ez az üres modellnek a devianciája. A tökéletes modelltõl való eltérés. A tökéletes 0.
# Reziduális deviancia(saját modellünknek): 3654.2, ez is a tökéletes modelltól a 0-tól való eltérés.
# 8 magyarázóváltozóval 4089.9-rõl 3654.2-re csökkentettük a devianciát.
# R^2 manuális kiszámolása: (4089.9-3654.2)/4089.9 = 0,10655343
str(utazas_vegleges)
(4089.9-3654.2)/4089.9
PR2_modell_vegleges # Ellenõrzés, valóban annyi. Szuper.
# Az üres és a tökéletes modell közötti út 10,65%-át tettük meg.

# Khi négyzet próbafüggvény értéke:
4089.9-3654.2 # 435,7
# Szabadságfokok száma = df = 8
# Ez egy jobboldali próba, tehát a 435,7 felé esési valószínûséget keressük. 
# 435,7: ennyi devianciát magyaráz meg a modell az üreshez képest.
1- pchisq(435.7, df =8) # A p-érték 0, ez szuper. A H0-t elutasítjuk. Az aktuális modell ellett döntünk. 
# Ami azt jelenti, hogy releváns a modell a sokaságban is.

# Multikollinearitás
vif(modell_vegleges)
# VIF értékek 5 alatt vannak bõven, sõt az összes 2 alatt, szóval szó sincs multikollinearitásról.

# 5. Feladat: Klasszifikációs mátrix + ROC-görbe:
# Klasszifikációs mátrix
prop.table(table(utazas$Adasvetel))
# Az összes utazási értékesítés 19%-a volt sikeres.
# 1: sikeres értékesítés, megvették az utazást, 0: nem sikeres értékesítés, nem vettéj meg az utazást

### Eredményváltozó és magyarázó változók páronkénti kapcsolatai ###
# Odds adásvétel = P (megvette)/ 1-P (nem vette meg)

# LOGIT oszlop hozzáadása: link=ln odds adásvétel = alfa + béta*x minden megfigyelésre.
utazas_vegleges$pred_link <- predict.glm(object = modell_vegleges, newdata = utazas_vegleges, type = 'link')
head(utazas_vegleges) 

# Adásvétel valószínûség oszlop hozzáadása: Adásvétel valószinûség minden valószínûségre
utazas_vegleges$pred_resp <- predict.glm(object = modell_vegleges, newdata = utazas_vegleges, type = 'response')
head(utazas_vegleges)

# Adásvétel valószinûsége minden egyes megfigyelés esetén.
hist(utazas_vegleges$pred_resp)
ggplot(utazas_vegleges, aes(x=pred_resp)) + geom_histogram(color="white", fill="pink3", binwidth = 0.065) +
  scale_x_continuous(name = 'Adásvétel elõrejelzett valószínûség') +
  scale_y_continuous(name = 'Gyakoriság') +
  ggtitle('Adásvétel elõrejelzett valõszínûségének eloszlása')

# Klasszifikációs mátrixok:
utazas_vegleges$prediction50 <- as.factor(as.numeric(utazas_vegleges$pred_resp > 0.5))
table(utazas_vegleges$prediction50) # Csak elõnézet. A modellünk 159 sikeres adásvételt jelez elõre.

# Klasszifikációs mátrix 50%-os Cut-value érték mellett:
# y = 1, úgynevezett pozitív osztály
table(utazas_vegleges$Adasvetel, utazas_vegleges$prediction50)
# Accuracy(Pontosság)(1): Helyes találat/összes találatok aránya: 
Accuracy50 <- (3338+113)/4186 # 82,4% a Pontosság aránya

# Recall(1): Ami a valóságban adásvétel volt, azok közül mennyit prediktáltam adásvételnek. A helyesen sikeresen jelzettek aránya, a valóban sikeresek közül. Ez valójában a TPR.
Recall50 <- 113/(113+689) # 14,1%

# Precision(1): Helyesen sikeresnek jelzettek aránya, az összes sikeresnek közül
Precision50 <- 113/(113+46) # 71,1%

# TPR(1): Ugyanaz, mint a Recall(1)
TPR50 <- 113/(113+689) # 14,1%

# FPR(1): Valójában nem sikeres adásvételek közül a modell mennyit azonosított sikeresnek.
FPR50 <- 46/(46+3338) # 1,36%

# FNR (1): Valójában sikeres adásvételek közül a modell mennyit jelzett helytelenül sikertelennek. 1-TPR
FNR50 <- 689/(689+113) # 85,9%

# TNR(1): 1- FPR, A sikertelen adásvételek hány százalékát jelezte elõre sikertelennek. A valóban sikertelen adásvételek közül menyiit azonosított a modell helyesen sikertelennek.
TNR50 <- 3338/(3338+46) # 98,64%

table(utazas_vegleges$Adasvetel)
802/4186 # Ellenõrzés, valóban ~ 19,2%
utazas_vegleges$prediction19.2 <- as.factor(as.numeric(utazas_vegleges$pred_resp > 802/4186))
table(utazas_vegleges$prediction19.2)
# A modellunk 1641 sikeres adásvételt jelez elõre.

# Klasszifikációs mátrix 19,2%-os Cut-value érték mellett: Ez a sikeres adásvételek aránya a mintán belül
# y = 1, úgynevezett pozitív osztály
table(utazas_vegleges$Adasvetel, utazas_vegleges$prediction19.2)
# Accuracy(Pontosság)(1): Helyes találat/összes találatok aránya: 
Accuracy19.2 <- (2254+511)/4186 # 66,1% a pontosság.

# Recall(1): Ami a valóságban adásvétel volt, azok közül mennyit prediktáltam adásvételnek. A helyesen sikeresen jelzettek aránya, a valóban sikeresek közül. Ez valójában a TPR.
Recall19.2 <- 511/(511+291) # 63,7%

# Precision(1): Helyesen sikeresnek jelzettek aránya, az összes sikeresnek közül
Precision19.2 <- 511/(511+1130) # 31,14%

# TPR(1): Ugyanaz, mint a Recall(1)
TPR19.2 <- 511/(511+291) # 63,7%

# FPR(1): Valójában nem sikeres adásvételek közül a modell mennyit azonosított sikeresnek.
FPR19.2 <- 1130/(1130+2254) # 33,4%

# FNR (1): Valójában sikeres adásvételek közül a modell mennyit jelzett helytelenül sikertelennek. 1-TPR
FNR19.2 <- 291/(291+511) # 36,3%

# TNR(1): 1- FPR, A sikertelen adásvételek hány százalékát jelezte elõre sikertelennek. A valóban sikertelen adásvételek közül menyiit azonosított a modell helyesen sikertelennek.
TNR19.2 <- 2254/(2254+1130) # 66,6%

# ROC-görbe:
# install.packages('pROC')
library(pROC)
roc_utazas <- roc(utazas_vegleges$Adasvetel, utazas_vegleges$pred_resp)
plot(roc_utazas)
auc(roc_utazas) # 0,7122. Tökélets a modell, ha ez az érték 1 lenne. Nem véletlenszerûen tippelget, de nem is tökéletes a modell, valahol középtávon, félúton van.
auc(roc_utazas)/0.5 # 42%-ban jobb, mint a vaktában találgatás.

# Ellenõrzések:
which.min(abs(roc_utazas$thresholds- 802/4186))
roc_utazas$thresholds[2185]
roc_utazas$sensitivities[2185] 
roc_utazas$specificities[2185] 

which.min(abs(roc_utazas$thresholds-0.5))
roc_utazas$thresholds[3295] # Valóban annyi.
roc_utazas$sensitivities[3295]
TPR50 # Valóban annyi.
roc_utazas$specificities[3295]
TNR50 # Valóban annyi.

# Legjobb:
coords(roc_utazas, "best", ret = "threshold", best.method = "closest.topleft")
which.min(abs(roc_utazas$thresholds-0.1876846))
roc_utazas$thresholds[2139]
roc_utazas$sensitivities[2139] # TPR, ez a ROC görbe y-tengelye
roc_utazas$specificities[2139] # 
FPR_rocoptim <- 1-(roc_utazas$specificities[2139])
FPR_rocoptim

# 6. Feladat: Regressziós egyenlet + Értelmezés:
# Regressziós egyenlet:
summary(modell_vegleges)
BetaTabla <- summary(modell_vegleges)$coefficients
BetaTabla
# A regressziós egyenlet és értelmezések a Word-dokumentumban.

# 7. Feladat: Elõrejelzés a modellel
# install.packages('margins')
library(margins)
minta_ugyfel_1 <- data.frame(
  Eletkor = 25,
  Kontakt_tipusa = 'Sajat',
  Pitch_hossza =9,
  Preferalt_szinvonal = '3',
  Egyezetetesek_szama = 3,
  Csaladi_allapot = 'Kapcsolatban',
  Utazasok_szama = 3,
  Havi_jovedelem = 20000
)
predict.glm(modell_vegleges, newdata = minta_ugyfel_1,type = 'response')
# Ha egy ügyfél ezekkel az adatokkal rendelkezik (wordben írjátok le.), akkor az adásvétel oddsa várhatóan 18,58%.

minta_ugyfel_2 <- data.frame(
  Eletkor = 23,
  Kontakt_tipusa = 'Iroda',
  Pitch_hossza =12,
  Preferalt_szinvonal = '4',
  Egyezetetesek_szama = 4,
  Csaladi_allapot = 'Egyedulallo',
  Utazasok_szama = 4,
  Havi_jovedelem = 19000
)
predict.glm(modell_vegleges, newdata = minta_ugyfel_2,type = 'response')
# Most itt az volt a cél, hogy az változók értékeit úgy változtassuk, hogy annak odds-növelõ hatása legyen.
# Ez sikerült is, az ügyfélnek 55,1%-os lett a becsült adásvétel oddsa.

minta_ugyfel_y <- data.frame(
  Eletkor = 49,
  Kontakt_tipusa = 'Iroda',
  Pitch_hossza =14,
  Preferalt_szinvonal = '4',
  Egyezetetesek_szama = 4,
  Csaladi_allapot = 'Elvalt',
  Utazasok_szama = 2,
  Havi_jovedelem = 20130
)
predict.glm(modell_vegleges, newdata = minta_ugyfel_y,type = 'response')
# Ezzel elõrejelezzük az y-kalapot egy ilyen tulajdonságú ügyfélnél.
# Ez csak egy ellenõrzés volt a táblázat második ügyfelére. 
head(utazas_vegleges)

### VÉGE ### 
##################################################################################################################################################################################################################################
### VÉGE ###



# R outputok, amelyeket megjelenítünk a dokumentumban (INNENTÕL MÁR NEM ÉRDEMES ELOLVASNI/LEFUTTATNI):
library(writexl)

# Alap leíró statisztika:
Output1 <- describe(utazas[c('Eletkor', 'Pitch_hossza', 'Utazok_szama', 'Egyezetetesek_szama', 'Utazasok_szama', 'Havi_jovedelem' )])
Output2 <- as.data.frame(summary(utazas[c('Eletkor', 'Pitch_hossza', 'Utazok_szama', 'Egyezetetesek_szama', 'Utazasok_szama', 'Havi_jovedelem' )]))
write_xlsx(Output1, 'describe.xlsx')
write_xlsx(Output2, 'summary.xlsx')

# Alap_modell:
Output3 <- as.data.frame(summary(modell_alap)$coefficients)
write_xlsx(Output3, 'modell_alap.xlsx')

# Kézi, Manuális modell:
BetaTabla <- summary(modell_alap)$coefficients
Output4 <- as.data.frame(BetaTabla[order(BetaTabla[,4]),])
write_xlsx(Output4, 'BetaTablaSorrend.xlsx')

# Modellek összehasonlítása:
Output5.1 <- AIC(modell_alap, modell_szukitett_kezi, modell_szukitett_gepi)
Output5.2 <- BIC(modell_alap, modell_szukitett_kezi, modell_szukitett_gepi)
Output5 <- as.data.frame(cbind(Output5.1, BIC = Output5.2[,2]))
write_xlsx(Output5,'ModellOsszehasonlitas.xlsx')

# Végsõ modell:
Output6 <- as.data.frame(summary(modell_vegleges)$coefficients)
write_xlsx(Output6, 'VegsoModell.xlsx')

# Klasszifikációs mátrix:
Output7 <- as.data.frame(table(utazas_vegleges$Adasvetel, utazas_vegleges$prediction50))
write_xlsx(Output7, 'C50.xlsx')
Output8 <- as.data.frame(table(utazas_vegleges$Adasvetel, utazas_vegleges$prediction19.2))
write_xlsx(Output8, 'C19.2.xlsx')

# Regressziós egyenlet:
Output9 <- as.data.frame(exp(BetaTabla[,1]))
Output9$Variables <- row.names(BetaTabla)
write_xlsx(Output9, 'RegressziosEgyenlet.xlsx')

