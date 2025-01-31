# 0. Feladat: Working directory:
setwd('C:/Users/B�lint/OneDrive/Dokumentumok/BCE 2023-24/Adatelemz�s �s vizualiz�ci� R-ben/BEADAND�')

# 1. Feladat: Adatok beolvas�sa  + Hi�nyz� adatok kisz�r�se + V�ltoz�k �talak�t�sa + Outliersz�r�s + Le�r� statisztikai elemz�s:
utazas <- read.csv2("1. Utaz�s el�rejelz�se.csv", sep = ',')
utazas <- as.data.frame(utazas) # �talak�t�s Data Frame-m�.
str(utazas)
# View(utazas)

# Az adatb�zis v�ltoz�k neveinek az �t�r�sa a k�nyebb �rthet�s�g miatt:
colnames(utazas) <- c('Ugyfel_ID','Adasvetel','Eletkor', 'Kontakt_tipusa','Varos_fejlettsege', 'Pitch_hossza','Foglalkozas','Ugyfel_neme', 'Utazok_szama', 'Egyezetetesek_szama','Utazasi_csomag','Preferalt_szinvonal', 'Csaladi_allapot','Utazasok_szama','Utlevel','Pitch_ertekelese','Sajat_auto','Utazo_gyerekek_szama','Betoltott_pozicio','Havi_jovedelem')
str(utazas) # Szuper. 

# Kezdeti adatb�zis kiv�laszt�sa (12 darab v�ltoz�: 1 eredm�nyv�ltoz� - 11 magyar�z�v�ltoz�) :
utazas <- utazas[, c('Adasvetel', 'Eletkor', 'Kontakt_tipusa', 'Pitch_hossza', 'Ugyfel_neme', 'Utazok_szama', 'Egyezetetesek_szama', 
                 'Preferalt_szinvonal', 'Csaladi_allapot', 'Utazasok_szama', 'Pitch_ertekelese', 'Havi_jovedelem')]
# 11 magyar�z�v�ltoz�: 6 darab mennyis�gi - 5 darab min�s�gi
# Kihagyott v�ltoz�k (8 darab): Ugyfel_ID, Varos_fejlettsege, Foglalkozas, Utazasi_csomag, Utlevel, Sajat_auto, Utazo_gyerekek_szama, Betoltott_pozicio
str(utazas)
View(utazas)

# Hi�nyz� adatok kisz�r�se (NA-k kisz�r�se):
sapply(utazas, function(x) sum(is.na(x))) # Hi�nyz� adatok kimutat�sa v�ltoz�nkk�nt.
utazas <- utazas[complete.cases(utazas),] # �sszes sor kisz�r�se, ami tartalmaz NA-t.
sum(is.na(utazas)) # Ellen�rz�s.
str(utazas)

# Sz�ks�ges v�ltoz�k �talak�t�sa faktorr� + kateg�ri�k magyar n�vv� alak�t�sa (itt is �kezetek n�lk�l):
utazas [,c('Adasvetel','Kontakt_tipusa', 'Ugyfel_neme', 'Preferalt_szinvonal', 'Csaladi_allapot','Pitch_ertekelese')] <- lapply(utazas [,c('Adasvetel','Kontakt_tipusa', 'Ugyfel_neme', 'Preferalt_szinvonal', 'Csaladi_allapot','Pitch_ertekelese')], as.factor)
str(utazas)

levels(utazas$Kontakt_tipusa) # Viszony�t�si alap megfelel.
utazas$Kontakt_tipusa <- factor(utazas$Kontakt_tipusa,
                             levels = c('Company Invited', 'Self Enquiry'),
                             labels = c('Iroda','Sajat'))

levels(utazas$Csaladi_allapot) # Viszony�t�si alap az Egyed�l�ll� legyen.
utazas$Csaladi_allapot <- factor(utazas$Csaladi_allapot,
                             levels = c('Divorced', 'Married','Single','Unmarried'),
                             labels = c('Elvalt','Hazas','Egyedulallo','Kapcsolatban'))
utazas$Csaladi_allapot <- relevel(utazas$Csaladi_allapot, ref = 'Egyedulallo') # K�s�bbi haszn�latra.

levels(utazas$Ugyfel_neme) # Viszony�t�si alap megfelel.
utazas$Ugyfel_neme <- factor(utazas$Ugyfel_neme,
                      levels = c('Fe Male', 'Female', 'Male'),
                      labels = c('No', 'No', 'Ferfi')) # Az adatb�zisban a Female el�r�s miatt k�tszer szerepelt.
str(utazas) # Ellen�rz�s.

# Outliersz�r�s :
# Az adatok kisz�r�s�n�l maximum 1% az elfogadhat�:
4194*0.01 # =41,94, ami azt jelenti, hogy maximum 41-42 adatot sz�rhet�nk/dobhatunk ki.

# Outlierek keres�se :
summary(utazas)
boxplot(utazas$Eletkor) # Nincsen outlier.
boxplot(utazas$Pitch_hossza) # Van 1 outlier, 120-n�l kisebb �rt�ket kell meghat�rozni, hogy kisz�rj�k.
boxplot(utazas$Utazok_szama) # Nincsen outlier.
boxplot(utazas$Egyezetetesek_szama) # Nincsen outlier.
boxplot(utazas$Utazasok_szama) # Van 4 outlier, 15-n�l kisebb �rt�ket kell meghat�rozni, hogy kisz�rj�k.
boxplot(utazas$Havi_jovedelem) # Van 3 outlier (1 fel�l, 2 alul), ezek pontos �rt�kei nem l�that�ak. Megn�zz�k head �s tail f�gv�nnyel.
tail(sort(utazas$Havi_jovedelem), 30) # 40000 f�l�tt 1 outlier, 40000-n�l kisebb �rt�ket kell meghat�rozni, hogy kisz�rj�k.
head(sort(utazas$Havi_jovedelem), 30) # 5000 alatt 2 outlier, 5000-n�l nagyobb �rt�ket kell meghat�rozni, hogy kisz�rj�k.

# Outlierek kisz�r�se
utazas <- utazas[utazas$Pitch_hossza < 120, ]
utazas <- utazas[utazas$Utazasok_szama < 15, ]
utazas <- utazas[utazas$Havi_jovedelem < 40000 & utazas$Havi_jovedelem > 5000, ]
summary(utazas) # Ellen�rz�s.
# A kezdeti 4194-r�l 4186-ra cs�kkentett�k le a megfigyel�sek sz�m�t, ezzel 8 megfigyel�st sz�rt�nk le/dobtunk ki. 
# Ez a 8 megfigyel�s a 41-42-es megengedhet� t�r�shat�ron bel�l van.
4186/4194 # Ez azt jelenti, hogy 99,8%-�t megtartottuk az adatoknak. Ez szuper �s elfogadhat�.

# Le�r� statisztikai elemz�s:
library(psych)
str(utazas)
describe(utazas[c('Eletkor', 'Pitch_hossza', 'Utazok_szama', 'Egyezetetesek_szama', 'Utazasok_szama', 'Havi_jovedelem' )])
summary(utazas[c('Eletkor', 'Pitch_hossza', 'Utazok_szama', 'Egyezetetesek_szama', 'Utazasok_szama', 'Havi_jovedelem' )])

# 2. Feladat: Egyv�ltoz�s + K�tv�ltoz�s + T�bbv�ltoz�s adatvizualiz�ci�:
### Eredm�nyv�ltoz� �s magyar�z� v�ltoz�k p�ronk�nti kapcsolatai 
# Odds ad�sv�tel = P (megvette)/ 1-P (nem vette meg)
# az ad�sv�tel es�ly�t n�vel� magyar�z�v�ltoz�k eset�n pozit�v, az azt cs�kkent�k eset�n negat�v b�t�t v�runk

# Egyv�ltoz�s vizualiz�ci�:
# Min�s�gi v�ltoz�k: Oszlopdiagrammok
library(ggplot2)
str(utazas)
# Ad�sv�tel:
a <- barplot(prop.table(table(utazas$Adasvetel)),
             ylim = c(0,1),
             main = 'Ad�sv�tel megoszl�sa',
             ylab = 'Relat�v gyakoris�g',
             col = 'pink3')
text(a, prop.table(table(utazas$Adasvetel)),
     table(utazas$Adasvetel), pos = 3)

# Kontakt t�pusa:
b <- barplot(prop.table(table(utazas$Kontakt_tipusa)),
             ylim = c(0,1),
             main = 'Kontakt t�p�s�nak megoszl�sa ',
             ylab = 'Relat�v gyakoris�g',
             col = 'pink3')
text(a, prop.table(table(utazas$Kontakt_tipusa)),
     table(utazas$Kontakt_tipusa), pos = 3)

# �gyf�l neme:
c <- barplot(prop.table(table(utazas$Ugyfel_neme)),
             ylim = c(0,1),
             main = '�gyf�l nemek megoszl�sa',
             ylab = 'Relat�v gyakoris�g',
             col = 'pink3')
text(c, prop.table(table(utazas$Ugyfel_neme)),
     table(utazas$Ugyfel_neme), pos = 3)

# Prefer�lt sz�nvonal:
d <- barplot(prop.table(table(utazas$Preferalt_szinvonal)),
             ylim = c(0,1),
             main = 'Prefer�lt sz�nvonal megoszl�sa',
             ylab = 'Relat�v gyakoris�g',
             col = 'pink3')
text(d, prop.table(table(utazas$Preferalt_szinvonal)),
     table(utazas$Preferalt_szinvonal), pos = 3)

# Csal�di �llapot:
e <- barplot(prop.table(table(utazas$Csaladi_allapot)),
             ylim = c(0,1),
             main = 'Csal�di �llapot megoszl�sa',
             ylab = 'Relat�v gyakoris�g',
             col = 'pink3')
text(e, prop.table(table(utazas$Csaladi_allapot)),
     table(utazas$Csaladi_allapot), pos = 3)

# Pitch �rt�kel�se:
f <- barplot(prop.table(table(utazas$Pitch_ertekelese)),
             ylim = c(0,1),
             main = 'Pitch �rt�kel�sek megoszl�sa',
             ylab = 'Relat�v gyakoris�g',
             col = 'pink3')
text(f, prop.table(table(utazas$Pitch_ertekelese)),
     table(utazas$Pitch_ertekelese), pos = 3)

# Mennyis�gi v�ltoz�k: Hisztogrammok
# �letkor
str(utazas)
hist(utazas$Eletkor)
ggplot(utazas, aes(x=Eletkor)) + geom_histogram(color="white", fill="pink3", binwidth = 5) +
  scale_x_continuous(name = '�letkor [�v]') +
  scale_y_continuous(name = 'Gyakoris�g [F�]') +
  ggtitle('�letkor megoszl�sa')

# Pitch hossza:
hist(utazas$Pitch_hossza)
ggplot(utazas, aes(x=Pitch_hossza)) + geom_histogram(color="white", fill="pink3", binwidth = 1.9) +
  scale_x_continuous(name = 'Pitch hossza [Perc]') +
  scale_y_continuous(name = 'Gyakoris�g') +
  ggtitle('Pitch hossz�nak a megoszl�sa')

# Utaz�k sz�ma:
hist(utazas$Utazok_szama)
ggplot(utazas, aes(x=Utazok_szama)) + geom_histogram(color="white", fill="pink3", binwidth = 1) +
  scale_x_continuous(name = 'Utaz�k sz�ma [F�]') +
  scale_y_continuous(name = 'Gyakoris�g') +
  ggtitle('Utaz�k sz�m�nak a megoszl�sa')

# Egyeztet�sek sz�ma:
hist(utazas$Egyezetetesek_szama)
ggplot(utazas, aes(x=Egyezetetesek_szama)) + geom_histogram(color="white", fill="pink3", binwidth = 1) +
  scale_x_continuous(name = 'Egyeztet�sek sz�ma [Darab]') +
  scale_y_continuous(name = 'Gyakoris�g') +
  ggtitle('Egyeztet�sek sz�m�nak a megoszl�sa')

# Utaz�sok sz�ma:
hist(utazas$Utazasok_szama)
ggplot(utazas, aes(x=Utazasok_szama)) + geom_histogram(color="white", fill="pink3", binwidth = 1) +
  scale_x_continuous(name = 'Utaz�sok sz�ma [Darab]') +
  scale_y_continuous(name = 'Gyakoris�g') +
  ggtitle('�tlagos �vi utaz�ssz�m megoszl�sa')

# Havi j�vedelem:
hist(utazas$Havi_jovedelem)
ggplot(utazas, aes(x=Havi_jovedelem)) + geom_histogram(color="white", fill="pink3", binwidth = 1910) +
  scale_x_continuous(name = 'Havi j�vedelem [USD]') +
  scale_y_continuous(name = 'Gyakoris�g') + 
  ggtitle('Havi j�vedelem megoszl�sa')

# K�tv�ltoz�s vizualiz�ci�:
# Keresztt�bla a kateg�ria t�pus� v�ltoz�kra
round(prop.table(table(utazas$Preferalt_szinvonal, utazas$Adasvetel), margin=1), 3)
# 3-4 csillagosok azonos ar�nyban ker�ltek megv�telre, 5 csillagosok nagyobb ar�nyban.
# Pozit�v b�t�t v�runk, referencia kateg�ria a 3 csillag.
round(prop.table(table(utazas$Pitch_ertekelese, utazas$Adasvetel), margin=1), 3)
round(prop.table(table(utazas$Pitch_ertekelese)), 3)
# 3-ast adnak leggyakrabban, az�rt mert ez a semleges �rt�kel�s.
# 4-est kevesebben adnak, mert ha valami tetszik, akkor vagy 5-�st adsz vagy semlegesre �rt�keled.
# Nem teljesen konzisztensen n� a siker ar�nya a magasabb �rt�kel�st kapott pitchek k�z�tt.
# Enyh�n pozit�v b�t�t v�runk vagy nem szignifik�ns v�ltoz�t, referencia kateg�ria az 1-es.
# Ez j�l mutatja ezt.
round(prop.table(table(utazas$Ugyfel_neme, utazas$Adasvetel), margin=1), 3)
# Nincs jelent�s elt�r�s, hasonl� ar�nyban utas�tott�k el illetve vett�k meg az utat a n�k �s f�rfiak.
# Nem biztos, hogy szignifik�ns lesz a v�ltoz�.
round(prop.table(table(utazas$Csaladi_allapot, utazas$Adasvetel), margin=1), 3)
# Legmagasabb ar�nyban sikeres csoport a Egyedulallo volt, mivel ez a referencia kateg�ria, �gy negat�v b�t�t v�runk.
round(prop.table(table(utazas$Kontakt_tipusa, utazas$Adasvetel), margin=1), 3)
# Iroda nagyobb ar�nyban sikeres, ez a referencia kateg�ria -> negat�v b�t�t v�runk.
# Ha magadt�l m�sz van m�r egy preferenci�d, ha ezt nem siker�l megtal�lni nem fogadod el, veszed meg. Ha neked pitchelnek, akkor lehet megtetszik �s megveszed �gy, hogy nem is tervezt�l utazni.

# P�ronk�nti doboz�bra a vegyes kapcsolatok szeml�ltet�s�re
boxplot(Eletkor~Adasvetel, data=utazas)
# Sikeres kimenetel ink�bb a fiatalabb koroszt�lyb�l ker�lt ki, id�sebbek ink�bb el�lltak.
# Negat�v b�t�t v�runk.
# Ggplot-tal:
ggplot(utazas, aes(x = Adasvetel, y = Eletkor)) + 
  geom_boxplot(color="black", fill="pink3") +
  xlab('Ad�sv�tel') +
  ylab('�letkor [�v]') +
  ggtitle('�letkor �s Ad�sv�tel kapcsolata')
  
boxplot(Pitch_hossza ~ Adasvetel, data=utazas)
# Sikeres kimenetelt eredm�nyeztek a hosszabb pitchek, sikeres pitchek hossz�nak a medi�nja magasabb n�mileg mint a nem sikeresek�.
# Enyh�n pozit�v b�t�t v�runk.
# Ggplot-tal:
ggplot(utazas, aes(x = Adasvetel, y = Pitch_hossza)) + 
  geom_boxplot(color="black", fill="pink3") +
  xlab('Ad�sv�tel') +
  ylab('Pitch hossza [Perc]') +
  ggtitle('Pitch hossza �s Ad�sv�tel kapcsolata')

boxplot(Egyezetetesek_szama ~ Adasvetel, data=utazas)
# Volt aki 1 egyeztet�s ut�n r�gt�n elutas�totta az aj�nlatot �s volt aki 6 ut�n l�pett vissza.
# Medi�n mindk�t esetben 4.
# Enyh�n pozit�v b�t�t v�runk.
# Ggplot-tal:
ggplot(utazas, aes(x = Adasvetel, y = Egyezetetesek_szama)) + 
  geom_boxplot(color="black", fill="pink3") +
  xlab('Ad�sv�tel') +
  ylab('Egyeztet�sek sz�ma') +
  ggtitle('Egyeztet�sek sz�ma �s Ad�sv�tel kapcsolata')

boxplot(Utazasok_szama ~ Adasvetel, data=utazas)
# Nem biztos, hogy ez a v�ltoz� szignifik�ns lesz.
# Ggplot-tal:
ggplot(utazas, aes(x = Adasvetel, y = Utazasok_szama)) + 
  geom_boxplot(color="black", fill="pink3") +
  xlab('Ad�sv�tel') +
  ylab('Utaz�sok sz�ma') +
  ggtitle('Utaz�sok sz�ma �s Ad�sv�tel kapcsolata')

boxplot(Havi_jovedelem ~ Adasvetel, data=utazas)
# A vev�k k�z�tt kisebb a medi�nb�r mint az elutas�t�k k�z�tt, ez �rdekes.
# Enyh�n negat�v b�t�t v�runk.
# Ez �sszekapcsolhat� az Eletkor-Adasvetel boxplottal, aki fiatalabb ink�bb elfogadta �s a korrel�ci�s m�trixb�l l�tszik, h 0,4-el korrel�l egym�ssal.
# EZ logikus is, aki fiatalabb, annak m�g kisebb a j�vedelme. Kevesebb tapasztalat stb...
# Ggplot-tal:
ggplot(utazas, aes(x = Adasvetel, y = Havi_jovedelem)) + 
  geom_boxplot(color="black", fill="pink3") +
  xlab('Ad�sv�tel') +
  ylab('Havi j�vedelem (USD)') +
  ggtitle('Havi j�vedelem �s Ad�sv�tel kapcsolata')

boxplot(Utazok_szama ~ Adasvetel, data=utazas)
# Nem biztos, hogy ez a v�ltoz� szignifik�ns lesz.
# Ggplot-tal:
ggplot(utazas, aes(x = Adasvetel, y = Utazok_szama)) + 
  geom_boxplot(color="black", fill="pink3") +
  xlab('Ad�sv�tel') +
  ylab('Utaz�k sz�ma') +
  ggtitle('Utaz�k sz�ma �s Ad�sv�tel kapcsolata')

# Multikollinearit�s esetleges el�rejelz�se a magyar�z�v�ltoz�k k�z�tt:
# I. Csal�di �llapot �s egy�tt utaz�k sz�ma:
round(prop.table(table(utazas$Csaladi_allapot, utazas$Utazok_szama),margin=1),3)
# Nincs l�nyegi k�l�nbs�g a csoportok k�z�tt.
by(utazas[, "Utazok_szama"], utazas$Csaladi_allapot, summary)
# Minden csoportn�l 3 f� egy�tt utaz� a medi�n.
ggplot(utazas, aes(x = Csaladi_allapot, y = Utazok_szama)) + 
  geom_boxplot(color="black", fill="pink3") +
  xlab('Csal�di �llapot') +
  ylab('Utaz�k sz�ma') +
  ggtitle('Utaz�k sz�ma �s Csal�di �llapot kapcsolata')

# II. Pitch el�gedetts�g �s egyeztet�sek sz�ma:
round(prop.table(table(utazas$Pitch_ertekelese, utazas$Egyezetetesek_szama),margin=1),3)
by(utazas[, "Egyezetetesek_szama"], utazas$Pitch_ertekelese, summary)
# Nincs k�l�nbs�g.
ggplot(utazas, aes(x =Pitch_ertekelese, y = Egyezetetesek_szama)) + 
  geom_boxplot(color="black", fill="pink3") +
  xlab('Pitch �rt�kel�sek') +
  ylab('Egyeztet�sek sz�ma') +
  ggtitle('Egyeztet�sek sz�ma �s Pitch �rt�kel�sek kapcsolata')

# III. Prefer�lt min�s�g �s havi j�vedelem:
boxplot(utazas$Havi_jovedelem~utazas$Preferalt_szinvonal)
# Nagyon egyenl�ek az eloszl�sok, �gy l�tszik, nem befoly�solta a j�vedelem a min�s�get illet� v�laszt�st.
by(utazas[, "Havi_jovedelem"], utazas$Preferalt_szinvonal, summary)
ggplot(utazas, aes(x =Preferalt_szinvonal, y = Havi_jovedelem)) + 
  geom_boxplot(color="black", fill="pink3") +
  xlab('Prefer�lt sz�nvonal') +
  ylab('Havi j�vedelem') +
  ggtitle('Havi j�vedelem �s Prefer�lt sz�nvonal kapcsolata')

# IV. El�gedetts�g �s pitch hossza:
boxplot(utazas$Pitch_hossza~utazas$Pitch_ertekelese)
by(utazas[, "Pitch_hossza"], utazas$Pitch_ertekelese, summary)
# Szint�n nincs nagy elt�r�s.
ggplot(utazas, aes(x =Pitch_ertekelese, y = Pitch_hossza)) + 
  geom_boxplot(color="black", fill="pink3") +
  xlab('Pitch �rt�kel�se') +
  ylab('Pitch hossza') +
  ggtitle('Pitch hossza �s Pitch �rt�kel�s kapcsolata')

# T�bbv�ltoz�s vizualiz�ci�:
# Korrel�ci�s m�trix a mennyis�gi v�ltoz�kra:
korrelacios_matrix <- round(cor(utazas[ ,c( "Pitch_hossza", "Egyezetetesek_szama","Utazasok_szama", "Havi_jovedelem", "Eletkor", "Utazok_szama")]), 3)
library(corrplot)
corrplot(korrelacios_matrix, method= "circle")
# �letkor �s havi j�vedelem korrel�l legink�bb, pozit�van kb 0,4-es er�ss�g�.
# T�bbi magyar�z� v�ltoz� k�z�tt legfeljebb 0,2-es korrel�ci� van -> nem sz�m�tunk magas vif mutat�ra.
# install.packages('ggcorrplot')
library(ggcorrplot)
ggcorrplot(korrelacios_matrix) 
ggcorrplot(korrelacios_matrix, method = 'circle') +
  ggtitle('Korrel�ci�s m�trix a mennyis�gi v�ltoz�kra')

# 3. Feladat:  Logisztikus regresszi�s modell + modellszelekci�:
# Logisztikus regresszi� futtat�sa az �sszes v�ltoz�val:
modell_alap <- glm(Adasvetel ~ ., data = utazas, family = binomial(link = 'logit'))
summary(modell_alap)

# Multikollinearit�s:
# install.packages('car')
library(car)
vif(modell_alap) # Nincs multikollinearit�s. Igazolja a sejt�s�nk.

# K�zi �s g�pi modellszelekci�:
# K�zi modellszelekci�:
BetaTabla <- summary(modell_alap)$coefficients
BetaTabla[order(BetaTabla[,4]),] 
# Az legutols� 3 v�ltoz�t szedt�k ki. Ezek a k�vetkez� v�ltoz�k: Pitch_ertekelese, Utazok_szama, Ugyfel_neme
# Erre k�sz�tett�nk egy modellt:
utazas_szukitett_kezi <- utazas[, c('Adasvetel', 'Eletkor', 'Kontakt_tipusa', 'Pitch_hossza', 'Preferalt_szinvonal', 'Egyezetetesek_szama', 
                'Csaladi_allapot', 'Utazasok_szama', 'Havi_jovedelem')]

modell_szukitett_kezi <- glm(Adasvetel ~ ., data = utazas_szukitett_kezi, family = binomial(link = 'logit'))
summary(modell_szukitett_kezi)
# Minden szok�sos szignifikanciaszinten szignifik�nsak a v�ltoz�ink. (Alfa =1%, 5% �s 10% is egyar�nt.)

# G�pi modellszelekci� (Stepwise): 
library(MASS)
# A full modell az nek�nk az a model_alap, ezt nem nevezz�k �t. Ezzel dolgozunk a backward elj�r�sn�l.
step(object = modell_alap, direction = 'backward', k = log(4186))
# A g�p 4 v�ltoz�t dobna ki: Pitch_ertekelese, Utazok_szama, Ugyfel_neme, Utazasok_szama
# Az els� 3 kidobott v�ltoz�t mi is kidobn�nk, de a g�p m�g egy negyediket is kidobna ezek ut�n.
# N�zz�k meg sz�mokban a modellt.

utazas_szukitett_gepi <- utazas[, c('Adasvetel', 'Eletkor', 'Kontakt_tipusa', 'Pitch_hossza', 'Preferalt_szinvonal', 'Egyezetetesek_szama', 
                           'Csaladi_allapot', 'Havi_jovedelem')]
modell_szukitett_gepi <- glm(Adasvetel ~ ., data = utazas_szukitett_gepi, family = binomial(link = 'logit'))
summary(modell_szukitett_gepi)

# A 3 modell �sszehasonl�t�sa:
# AIC, BIC alapj�n:
AIC(modell_alap, modell_szukitett_kezi, modell_szukitett_gepi)
BIC(modell_alap, modell_szukitett_kezi, modell_szukitett_gepi)
# AIC alapj�n a sorrend: modell_alap > modell_szukitett_kezi > modell_szukitett_gepi
# BIC alapj�n a sorrend: modell_alap < modell_szukitett_kezi < modell_szukitett_gepi

# McFadden f�le Pszeudo R^2 alapj�n:
# install.packages('DescTools')
library(DescTools)
PR2_modell_alap <- PseudoR2(modell_alap, which = "McFadden")
PR2_modell_szukitett_kezi <- PseudoR2(modell_szukitett_kezi, which = "McFadden")
PR2_modell_szukitett_gepi <- PseudoR2(modell_szukitett_gepi, which = "McFadden")
(PR2_modell_alap - PR2_modell_szukitett_kezi)*100 # 0,54%-ponttal cs�kkent az R2 az nagyon kev�s.
# �sszegz�s:
# Mi ezeknek ellen�re is a modell__szukitett_kezi -vel akarunk tov�bbmenni. 
# Oka: A g�pi szelekci�n�l a negyedik v�ltoz� elhagy�sa (Utazasok_szama) egy minden szignifikanciaszinten is szignifik�ns v�ltoz�.
# A BIC is mutatja, hogy nagyon minim�lis a k�l�nbs�g, ha ezt a v�ltoz�t kiszedj�k. Ezek ok�n mi szeretn�nk megtartani a Utazasok_szama v�ltoz�t.
# Sz�val a modell_szukitett_kezi lesz az, amivel dolgozni szeretn�nk. Legyen ennek a neve modell_vegleges
modell_vegleges <- modell_szukitett_kezi
utazas_vegleges <- utazas_szukitett_kezi
PR2_modell_vegleges <- PR2_modell_szukitett_kezi

# V�gs� modell:
summary(modell_vegleges)

# 4. Feladat: McFadden-f�le pszeudo R-n�gyzet + Glob�lis Khi-n�gyzet pr�ba  + VIF mutat�
# Mcfadden:
PR2_modell_vegleges
PR2_modell_alap

# Ez egy �ppenhogy k�zepes magyar�z�er�, ami �sszecseng a ROC g�rbe alatti ter�let m�ret�vel.
deviance(modell_alap) - deviance(modell_vegleges)
anova(modell_alap, modell_vegleges)

# Hipot�zisvizsg�lat: GOF teszt- Goodness of Fit teszt
# H0: B1=B2=B3...Bk == 0 , azaz a sokas�gban �sszeomlik a minta magyar�z�ereje.
# H1: van legal�bb egy Bj, ami nem 0, azaz szignifik�ns. Ez azt jelenti, hogy kiterjeszthet� a modell a sokas�gra (a val�s�gra).
summary(modell_vegleges)
# Nulldeviancia: 4089.9, ez azt jelenti, hogy ez az �res modellnek a devianci�ja. A t�k�letes modellt�l val� elt�r�s. A t�k�letes 0.
# Rezidu�lis deviancia(saj�t modell�nknek): 3654.2, ez is a t�k�letes modellt�l a 0-t�l val� elt�r�s.
# 8 magyar�z�v�ltoz�val 4089.9-r�l 3654.2-re cs�kkentett�k a devianci�t.
# R^2 manu�lis kisz�mol�sa: (4089.9-3654.2)/4089.9 = 0,10655343
str(utazas_vegleges)
(4089.9-3654.2)/4089.9
PR2_modell_vegleges # Ellen�rz�s, val�ban annyi. Szuper.
# Az �res �s a t�k�letes modell k�z�tti �t 10,65%-�t tett�k meg.

# Khi n�gyzet pr�baf�ggv�ny �rt�ke:
4089.9-3654.2 # 435,7
# Szabads�gfokok sz�ma = df = 8
# Ez egy jobboldali pr�ba, teh�t a 435,7 fel� es�si val�sz�n�s�get keress�k. 
# 435,7: ennyi devianci�t magyar�z meg a modell az �reshez k�pest.
1- pchisq(435.7, df =8) # A p-�rt�k 0, ez szuper. A H0-t elutas�tjuk. Az aktu�lis modell ellett d�nt�nk. 
# Ami azt jelenti, hogy relev�ns a modell a sokas�gban is.

# Multikollinearit�s
vif(modell_vegleges)
# VIF �rt�kek 5 alatt vannak b�ven, s�t az �sszes 2 alatt, sz�val sz� sincs multikollinearit�sr�l.

# 5. Feladat: Klasszifik�ci�s m�trix + ROC-g�rbe:
# Klasszifik�ci�s m�trix
prop.table(table(utazas$Adasvetel))
# Az �sszes utaz�si �rt�kes�t�s 19%-a volt sikeres.
# 1: sikeres �rt�kes�t�s, megvett�k az utaz�st, 0: nem sikeres �rt�kes�t�s, nem vett�j meg az utaz�st

### Eredm�nyv�ltoz� �s magyar�z� v�ltoz�k p�ronk�nti kapcsolatai ###
# Odds ad�sv�tel = P (megvette)/ 1-P (nem vette meg)

# LOGIT oszlop hozz�ad�sa: link=ln odds ad�sv�tel = alfa + b�ta*x minden megfigyel�sre.
utazas_vegleges$pred_link <- predict.glm(object = modell_vegleges, newdata = utazas_vegleges, type = 'link')
head(utazas_vegleges) 

# Ad�sv�tel val�sz�n�s�g oszlop hozz�ad�sa: Ad�sv�tel val�szin�s�g minden val�sz�n�s�gre
utazas_vegleges$pred_resp <- predict.glm(object = modell_vegleges, newdata = utazas_vegleges, type = 'response')
head(utazas_vegleges)

# Ad�sv�tel val�szin�s�ge minden egyes megfigyel�s eset�n.
hist(utazas_vegleges$pred_resp)
ggplot(utazas_vegleges, aes(x=pred_resp)) + geom_histogram(color="white", fill="pink3", binwidth = 0.065) +
  scale_x_continuous(name = 'Ad�sv�tel el�rejelzett val�sz�n�s�g') +
  scale_y_continuous(name = 'Gyakoris�g') +
  ggtitle('Ad�sv�tel el�rejelzett val�sz�n�s�g�nek eloszl�sa')

# Klasszifik�ci�s m�trixok:
utazas_vegleges$prediction50 <- as.factor(as.numeric(utazas_vegleges$pred_resp > 0.5))
table(utazas_vegleges$prediction50) # Csak el�n�zet. A modell�nk 159 sikeres ad�sv�telt jelez el�re.

# Klasszifik�ci�s m�trix 50%-os Cut-value �rt�k mellett:
# y = 1, �gynevezett pozit�v oszt�ly
table(utazas_vegleges$Adasvetel, utazas_vegleges$prediction50)
# Accuracy(Pontoss�g)(1): Helyes tal�lat/�sszes tal�latok ar�nya: 
Accuracy50 <- (3338+113)/4186 # 82,4% a Pontoss�g ar�nya

# Recall(1): Ami a val�s�gban ad�sv�tel volt, azok k�z�l mennyit predikt�ltam ad�sv�telnek. A helyesen sikeresen jelzettek ar�nya, a val�ban sikeresek k�z�l. Ez val�j�ban a TPR.
Recall50 <- 113/(113+689) # 14,1%

# Precision(1): Helyesen sikeresnek jelzettek ar�nya, az �sszes sikeresnek k�z�l
Precision50 <- 113/(113+46) # 71,1%

# TPR(1): Ugyanaz, mint a Recall(1)
TPR50 <- 113/(113+689) # 14,1%

# FPR(1): Val�j�ban nem sikeres ad�sv�telek k�z�l a modell mennyit azonos�tott sikeresnek.
FPR50 <- 46/(46+3338) # 1,36%

# FNR (1): Val�j�ban sikeres ad�sv�telek k�z�l a modell mennyit jelzett helytelen�l sikertelennek. 1-TPR
FNR50 <- 689/(689+113) # 85,9%

# TNR(1): 1- FPR, A sikertelen ad�sv�telek h�ny sz�zal�k�t jelezte el�re sikertelennek. A val�ban sikertelen ad�sv�telek k�z�l menyiit azonos�tott a modell helyesen sikertelennek.
TNR50 <- 3338/(3338+46) # 98,64%

table(utazas_vegleges$Adasvetel)
802/4186 # Ellen�rz�s, val�ban ~ 19,2%
utazas_vegleges$prediction19.2 <- as.factor(as.numeric(utazas_vegleges$pred_resp > 802/4186))
table(utazas_vegleges$prediction19.2)
# A modellunk 1641 sikeres ad�sv�telt jelez el�re.

# Klasszifik�ci�s m�trix 19,2%-os Cut-value �rt�k mellett: Ez a sikeres ad�sv�telek ar�nya a mint�n bel�l
# y = 1, �gynevezett pozit�v oszt�ly
table(utazas_vegleges$Adasvetel, utazas_vegleges$prediction19.2)
# Accuracy(Pontoss�g)(1): Helyes tal�lat/�sszes tal�latok ar�nya: 
Accuracy19.2 <- (2254+511)/4186 # 66,1% a pontoss�g.

# Recall(1): Ami a val�s�gban ad�sv�tel volt, azok k�z�l mennyit predikt�ltam ad�sv�telnek. A helyesen sikeresen jelzettek ar�nya, a val�ban sikeresek k�z�l. Ez val�j�ban a TPR.
Recall19.2 <- 511/(511+291) # 63,7%

# Precision(1): Helyesen sikeresnek jelzettek ar�nya, az �sszes sikeresnek k�z�l
Precision19.2 <- 511/(511+1130) # 31,14%

# TPR(1): Ugyanaz, mint a Recall(1)
TPR19.2 <- 511/(511+291) # 63,7%

# FPR(1): Val�j�ban nem sikeres ad�sv�telek k�z�l a modell mennyit azonos�tott sikeresnek.
FPR19.2 <- 1130/(1130+2254) # 33,4%

# FNR (1): Val�j�ban sikeres ad�sv�telek k�z�l a modell mennyit jelzett helytelen�l sikertelennek. 1-TPR
FNR19.2 <- 291/(291+511) # 36,3%

# TNR(1): 1- FPR, A sikertelen ad�sv�telek h�ny sz�zal�k�t jelezte el�re sikertelennek. A val�ban sikertelen ad�sv�telek k�z�l menyiit azonos�tott a modell helyesen sikertelennek.
TNR19.2 <- 2254/(2254+1130) # 66,6%

# ROC-g�rbe:
# install.packages('pROC')
library(pROC)
roc_utazas <- roc(utazas_vegleges$Adasvetel, utazas_vegleges$pred_resp)
plot(roc_utazas)
auc(roc_utazas) # 0,7122. T�k�lets a modell, ha ez az �rt�k 1 lenne. Nem v�letlenszer�en tippelget, de nem is t�k�letes a modell, valahol k�z�pt�von, f�l�ton van.
auc(roc_utazas)/0.5 # 42%-ban jobb, mint a vakt�ban tal�lgat�s.

# Ellen�rz�sek:
which.min(abs(roc_utazas$thresholds- 802/4186))
roc_utazas$thresholds[2185]
roc_utazas$sensitivities[2185] 
roc_utazas$specificities[2185] 

which.min(abs(roc_utazas$thresholds-0.5))
roc_utazas$thresholds[3295] # Val�ban annyi.
roc_utazas$sensitivities[3295]
TPR50 # Val�ban annyi.
roc_utazas$specificities[3295]
TNR50 # Val�ban annyi.

# Legjobb:
coords(roc_utazas, "best", ret = "threshold", best.method = "closest.topleft")
which.min(abs(roc_utazas$thresholds-0.1876846))
roc_utazas$thresholds[2139]
roc_utazas$sensitivities[2139] # TPR, ez a ROC g�rbe y-tengelye
roc_utazas$specificities[2139] # 
FPR_rocoptim <- 1-(roc_utazas$specificities[2139])
FPR_rocoptim

# 6. Feladat: Regresszi�s egyenlet + �rtelmez�s:
# Regresszi�s egyenlet:
summary(modell_vegleges)
BetaTabla <- summary(modell_vegleges)$coefficients
BetaTabla
# A regresszi�s egyenlet �s �rtelmez�sek a Word-dokumentumban.

# 7. Feladat: El�rejelz�s a modellel
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
# Ha egy �gyf�l ezekkel az adatokkal rendelkezik (wordben �rj�tok le.), akkor az ad�sv�tel oddsa v�rhat�an 18,58%.

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
# Most itt az volt a c�l, hogy az v�ltoz�k �rt�keit �gy v�ltoztassuk, hogy annak odds-n�vel� hat�sa legyen.
# Ez siker�lt is, az �gyf�lnek 55,1%-os lett a becs�lt ad�sv�tel oddsa.

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
# Ezzel el�rejelezz�k az y-kalapot egy ilyen tulajdons�g� �gyf�ln�l.
# Ez csak egy ellen�rz�s volt a t�bl�zat m�sodik �gyfel�re. 
head(utazas_vegleges)

### V�GE ### 
##################################################################################################################################################################################################################################
### V�GE ###



# R outputok, amelyeket megjelen�t�nk a dokumentumban (INNENT�L M�R NEM �RDEMES ELOLVASNI/LEFUTTATNI):
library(writexl)

# Alap le�r� statisztika:
Output1 <- describe(utazas[c('Eletkor', 'Pitch_hossza', 'Utazok_szama', 'Egyezetetesek_szama', 'Utazasok_szama', 'Havi_jovedelem' )])
Output2 <- as.data.frame(summary(utazas[c('Eletkor', 'Pitch_hossza', 'Utazok_szama', 'Egyezetetesek_szama', 'Utazasok_szama', 'Havi_jovedelem' )]))
write_xlsx(Output1, 'describe.xlsx')
write_xlsx(Output2, 'summary.xlsx')

# Alap_modell:
Output3 <- as.data.frame(summary(modell_alap)$coefficients)
write_xlsx(Output3, 'modell_alap.xlsx')

# K�zi, Manu�lis modell:
BetaTabla <- summary(modell_alap)$coefficients
Output4 <- as.data.frame(BetaTabla[order(BetaTabla[,4]),])
write_xlsx(Output4, 'BetaTablaSorrend.xlsx')

# Modellek �sszehasonl�t�sa:
Output5.1 <- AIC(modell_alap, modell_szukitett_kezi, modell_szukitett_gepi)
Output5.2 <- BIC(modell_alap, modell_szukitett_kezi, modell_szukitett_gepi)
Output5 <- as.data.frame(cbind(Output5.1, BIC = Output5.2[,2]))
write_xlsx(Output5,'ModellOsszehasonlitas.xlsx')

# V�gs� modell:
Output6 <- as.data.frame(summary(modell_vegleges)$coefficients)
write_xlsx(Output6, 'VegsoModell.xlsx')

# Klasszifik�ci�s m�trix:
Output7 <- as.data.frame(table(utazas_vegleges$Adasvetel, utazas_vegleges$prediction50))
write_xlsx(Output7, 'C50.xlsx')
Output8 <- as.data.frame(table(utazas_vegleges$Adasvetel, utazas_vegleges$prediction19.2))
write_xlsx(Output8, 'C19.2.xlsx')

# Regresszi�s egyenlet:
Output9 <- as.data.frame(exp(BetaTabla[,1]))
Output9$Variables <- row.names(BetaTabla)
write_xlsx(Output9, 'RegressziosEgyenlet.xlsx')

