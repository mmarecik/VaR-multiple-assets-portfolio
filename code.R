library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(knitr)


dax <- read.csv("dax.csv") # German Stock Index DAX 30
wig <- read.csv("wig.csv") # WIG
smi <- read.csv("smi.csv") # Swiss Market Index

dax <- dax[,c(1,5)]
wig <- wig[,c(1,5)]
smi <- smi[,c(1,5)]

dax$Data <-  as.Date(dax$Data)
wig$Data <-  as.Date(wig$Data)
smi$Data <-  as.Date(smi$Data)

colnames(dax) <- c("data", "zamkniecie")
colnames(wig) <- c("data", "zamkniecie")
colnames(smi) <- c("data", "zamkniecie")


## Wstêpna analiza


stocks <- merge(dax, wig, by="data")
stocks <- merge(stocks, smi, by="data")


g1 <- ggplot(data = stocks, aes(x = data)) +
  geom_line(aes(y = zamkniecie.x), size=.6, color = '#065823') + 
  xlab("") + ylab("") + 
  labs(title="Ceny zamkniecia DAX 30") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()

g2 <- ggplot(data = stocks, aes(x = data)) +
  geom_line(aes(y = zamkniecie.y), size=.6, color = '#3E8C4B') +
  xlab("") + ylab("") + 
  labs(title="Ceny zamkniecia WIG") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()

g3 <- ggplot(data = stocks, aes(x = data)) +
  geom_line(aes(y = zamkniecie), size=.6, color = '#74AE56') + 
  xlab("") + ylab("") + 
  labs(title="Ceny zamkniecia SMI") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()

grid.arrange(g1, g2, g3)


ggplot(data = stocks, aes(x = data)) +
  geom_line(aes(y = zamkniecie.x, color = 'DAX 30'), size=.6, color = '#065823') + 
  geom_line(aes(y = zamkniecie, color = 'SMI'), size=.6, color = '#74AE56') +
  geom_line(aes(y = zamkniecie.y, color = 'WIG'), size=.6, color = '#3E8C4B') + 
  xlab("Data") + ylab("Cena zamkniecia") + 
  labs(title="Ceny zamkniecia indeksow gieldowych\nDAX 30, WIG oraz SMI",
       color = "Legenda") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()


dax$zwroty <- (dax$zamkniecie - lag(dax$zamkniecie)) /  lag(dax$zamkniecie)
wig$zwroty <- (wig$zamkniecie - lag(wig$zamkniecie)) /  lag(wig$zamkniecie)
smi$zwroty <- (smi$zamkniecie - lag(smi$zamkniecie)) /  lag(smi$zamkniecie)

g1 <- ggplot(data = dax, aes(x = data)) +
  geom_line(aes(y = zwroty), size=.3, alpha=.8) + 
  xlab("") + ylab("") + 
  labs(title="Stopy zwrotu DAX 30") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()

g2 <- ggplot(data = wig, aes(x = data)) +
  geom_line(aes(y = zwroty), size=.3, alpha=.8) +
  xlab("") + ylab("") + 
  labs(title="Stopy zwrotu WIG") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()

g3 <- ggplot(data = smi, aes(x = data)) +
  geom_line(aes(y = zwroty), size=.3, alpha=.8) + 
  xlab("") + ylab("") + 
  labs(title="Stopy zwrotu SMI") + 
  scale_x_date(date_breaks = "2 years")  + theme_bw()

grid.arrange(g1, g2, g3)


## Monitorowanie korelacji

dax <- dax[-1,]
dax$wariancja <- NaN
okno <- 30
for(i in c((okno+1):(nrow(dax)))){
  dax$wariancja[i] <- (sum((dax$zwroty[(i-okno):(i-1)])^2))/okno
}
names(dax) <- c("data", "zamkniecie.dax", "zwrot.dax", "var.dax")

wig <- wig[-1,]
wig$wariancja <- NaN
okno <- 30
for(i in c((okno+1):(nrow(wig)))){
  wig$wariancja[i] <- (sum((wig$zwroty[(i-okno):(i-1)])^2))/okno
}
names(wig) <- c("data", "zamkniecie.wig", "zwrot.wig", "var.wig")


smi <- smi[-1,]
smi$wariancja <- NaN
okno <- 30
for(i in c((okno+1):(nrow(smi)))){
  smi$wariancja[i] <- (sum((smi$zwroty[(i-okno):(i-1)])^2))/okno
}
names(smi) <- c("data", "zamkniecie.smi", "zwrot.smi", "var.smi")


df <-  merge(dax[,-2], wig[,-2], by="data")
df <-  merge(df, smi[,-2], by="data")

df$cov.dax.wig <- NaN
df$cov.dax.smi <- NaN
df$cov.wig.smi <- NaN
df$cor.dax.wig <- NaN
df$cor.dax.smi <- NaN
df$cor.wig.smi <- NaN


for(i in c((okno+1):(nrow(df)))){
  df$cov.dax.wig[i]<- cov(df$zwrot.dax[(i-okno):(i-1)], df$zwrot.wig[(i-okno):(i-1)], method = 'pearson')
  df$cov.dax.smi[i]<- cov(df$zwrot.dax[(i-okno):(i-1)], df$zwrot.smi[(i-okno):(i-1)], method = 'pearson')
  df$cov.wig.smi[i]<- cov(df$zwrot.wig[(i-okno):(i-1)], df$zwrot.smi[(i-okno):(i-1)], method = 'pearson')
  
  df$cor.dax.wig[i]<- cor(df$zwrot.dax[(i-okno):(i-1)], df$zwrot.wig[(i-okno):(i-1)], method = 'pearson')
  df$cor.dax.smi[i]<- cor(df$zwrot.dax[(i-okno):(i-1)], df$zwrot.smi[(i-okno):(i-1)], method = 'pearson')
  df$cor.wig.smi[i]<- cor(df$zwrot.wig[(i-okno):(i-1)], df$zwrot.smi[(i-okno):(i-1)], method = 'pearson')
}

df$data <- as.Date(df$data)

ewma <-  merge(dax[,-2], wig[,-2], by="data")
ewma <-  merge(ewma, smi[,-2], by="data")

ewma$cov.dax.wig <- NaN
ewma$cov.dax.smi <- NaN
ewma$cov.wig.smi <- NaN
ewma$cor.dax.wig <- NaN
ewma$cor.dax.smi <- NaN
ewma$cor.wig.smi <- NaN

ewma$cov.dax.wig[okno+1] <- df$cov.dax.wig[okno+1]
ewma$cov.dax.smi[okno+1] <- df$cov.dax.smi[okno+1]
ewma$cov.wig.smi[okno+1] <- df$cov.wig.smi[okno+1]

lambda <- 0.94
i<-okno+2
for(i in c((okno+2):(nrow(ewma)))){
  ewma$cov.dax.wig[i]<- lambda * ewma$cov.dax.wig[i-1] + 
    (1-lambda) * ewma$zwrot.dax[i-1] * ewma$zwrot.wig[i-1]
  ewma$cov.dax.smi[i]<- lambda * ewma$cov.dax.smi[i-1] + 
    (1-lambda) * ewma$zwrot.dax[i-1] * ewma$zwrot.smi[i-1]
  ewma$cov.wig.smi[i]<- lambda * ewma$cov.wig.smi[i-1] + 
    (1-lambda) * ewma$zwrot.wig[i-1] * ewma$zwrot.smi[i-1]
  
  ewma$var.dax[i]<- lambda * ewma$var.dax[i-1] + (1-lambda) * ewma$zwrot.dax[i-1]^2
  ewma$var.wig[i]<- lambda * ewma$var.wig[i-1] + (1-lambda) * ewma$zwrot.wig[i-1]^2
  ewma$var.smi[i]<- lambda * ewma$var.smi[i-1] + (1-lambda) * ewma$zwrot.smi[i-1]^2
  
  ewma$cor.dax.wig[i]<- ewma$cov.dax.wig[i]/sqrt(ewma$var.dax[i]*ewma$var.wig[i])
  ewma$cor.dax.smi[i]<- ewma$cov.dax.smi[i]/sqrt(ewma$var.dax[i]*ewma$var.smi[i])
  ewma$cor.wig.smi[i]<- ewma$cov.wig.smi[i]/sqrt(ewma$var.wig[i]*ewma$var.smi[i])
}


garch <-  merge(dax[,-2], wig[,-2], by="data")
garch <-  merge(garch, smi[,-2], by="data")

garch$cov.dax.wig <- NaN
garch$cov.dax.smi <- NaN
garch$cov.wig.smi <- NaN
garch$cor.dax.wig <- NaN
garch$cor.dax.smi <- NaN
garch$cor.wig.smi <- NaN

garch$cov.dax.wig[okno+1] <- df$cov.dax.wig[okno+1]
garch$cov.dax.smi[okno+1] <- df$cov.dax.smi[okno+1]
garch$cov.wig.smi[okno+1] <- df$cov.wig.smi[okno+1]

gamm <- 0.01
alfa <- 0.1
beta <- 0.89

long.cov.dax.wig <- gamm*(sum(df$zwrot.dax*df$zwrot.wig))/nrow(df)
long.cov.dax.smi <- gamm*(sum(df$zwrot.dax*df$zwrot.smi))/nrow(df)
long.cov.wig.smi <- gamm*(sum(df$zwrot.wig*df$zwrot.smi))/nrow(df)

long.var.wig <- gamm*(sum(df$zwrot.wig^2))/nrow(df)
long.var.dax <- gamm*(sum(df$zwrot.dax^2))/nrow(df)
long.var.smi <- gamm*(sum(df$zwrot.smi^2))/nrow(df)

for(i in c((okno+2):(nrow(garch)))){
  
  garch$cov.dax.wig[i]<- long.cov.dax.wig + 
    alfa * garch$zwrot.dax[i-1] * garch$zwrot.wig[i-1] +
    beta * garch$cov.dax.wig[i-1]
  
  garch$cov.dax.smi[i]<- long.cov.dax.smi + 
    alfa * garch$zwrot.dax[i-1] * garch$zwrot.smi[i-1] +
    beta * garch$cov.dax.smi[i-1]
  
  garch$cov.wig.smi[i]<- long.cov.wig.smi + 
    alfa * garch$zwrot.smi[i-1] * garch$zwrot.wig[i-1] +
    beta * garch$cov.wig.smi[i-1]
  
  
  garch$var.dax[i]<- long.var.dax + alfa * garch$zwrot.dax[i-1]^2 + beta * garch$var.dax[i-1]
  garch$var.wig[i]<- long.var.wig + alfa * garch$zwrot.wig[i-1]^2 + beta * garch$var.wig[i-1]
  garch$var.smi[i]<- long.var.smi + alfa * garch$zwrot.smi[i-1]^2 + beta * garch$var.smi[i-1]
  
  garch$cor.dax.wig[i]<- garch$cov.dax.wig[i]/sqrt(garch$var.dax[i]*garch$var.wig[i])
  garch$cor.dax.smi[i]<- garch$cov.dax.smi[i]/sqrt(garch$var.dax[i]*garch$var.smi[i])
  garch$cor.wig.smi[i]<- garch$cov.wig.smi[i]/sqrt(garch$var.wig[i]*garch$var.smi[i])
}

cor.df <- data.frame(data=as.Date(df$data),
                     pearson.dax.smi = df$cor.dax.smi,
                     pearson.dax.wig = df$cor.dax.wig,
                     pearson.wig.smi = df$cor.wig.smi,
                     ewma.dax.smi = ewma$cor.dax.smi,
                     ewma.dax.wig = ewma$cor.dax.wig,
                     ewma.wig.smi = ewma$cor.wig.smi,
                     garch.dax.smi = garch$cor.dax.smi,
                     garch.dax.wig = garch$cor.dax.wig,
                     garch.wig.smi = garch$cor.wig.smi)
cor.df <- cor.df[-1,]

cov.df <- data.frame(data=as.Date(df$data),
                     pearson.dax.smi = df$cov.dax.smi,
                     pearson.dax.wig = df$cov.dax.wig,
                     pearson.wig.smi = df$cov.wig.smi,
                     ewma.dax.smi = ewma$cov.dax.smi,
                     ewma.dax.wig = ewma$cov.dax.wig,
                     ewma.wig.smi = ewma$cov.wig.smi,
                     garch.dax.smi = garch$cov.dax.smi,
                     garch.dax.wig = garch$cov.dax.wig,
                     garch.wig.smi = garch$cov.wig.smi)

t <- 3919
cm <- matrix(data=1, nrow=3, ncol=3)
colnames(cm) <- c("dax", "smi", "wig")
rownames(cm) <- c("dax", "smi", "wig")

for(i in c(1:3)){
  for(j in c(1:i)){
    if(i!=j){
      cm[i,j] <- cor.df[t, (i+j-1)] 
      cm[j,i] <- cm[i,j]
    }
  }
}


g1 <- ggplot(data = cor.df, aes(x = data)) +
  geom_line(aes(y = pearson.dax.wig), size=.3, alpha=.9) + 
  xlab("Data") + ylab("Korelacja Pearsona") + 
  labs(title="Korelacja Pearsona") + 
  scale_x_date(date_breaks = "2 years") + theme_bw() + ylim(-0.6,1.2)

g2 <- ggplot(data = cor.df, aes(x = data)) +
  geom_line(aes(y = ewma.dax.wig), size=.3, alpha=.9) + 
  xlab("Data") + ylab("Korelacja EWMA") + 
  labs(title="Korelacja wg modelu EWMA") + 
  scale_x_date(date_breaks = "2 years") + theme_bw() + ylim(-0.6,1.2)

g3 <- ggplot(data = cor.df, aes(x = data)) +
  geom_line(aes(y = garch.dax.wig), size=.3, alpha=.9) + 
  xlab("Data") + ylab("Korelacja GARCH") + 
  labs(title="Korelacja wg modelu GARCH(1,1)") + 
  scale_x_date(date_breaks = "2 years") + theme_bw() + ylim(-0.6,1.2)

grid.arrange(g1, g2, g3,
             top = textGrob("Zmiana korelacji w czasie pomiedzy stopami zwrotu indeksow gieldowych DAX i WIG", 
                            gp=gpar(fontsize=17)))


g1 <- ggplot(data = cor.df, aes(x = data)) +
  geom_line(aes(y = pearson.dax.smi), size=.4, alpha=.9) + 
  xlab("Data") + ylab("Korelacja Pearsona") + 
  labs(title="Korelacja Pearsona") + 
  scale_x_date(date_breaks = "2 years") + theme_bw() + ylim(-0.6,1.2)

g2 <- ggplot(data = cor.df, aes(x = data)) +
  geom_line(aes(y = ewma.dax.smi), size=.4, alpha=.9) + 
  xlab("Data") + ylab("Korelacja EWMA") + 
  labs(title="Korelacja wg modelu EWMA") + 
  scale_x_date(date_breaks = "2 years") + theme_bw() + ylim(-0.6,1.2)

g3 <- ggplot(data = cor.df, aes(x = data)) +
  geom_line(aes(y = garch.dax.smi), size=.4, alpha=.9) + 
  xlab("Data") + ylab("Korelacja GARCH") + 
  labs(title="Korelacja wg modelu GARCH(1,1)") + 
  scale_x_date(date_breaks = "2 years") + theme_bw() + ylim(-0.6,1.2)

grid.arrange(g1, g2, g3,
             top = textGrob("Zmiana korelacji w czasie pomiedzy stopami zwrotu indeksow gieldowych DAX i SMI", 
                            gp=gpar(fontsize=17)))


g1 <- ggplot(data = cor.df, aes(x = data)) +
  geom_line(aes(y = pearson.wig.smi), size=.4, alpha=.9) + 
  xlab("Data") + ylab("Korelacja Pearsona") + 
  labs(title="Korelacja Pearsona") + 
  scale_x_date(date_breaks = "2 years") + theme_bw() + ylim(-0.6,1.2)

g2 <- ggplot(data = cor.df, aes(x = data)) +
  geom_line(aes(y = ewma.wig.smi), size=.4, alpha=.9) + 
  xlab("Data") + ylab("Korelacja EWMA") + 
  labs(title="Korelacja wg modelu EWMA") + 
  scale_x_date(date_breaks = "2 years") + theme_bw() + ylim(-0.6,1.2)

g3 <- ggplot(data = cor.df, aes(x = data)) +
  geom_line(aes(y = garch.wig.smi), size=.4, alpha=.9) + 
  xlab("Data") + ylab("Korelacja GARCH") + 
  labs(title="Korelacja wg modelu GARCH(1,1)") + 
  scale_x_date(date_breaks = "2 years") + theme_bw() + ylim(-0.6,1.2)

grid.arrange(g1, g2, g3,
             top = textGrob("Zmiana korelacji w czasie pomiedzy stopami zwrotu indeksow gieldowych WIG i SMI", 
                            gp=gpar(fontsize=17)))


## Zmiennoœæ portfela

zmiennosc <- function(v, m=30){
  n <- length(v)
  s <- c()
  s[1:m] <- NaN
  
  for(i in c((m+1):n)){
    s[i] <- sum(v[(i-m):(i-1)]^2)/length(v[(i-m):(i-1)])
  }
  return(s)
}

EWMA <- function(v, lambda=.94, m=30){
  n <- length(v)
  s <- c()
  s[1:(m+1)] <- zmiennosc(v[1:(m+1)], m)
  for(i in c((m+2):n)){
    s[i] <- lambda*s[i-1] + (1-lambda)*v[i-1]^2
  }
  return(s)
}

GARCH <- function(v, gamm=0.01, alfa=0.1, beta=0.89, m=30){
  n <- length(v)
  V_L <- gamm*var(v)
  s <- c()
  s[1:(m+1)] <- zmiennosc(v[1:(m+1)], m)
  
  for(i in c((m+2):n)){
    s[i] <- V_L + alfa*v[i-1]^2 + beta*s[i-1]
  }
  return(s)
}


okno <- 250

# Zmiennosc wariancja
zmiennosc.dax <- sqrt(zmiennosc(df$zwrot.dax,m=okno))
zmiennosc.wig <- sqrt(zmiennosc(df$zwrot.wig, m=okno))
zmiennosc.smi <- sqrt(zmiennosc(df$zwrot.smi, m=okno))

zmiennosci <- data.frame(zmiennosc.dax, zmiennosc.smi, zmiennosc.wig)
zmiennosci <- zmiennosci[-1,]

# Zmiennosc EWMA
zmiennosc.dax <- sqrt(EWMA(df$zwrot.dax, m=okno))
zmiennosc.wig <- sqrt(EWMA(df$zwrot.wig, m=okno))
zmiennosc.smi <- sqrt(EWMA(df$zwrot.smi, m=okno))

zmiennosci.ewma <- data.frame(zmiennosc.dax, zmiennosc.smi, zmiennosc.wig)
zmiennosci.ewma <- zmiennosci.ewma[-1,]

# Zmiennosc GARCH
zmiennosc.dax <- sqrt(GARCH(df$zwrot.dax, m=okno))
zmiennosc.wig <- sqrt(GARCH(df$zwrot.wig, m=okno))
zmiennosc.smi <- sqrt(GARCH(df$zwrot.smi, m=okno))

zmiennosci.garch <- data.frame(zmiennosc.dax, zmiennosc.smi, zmiennosc.wig)
zmiennosci.garch <- zmiennosci.garch[-1,]

udzial <- 1/3

cor.matrix <- matrix(data=1, nrow=3, ncol=3)
colnames(cor.matrix) <- c("dax", "smi", "wig")
rownames(cor.matrix) <- c("dax", "smi", "wig")

cor.matrix.ewma <- cor.matrix
cor.matrix.garch <- cor.matrix

wallet.var <- matrix(data = 0, nrow = nrow(cor.df), ncol = 1)
wallet.var.ewma <- matrix(data = 0, nrow = nrow(cor.df), ncol = 1)
wallet.var.garch <- matrix(data = 0, nrow = nrow(cor.df), ncol = 1)

for(t in c((okno+1):nrow(cor.df))){
  
  for(i in c(1:3)){
    for(j in c(1:i)){
      if(i!=j){
        cor.matrix[i,j] <- cor.df[t, (i+j-1)] 
        cor.matrix[j,i] <- cor.matrix[i,j]
        
        cor.matrix.ewma[i,j] <- cor.df[t, (i+j-1+3)] 
        cor.matrix.ewma[j,i] <- cor.matrix.ewma[i,j]
        
        cor.matrix.garch[i,j] <- cor.df[t, (i+j-1+6)] 
        cor.matrix.garch[j,i] <- cor.matrix.garch[i,j]
      }
    }
  }
  
  for(i in c(1:3)){
    for(j in c(1:3)){
      
      wallet.var[t] <- wallet.var[t] + 
        cor.matrix[i,j] * udzial^2 * 
        zmiennosci[t, i] * zmiennosci[t, j]
      
      wallet.var.ewma[t] <- wallet.var.ewma[t] + 
        cor.matrix.ewma[i,j] * udzial^2 * 
        zmiennosci.ewma[t, i] * zmiennosci.ewma[t, j]
      
      wallet.var.garch[t] <- wallet.var.garch[t] + 
        cor.matrix.garch[i,j] * udzial^2 * 
        zmiennosci.garch[t, i] * zmiennosci.garch[t, j]
    }
  }
}

wallet.sd <- sqrt(wallet.var)
wallet.sd.ewma <- sqrt(wallet.var.ewma)
wallet.sd.garch <- sqrt(wallet.var.garch)

zmien.portfela <- data.frame("data"=as.Date(cor.df$data), "Pearson" = wallet.sd, "EWMA" = wallet.sd.ewma, "GARCH" = wallet.sd.garch)
zmien.portfela <- zmien.portfela[(okno+1):nrow(zmien.portfela),]

d1 <- (1/3*zmiennosci$zmiennosc.dax + 1/3*zmiennosci$zmiennosc.wig + 1/3*zmiennosci$zmiennosc.smi) - (wallet.sd)
d2 <- (1/3*zmiennosci.ewma$zmiennosc.dax + 1/3*zmiennosci.ewma$zmiennosc.wig + 1/3*zmiennosci.ewma$zmiennosc.smi) - (wallet.sd.ewma)
d3 <- (1/3*zmiennosci.garch$zmiennosc.dax + 1/3*zmiennosci.garch$zmiennosc.wig + 1/3*zmiennosci.garch$zmiennosc.smi) - (wallet.sd.garch)

delta.zmiennosc <- data.frame("data"=df$data[-1],  "Pearson" =d1, "EWMA" = d2, "GARCH" = d3)
delta.zmiennosc <- delta.zmiennosc[(okno+1):nrow(delta.zmiennosc),]


g1 <- ggplot(data = zmien.portfela, aes(x = data)) +
  geom_line(aes(y = Pearson), size=.4, alpha=.9) + 
  xlab("") + ylab("") + ylim(0, 0.04) +
  labs(title="Korelacja Pearsona, zmiennoœæ stóp zwrotu jako wariancja") + 
  scale_x_date(date_breaks = "2 years") + theme_bw()

g2 <- ggplot(data = zmien.portfela, aes(x = data)) +
  geom_line(aes(y = EWMA), size=.4, alpha=.9) + 
  xlab("") + ylab(" EWMA") + ylim(0, 0.04) +
  labs(title="Korelacja oraz zmiennoœæ stóp zwrotu wg modelu EWMA") + 
  scale_x_date(date_breaks = "2 years") + theme_bw()

g3 <- ggplot(data = zmien.portfela, aes(x = data)) +
  geom_line(aes(y = GARCH), size=.4, alpha=.9) + 
  xlab("") + ylab("") + ylim(0, 0.04) +
  labs(title="Korelacja oraz zmiennoœæ stóp zwrotu wg modelu GARCH(1,1)") + 
  scale_x_date(date_breaks = "2 years") + theme_bw()

grid.arrange(g1, g2, g3,
             top = textGrob("Zmiennoœæ portfela", 
                            gp=gpar(fontsize=16)))


g1 <- ggplot(data = delta.zmiennosc, aes(x = data)) +
  geom_line(aes(y = Pearson), size=.4, alpha=.9) + 
  xlab("") + ylab("") + ylim(-.001, 0.006) +
  labs(title="Korelacja Pearsona, zmiennoœæ stóp zwrotu jako wariancja") + 
  scale_x_date(date_breaks = "2 years") + theme_bw()

g2 <- ggplot(data = delta.zmiennosc, aes(x = data)) +
  geom_line(aes(y = EWMA), size=.4, alpha=.9) + 
  xlab("") + ylab(" EWMA") + ylim(-.001, 0.006) +
  labs(title="Korelacja oraz zmiennoœæ stóp zwrotu wg modelu EWMA") + 
  scale_x_date(date_breaks = "2 years") + theme_bw()

g3 <- ggplot(data = delta.zmiennosc, aes(x = data)) +
  geom_line(aes(y = GARCH), size=.4, alpha=.9) + 
  xlab("") + ylab("") + ylim(-.001, 0.006) +
  labs(title="Korelacja oraz zmiennoœæ stóp zwrotu wg modelu GARCH(1,1)") + 
  scale_x_date(date_breaks = "2 years") + theme_bw()

grid.arrange(g1, g2, g3,
             top = textGrob("Korzyœci z dywersyfikacji portfela\nRó¿nica pomiêdzy sum¹ zmiennoœci poszczególnych walorów a zmiennoœci¹ ca³ego portfela", 
                            gp=gpar(fontsize=16)))



#### VaR

VaR.pearson.95 <- c()
VaR.ewma.95 <- c()
VaR.garch.95 <- c()


for(i in c(1:(nrow(zmien.portfela)))){
  VaR.pearson.95[i] <- zmien.portfela$Pearson[i]*qnorm(.95)
  VaR.ewma.95[i] <- zmien.portfela$EWMA[i]*qnorm(.95)
  VaR.garch.95[i] <- zmien.portfela$GARCH[i]*qnorm(.95)
}


VaR <- data.frame("data"=as.Date(zmien.portfela$data),
                  "portfolio.change" = - (as.matrix(df[(okno+2):nrow(df),c(2,4,6)]) %*% c(1/3, 1/3, 1/3)),
                  "Pearson.95" = VaR.pearson.95,
                  "EWMA.95" = VaR.ewma.95,
                  "GARCH.95" = VaR.garch.95)


highlight_df <- VaR %>% 
  filter(portfolio.change > Pearson.95)

g1 <- ggplot(VaR, aes(data)) + 
  geom_line(aes(y = Pearson.95), size=.7, color="red", alpha=.7) + 
  geom_line(aes(y = portfolio.change), col="#949494", alpha=.7, size=.5)+
  geom_point(data=highlight_df, aes(x=data, y=portfolio.change), color='black', size=1, alpha=.5) +
  xlab("") + ylab("") +
  labs(title="95% VaR dla portfela - Pearson") +
  scale_x_date(date_breaks = "2 years")  + theme_bw()

highlight_df <- VaR %>% 
  filter(portfolio.change > EWMA.95)

g2 <- ggplot(VaR, aes(data)) + 
  geom_line(aes(y = EWMA.95), size=.7, color="red", alpha=.7) + 
  geom_line(aes(y = portfolio.change), col="#949494", alpha=.7, size=.5)+
  geom_point(data=highlight_df, aes(x=data, y=portfolio.change), color='black', size=1, alpha=.5) +
  xlab("") + ylab("") +
  labs(title="95% VaR dla portfela - EWMA") +
  scale_x_date(date_breaks = "2 years")  + theme_bw()

highlight_df <- VaR %>% 
  filter(portfolio.change > GARCH.95)

g3 <- ggplot(VaR, aes(data)) + 
  geom_line(aes(y = GARCH.95), size=.7, color="red", alpha=.7) + 
  geom_line(aes(y = portfolio.change), col="#949494", alpha=.7, size=.5)+
  geom_point(data=highlight_df, aes(x=data, y=portfolio.change), color='black', size=1, alpha=.5) +
  xlab("") + ylab("") +
  labs(title="95% VaR dla portfela - GARCH") +
  scale_x_date(date_breaks = "2 years")  + theme_bw()

grid.arrange(g1, g2, g3)



## Testowanie wsteczne

p1 <-  sum(VaR$portfolio.change > VaR$Pearson.95)/length(VaR$portfolio.change)*100
p2 <-  sum(VaR$portfolio.change > VaR$EWMA.95)/length(VaR$portfolio.change)*100
p3 <-  sum(VaR$portfolio.change > VaR$GARCH.95)/length(VaR$portfolio.change)*100

library(GAS)
p.values <- matrix(data=0, nrow=nrow(VaR) - 1 - okno, ncol=3)

for(i in c((okno+1):(nrow(VaR)-1))){
  b <- BacktestVaR(VaR$portfolio.change[(i-okno):(i-1)], VaR$Pearson.95[(i-okno):(i-1)], 0.95)
  p.values[i-okno,1] <- b$LRuc["Pvalue"]
  b <- BacktestVaR(VaR$portfolio.change[(i-okno):(i-1)], VaR$EWMA.95[(i-okno):(i-1)], 0.95)
  p.values[i-okno,2] <-  b$LRuc["Pvalue"]
  b <- BacktestVaR(VaR$portfolio.change[(i-okno):(i-1)], VaR$GARCH.95[(i-okno):(i-1)], 0.95)
  p.values[i-okno,3] <-  b$LRuc["Pvalue"]
}

kupiec.pearson <- sum(p.values[,1] > 0.05)/nrow(p.values)*100
kupiec.ewma <- sum(p.values[,2] > 0.05)/nrow(p.values)*100
kupiec.garch <- sum(p.values[,3] > 0.05)/nrow(p.values)*100

christ.pearson <-  BacktestVaR(VaR$portfolio.change, VaR$Pearson.95, .95)$LRcc["Pvalue"]
christ.ewma <-  BacktestVaR(VaR$portfolio.change, VaR$EWMA.95, .95)$LRcc["Pvalue"]
christ.garch <-  BacktestVaR(VaR$portfolio.change, VaR$GARCH.95, .95)$LRcc["Pvalue"]

backtests <- matrix(data=0, nrow=3, ncol=3)
colnames(backtests) <- c("Pearson", "EWMA", "GARCH")
rownames(backtests) <- c("% przekroczeñ VaR", 
                         "test Kupca (% okien z p-value < 0.05)", 
                         "test Christoffersena (p-value)")

backtests[1,] <- c(round(p1,3), round(p2,3), round(p3,3))
backtests[2,] <- c(round(kupiec.pearson,3),round(kupiec.ewma,3), round(kupiec.garch,3))
backtests[3,] <- c(round(christ.pearson,3), round(christ.ewma,3), round(christ.garch,3))






