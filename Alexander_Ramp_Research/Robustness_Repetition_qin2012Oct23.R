lifespan = read.csv("lifespan.csv");
fit = read.csv("growth.fitness.hom.csv");
fit$orf = as.character( fit$orf ); 
data = read.csv("Sce.Spa.KaKs.csv");
pairs = read.csv("pairs.csv");
pairs$ORF1 = as.character(pairs$ORF1);
pairs$ORF2 = as.character(pairs$ORF2);
ids = c(pairs$ORF1, pairs$ORF2);
degree = table(ids);
net = data.frame(degree);
str(net);
net$id = as.character(net$id);
head(data);
net[net$id=='YAL005C',]
net[net$id=='YAL012W',]
scmd = read.table( "mutant data.tab", sep="\t", header=T)

fit$Ka = data$Ka[match(fit$orf, data$orfname)];
m = lm(log(fit$YPE)~fit$Ka);
summary(m);
