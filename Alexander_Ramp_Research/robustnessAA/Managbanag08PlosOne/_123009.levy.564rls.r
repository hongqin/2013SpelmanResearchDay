tb = read.csv("RLS.of.564.gene.deletion.BY.Managbanag08Plos.csv");
tb = tb[ ! is.na(tb$RLS_Del_alpha), ]
tb$ORF = as.character(tb$ORF);
tb$d.vs.4742 = tb$RLS_Del_alpha/tb$BY4742
tb$YPD = fitness$YPD[ match( tb$ORF, fitness$orf) ];
tb$YPG = fitness$YPG[ match( tb$ORF, fitness$orf) ];
tb$YPDGE = fitness$YPGDE[ match( tb$ORF, fitness$orf) ];
tb$YPE = fitness$YPE[ match( tb$ORF, fitness$orf) ];
tb$YPL = fitness$YPL[ match( tb$ORF, fitness$orf) ];
tb$YPD0.1DNP = fitness$YPD0.1DNP[ match( tb$ORF, fitness$orf) ];
tb$YPD0.25DNP = fitness$YPD0.25DNP[ match( tb$ORF, fitness$orf) ];
tb$YPG0.25DNP = fitness$YPG0.25DNP[ match( tb$ORF, fitness$orf) ];
tb$YPG0.1DNP = fitness$YPG0.1DNP[ match( tb$ORF, fitness$orf) ];

levy = read.table( "levy08.tab", header=T, sep="\t", fill=T);
levy$ORF = as.character(levy$ORF);
levy$levyflag = 1;
hist( levy$Phenotypic_potential );
intersect( tb$ORF, levy$ORF );


tb$Phenotypic_potential = levy$Phenotypic_potential[match(tb$ORF, levy$ORF)]

sub = tb[tb$Phenotypic_potential>1 & ! is.na(tb$Phenotypic_potential), ]
str(sub)


x = levy[levy$Phenotypic_potential>1 , ]
x = x[ ! is.na(x[,1]), ]


cls = read.csv( "PowersRankedDeletions.csv" );
cls$ORF = as.character( cls$ORF );

intersect( tb$ORF, cls$ORF );





q("no");














g2g = read.table( "synthetic.lethals.tab2", sep="\t", fill=T);
g2g[,1] = as.character( g2g[,1] );
g2g[,2] = as.character( g2g[,2] );
table( c(g2g[,1], g2g[,2]) );
names(g2g) = c("orf1", "orf2");

synthetic.lethals = unique( sort( c(g2g[,1], g2g[,2]) ) )
syntb = data.frame( synthetic.lethals );
names(syntb) = c("orf");
syntb$orf = as.character( syntb$orf );
syntb$flag = 1;

fitness = read.table("Regression_Tc2_hom.txt",header=T, sep="\t");
#fitness = read.table("Regression_Tc2_het.txt",header=T, sep="\t"); #not informative
fitness$orf = as.character(fitness$orf);
fitness$syn.lethal = syntb$flag[ match( fitness$orf, syntb$orf ) ]

synFit = fitness[ fitness$syn.lethal==1 & ( ! is.na(fitness$syn.lethal)), ]
hist(synFit$YPD);

g2g$YPD1 = fitness$YPD[ match(g2g$orf1, fitness$orf) ]
g2g$YPD2 = fitness$YPD[ match(g2g$orf2, fitness$orf) ]

jpeg("g2g.fitness.122309.jpg")
plot( g2g$YPD1 ~ g2g$YPD2, col="red", xlab = "fitness 1", ylab="fitness 2");
dev.off();


g2g$d.vs.4742.1 = tb$d.vs.4742[match(g2g$orf1, tb$ORF )]
g2g$d.vs.4742.2 = tb$d.vs.4742[match(g2g$orf2, tb$ORF )]
plot( g2g$d.vs.4742.2 ~ g2g$d.vs.4742.1 ) #only two points

synFit$d.vs.4742 = tb$d.vs.4742[match(synFit$orf, tb$ORF )]
hist( synFit$d.vs.4742 );

tb$syn.lethal = syntb$flag[ match( tb$ORF, syntb$orf ) ]
tb.sub = tb[ ! is.na(tb$syn.lethal), ]
tb.sub[ tb.sub$p.value < 0.05,  ] 

quit("no");

summary(lm(tb$d.vs.4742 ~ tb$YPD + tb$YPG + tb$YPE + tb$YPL  ))
summary(lm(tb$d.vs.4742 ~ tb$YPE  )) #p = 0.00159
plot( tb$d.vs.4742 ~ tb$YPE );
abline( lm(tb$d.vs.4742 ~ tb$YPE  ), col="red");


long = tb[ (tb$RLS_Del_alpha/tb$BY4742)>1 & tb$p.value<0.05, ]
short= tb[ (tb$RLS_Del_alpha/tb$BY4742)<1 & tb$p.value<0.05, ]

summary(lm(long$d.vs.4742 ~ long$YPD + long$YPG + long$YPE + long$YPL  ))
summary(lm(long$d.vs.4742 ~ long$YPD   ))
summary(lm(long$d.vs.4742 ~ long$YPD / long$YPG   ))

summary(lm(short$d.vs.4742 ~ short$YPD + short$YPG + short$YPE + short$YPL  ))
