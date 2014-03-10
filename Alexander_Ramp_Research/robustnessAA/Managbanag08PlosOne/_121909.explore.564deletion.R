fitness = read.table("Regression_Tc2_hom.txt",header=T, sep="\t");
#fitness = read.table("Regression_Tc2_het.txt",header=T, sep="\t"); #not informative
fitness$orf = as.character(fitness$orf);

tb = read.csv("RLS.of.564.gene.deletion.BY.Managbanag08Plos.csv");
tb = tb[ ! is.na(tb$RLS_Del_alpha), ]
tb$ORF = as.character(tb$ORF);
tb$YPD = fitness$YPD[ match( tb$ORF, fitness$orf) ];
tb$YPG = fitness$YPG[ match( tb$ORF, fitness$orf) ];
tb$YPDGE = fitness$YPGDE[ match( tb$ORF, fitness$orf) ];
tb$YPE = fitness$YPE[ match( tb$ORF, fitness$orf) ];
tb$YPL = fitness$YPL[ match( tb$ORF, fitness$orf) ];
tb$YPD0.1DNP = fitness$YPD0.1DNP[ match( tb$ORF, fitness$orf) ];
tb$YPD0.25DNP = fitness$YPD0.25DNP[ match( tb$ORF, fitness$orf) ];
tb$YPG0.25DNP = fitness$YPG0.25DNP[ match( tb$ORF, fitness$orf) ];
tb$YPG0.1DNP = fitness$YPG0.1DNP[ match( tb$ORF, fitness$orf) ];

tb$d.vs.4742 = tb$RLS_Del_alpha/tb$BY4742

pdf("_123009.RLS-YPE.pdf", width=6,height=6);
summary(lm(tb$d.vs.4742 ~ tb$YPD + tb$YPG + tb$YPE + tb$YPL  ))
summary(lm(tb$d.vs.4742 ~ tb$YPE  )) #p = 0.00159
plot( tb$d.vs.4742 ~ tb$YPE, xlab='fitness in YPE', ylab='RLS' );
abline( lm(tb$d.vs.4742 ~ tb$YPE  ), col="red");
dev.off();


long = tb[ (tb$RLS_Del_alpha/tb$BY4742)>1 & tb$p.value<0.05, ]
short= tb[ (tb$RLS_Del_alpha/tb$BY4742)<1 & tb$p.value<0.05, ]

summary(lm(long$d.vs.4742 ~ long$YPD + long$YPG + long$YPE + long$YPL  ))
summary(lm(long$d.vs.4742 ~ long$YPD   ))
summary(lm(long$d.vs.4742 ~ long$YPD / long$YPG   ))

summary(lm(short$d.vs.4742 ~ short$YPD + short$YPG + short$YPE + short$YPL  ))
