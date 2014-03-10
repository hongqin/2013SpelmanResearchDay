file = "RLS.of.564.gene.deletion.BY.Managbanag08Plos.csv";
tb = read.csv(file)
write.csv(tb$Gene, "_564gene.tab", quote=F, row.names=F, colnames=F)