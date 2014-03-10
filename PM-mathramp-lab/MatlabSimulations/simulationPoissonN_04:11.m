%function  [lifespan] = rnorm(x,mean, sd)
 % function lifespan calculates the lifespan
 
 %x=1000;
 %mean=100;
 %sd=10;
 %ret = calculate.s.m.v2(lifespan)
 
 Nsim = 10
 CV = 1:Nsim
 
 for s = 1:Nsim
     
     Npop=100; % numOfSystems (individuals)
     lifespan = 1:Npop

     for nn = 1:Npop %loop over Nop individuals
         m=15;  % numOfBlocks in a system
         n = randraw('Poisson', 5, m) %this can give zero-element block
         n(n==0) = 1 %this is not Poisson anymore, but fixes the problem

         mymean = 0.1; % for expontial age of elements.
         %mysds = 0.1;

         BlockAges = 1:m % buffer for temporary storage
         for i=1:m 
            ElementAges = randraw('exp', 0.1, n(i))
            BlockAges(i) = max(ElementAges)
         end
         IndividualSystemLifespan = min(BlockAges)
         lifespan(nn) = IndividualSystemLifespan
     end
     
     hist(lifespan,20)
     
 m = mean(lifespan)% calcualte the mean
 s = std(lifespan)% calculate std dev
 CV = s/m
 
 end
 
 hist(CV,20)
 



