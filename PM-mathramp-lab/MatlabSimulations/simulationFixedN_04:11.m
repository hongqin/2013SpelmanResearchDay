%function  [lifespan] = rnorm(x,mean, sd)
 % function lifespan calculates the lifespan
 
 %x=1000;
 %mean=100;
 %sd=10;
 %ret = calculate.s.m.v2(lifespan)
 
 Nsim = 10
 CV = 1:Nsim
 
 for s = 1:Nsim

     Npop=1000; % numOfSystems (individuals)
     lifespan = 1:Npop

     for nn = 1:Npop 
         m=15;  % numOfBlocks in a system
         n = 5 %fixed number of elements in each block

         mymean = 0.1; % for expontial age of elements.
         %mysds = 0.1;

         ElementAges = randraw('exp', 0.1, m*n)

         BlockAges = 1:m % buffer for temporary storage
         for i=1:m 
             subElementAges = ElementAges((1+(i-1)*n):i*n)
             BlockAges(i) = max(subElementAges)
         end
         IndividualSystemLifespan = min(BlockAges)
         lifespan(nn) = IndividualSystemLifespan
     end

hist(lifespan,20)

M = mean(lifespan)
S = std(lifespan)
CV(s) = S/M

 end
 
 hist(CV,20)
