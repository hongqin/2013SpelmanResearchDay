
Npop = 1e5; 
% y = randraw('norm', [0, 1], 1, Npop) %not sure how this works??
y = randraw('exp', 1, Npop) %not sure how this works??
Nbins = 100; 


%2012 Sep 25. We need convert the empirical cdf section into a function


hist(y, Nbins)
h = hist(y, Nbins)

mypdf = h / Npop
mycdf = 1:Nbins
mybins = 1:Nbins
step = abs(max(y)-min(y))/ Nbins
for i = 1:Nbins
    mycdf(i) = sum( mypdf(1:i)); 
    mybins(i) = min(y) + (i-0.5)*step;
end


plot( mybins , mycdf);
