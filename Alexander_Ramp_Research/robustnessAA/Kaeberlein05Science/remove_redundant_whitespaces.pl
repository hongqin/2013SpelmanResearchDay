#!/usr/bin/perl
# v0 Nov 21, 05 Hong Qin

use strict; use warnings; 

my $debug = 1;
my $infile = $ARGV[0]; my $outfile = $ARGV[1];  my $line;

if (! $ARGV[1]) {   print "Usage perl $0 infile outfile \n";      exit(0);}

open (IN, "<$infile");
open (TMP, ">_tmp.tab");

while ( $line = <IN> ) {
  $line =~ s/\ +//g;
  print TMP $line;
}
close (IN); close (TMP);

open (TMP, "<_tmp.tab");
open (OUT, ">$outfile");
while ( $line = <TMP> ) {
  if ( $line !~ /^\s*\n/ ) {
    print OUT $line;
  }
}
close (TMP); close (OUT);
exit;
