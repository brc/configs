#!/usr/bin/perl

die "give me a .vim file\n" if ! defined @ARGV;

open FILE, "<$ARGV[0]" or die "could not open $ARGV[0]: $!\n";;
open NEWFILE, ">$ARGV[0].new";

#while (<FILE>) {
#   if (/^imap\s+<buffer>\s+(\.+.*?)\s+(.*)$/i) {
#      print NEWFILE;
#      print NEWFILE "map <buffer> $1 i$2\n";
#   }
#   else {
#      print NEWFILE;
#   }
#}
#close NEWFILE;

while (<FILE>)
{
   if (/^(i?map)\s+<buffer>\s+,(.*?)\s+(.*)$/i)
   {
      print NEWFILE "$1 <buffer> ..$2 $3\n";
   }
   elsif (/^(i?map)\s+<buffer>\s+\.\.(.*?)\s+(.*)$/i)
   {
      print NEWFILE "$1 <buffer> .$2 $3\n";
   }
   elsif (/^(i?map)\s+<buffer>\s+\.(.*?)\s+(.*)$/i)
   {
      print NEWFILE "$1 <buffer> ,$2 $3\n";
   }
   else
   {
      print NEWFILE;
   }
}
close NEWFILE;
close FILE;
