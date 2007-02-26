#!/usr/bin/perl

# Pretty basic testing - build and run all the .e files in the directory
# and check that they give the expected output

opendir DIR, ".";
@files = readdir DIR;
closedir DIR;

system("echo \"\" > output");

foreach $file (sort @files) {
    if ($file=~/([^\.]+)\.e$/ && $file ne "Prelude.e") {
	print "$file...\n";
	system("epic $file -o $1 >> output");
	system("./$1 >> output");
	system("rm $1 $1.o $1.ei");
    }
}

$output = `cat output`;
$expected = `cat expected`;

print $output;

if ($output ne $expected) {
    print "ERRORS!!!!!\n";
} else {
    print "All OK\n";
}
