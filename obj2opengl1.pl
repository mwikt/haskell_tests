#!/usr/bin/perl

# load a file

open FH,$ARGV[0];

@data=<FH>;

@regions=grep(/^o/,@data);
print STDERR "n.regions $#regions \n";
# how many points
@points=grep(/^[v][ ]/,@data);
@normals=grep(/^[v][n]/,@data);


# how many faces
@faces=grep(/^[f]/,@data);

@groups=();


#cosmetics
map {$_=~ s/^v[ \t]+//g } @points;
map {chomp } @points;
map {$_=~ s/^f[ \t]+//g } @faces;
map {chomp } @faces;
map {$_=~ s/^vn[ \t]+//g } @normals;
map {chomp } @normals;


foreach (@faces) {
        @corners=split(/ +/,$_);
        foreach (@corners) {$_ =~ s/[\/].+$//g; 
                            $tmp=$points[($_)-1];
                            $tmp=~s/([\-0-9\.]+)/($1)::GLfloat/g;
                            $tmp=~s/[ ]+/,/g;
                            $tmp='('.$tmp."),";
                            print " ".($tmp)."\n";}
#        foreach (@corners) {$_ =~ s/[\/].+$//g; print " ".$_." ";}
#        print "\n";
}

print "\n\n\n\n";

foreach (@faces) {
        @corners=split(/ +/,$_);
        foreach (@corners) {
                 $_ =~ s/[0-9]+\/\///;
                 $tmp=$normals[($_)-1];
                 $tmp=~s/([\-0-9\.]+)/($1)::GLfloat/g;
                 $tmp=~s/[ ]+/,/g;
                 $tmp='('.$tmp."),";
                 $_ =~ s/^.+[\/]//g; print "  ".($tmp)."\n";}
#                 print " ".($tmp)."\n";}
#        foreach (@corners) {$_ =~ s/[\/].+$//g; print " ".$_." ";}
#        print "\n";
}


#print "@faces";

