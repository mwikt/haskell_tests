#!/usr/bin/perl

# load a file

open FH,$ARGV[0];

@data=<FH>;

@regions=grep(/^o/,@data);
print STDERR "n.regions $#regions \n";
# how many points
@points=grep(/^[ov]/,@data);

# how many faces
@faces=grep(/^[of]/,@data);

@groups=();


$pind=21;
$f_ind=101;
$rind=201;

#cosmetics
map {$_=~ s/^v[ \t]+//g } @points;
map {chomp } @points;
map {$_=~ s/^f[ \t]+//g } @faces;
map {chomp } @faces;



# true triangular faces
@truefaces2= map {$_."\n"} @faces;
@truefaces= map {&nelem($_);} @truefaces2;

print "#section 1\n";
print " ".(($#points)-($#regions))." 3 0 1\n";

$nn=1;
$changed_group=0;
foreach (@points) {
        if ($_ =~ /^o/) {
                $pind++;
                $changed_group=1;
        }
        else {
                if ($changed_group==1) {
                        $changed_group=0;
                        push(@groups,$_);
                }
                @coords=split(/[\t ]+/,$_);
                @coords_m = map {$_ *1e-3 } @coords;
                $coord = join(' ',@coords_m);
                print "".$nn." ".$coord." ".$pind."\n";
                $nn++;
        }
}


print "#section 2\n";
print " ".(($#faces)-($#regions))."  1\n";

foreach (@faces) {
        if ($_ =~ /^o/) {
                $f_ind++;
        }
        else {
                $len=nelem($_)+1;
                print "1 0 $f_ind \n $len ".$_."\n";
        }
}
print "#section 3\n";
print "0\n";

print "#section 4\n";
print "".(($#groups)+1)."\n";

$pp=1;
foreach (@groups) {
        $pnt=$_;
        print "".$pp." ".(&mov($pnt))." ".$rind."\n";
        $rind++;
        $pp++;
}

#print "\n\n@truefaces\n\n";
#print " 1 ".&mov($points[1])."  \n";

sub mov {
        my $eps = 1e-5;
        my $inp=$_[0];
#        print "IIIIII $inp\n";
#        $inp =~ s/^[ov][ \t]+//;
        my @datas = split(/[ \t]+/,$inp);
        
        @dts = map {1e-3*($_+$eps)} @datas;

        $out=join (' ',@dts);

        $out
}




sub nelem {
        my @tab = split(/ +/,$_[0]);
#        print "IIIII $tab[0]:@tab\n";
        $#tab
}
