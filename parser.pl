use Marpa::R2;

my $dsl =sub {open my $fh, '<', $_[0] or die "Can not open file"; local $/; <$fh>;}
->('grammer.slif');

my $grammar = Marpa::R2::Scanless::G->new( { source => \$dsl } );
my $input = sub {open my $fh, '<', $_[0] or die "Can not open file"; local $/; <$fh>;}
->('input-src');

my $value_ref = $grammar->parse( \$input, 'My_Actions' );

sub My_Actions::do_add {
    my ( undef, $t1, undef, $t2 ) = @_;
    return $t1 + $t2;
}

sub My_Actions::do_multiply {
    my ( undef, $t1, undef, $t2 ) = @_;
    return $t1 * $t2;
}
sub My_Actions::do_if {
    my ( undef, $judgement, $body, $end ) = @_;
    # print "judgement: $judgement\nbody: $body\nend: $end\n";
    print "if statement end here\n\n";
}
sub My_Actions::do_for {
    my ( undef, $judgement, $body, $end ) = @_;
    # print "judgement: $judgement\nbody: $body\nend: $end\n";
    print "for statement end here\n\n";
}
sub My_Actions::do_expr_elem {
    print "do_expr_elem statement end here\n\n";
}
sub My_Actions::do_expr_and {
    print "do_expr_and statement end here\n\n";
}
sub My_Actions::do_expr_or {
    print "do_expr_or statement end here\n\n";
}
sub My_Actions::do_expr_paren {
    print "do_expr_paren statement end here\n\n";
}

sub My_Actions::do_expr_elem {
    print "do_expr_elem statement end here\n\n";
}
sub My_Actions::do_body {
    shift @_;
    for my $b (@_) {
        print "body line: $b\n";
    }
}
sub My_Actions::AUTOLOAD {
    my $program = $AUTOLOAD;
    print "name: $AUTOLOAD \n";
    # $program =~ s/.*:://;
    # system($program, @_);
}

sub My_Actions::do_whitespace {
    print "do_whitespace statement end here\n\n";
}
