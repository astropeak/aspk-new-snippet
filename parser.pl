use Marpa::R2;

my $dsl =sub {open my $fh, '<', $_[0] or die "Can not open file"; local $/; <$fh>;}
->('grammer.slif');

my $grammar = Marpa::R2::Scanless::G->new( { source => \$dsl } );
my $input = sub {open my $fh, '<', $_[0] or die "Can not open file"; local $/; <$fh>;}
->('input-src');

# my $value_ref = $grammar->parse( \$input, 'My_Actions' );

my $slr = Marpa::R2::Scanless::R->new(
    { grammar => $grammar, semantics_package => 'My_Actions' } );

my $length = length $input;
my $length_read    = $slr->read( \$input );
while ($length_read < $length) {
    for my $event ( @{ $slr->events() } ) {
        my ($name) = @{$event};
        print "Event $name\n";
    }
    $length_read = $slr->resume();
}
my $value_ref = $slr->value;

# my $value = ${$value_ref};
# print "value: $value\n"; 


# my $actual_events = q{};

#  READ: while (1) {

#      my @actual_events = ();

#    EVENT:
#      for my $event ( @{ $slr->events() } ) {
#          my ($name) = @{$event};
#          push @actual_events, $name;
#    }

#      if (@actual_events) {
#          $actual_events .= join q{ }, "Events at position $pos:", @actual_events;
#          $actual_events .= "\n";
#          print "event: $actual_events";
#    }

#      if ($pos < $length) {
#          $pos = $slr->resume();
#          next READ;
#    }
#      last READ;
# } ## end READ: while (1)




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
