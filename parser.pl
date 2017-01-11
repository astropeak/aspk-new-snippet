use Marpa::R2;

my $dsl = <<'END_OF_DSL';
    :default ::= action => [name,values]
    lexeme default = latm => 1
    ifStatements ::=ifStatement*
    ifStatement ::= If Body End action => do_if
    Body ::= BodyLine* action => do_body
    End ::= optEnd | empty
    empty ::=
    optEnd ~ '@end'
    BodyLine ~ [a-z@ ] +
    # a line with only contains below @,i,f,and blanks will be considered as a lexeme If.
    # BodyLine can also match this pattern but it can contains other words.
    # but If has high priority, so if the matched string is the same, If will win.
    # Problem is: 'fi@' will also be matched as If.
    If ~ '@if' blanks_maybe
    blanks_maybe ~ [ \t]*

    :lexeme ~ optEnd priority=>1
    :lexeme ~ If priority=>2
    :discard ~ whitespace
    whitespace ~ [\s]+
END_OF_DSL

my $grammar = Marpa::R2::Scanless::G->new( { source => \$dsl } );
my $input = <<'END_OF_SN';
@if aa
    aa
@if      bbbb

@if  
    ee

@if
    ccc 
END_OF_SN


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
sub My_Actions::do_body {
    shift @_;
    for my $b (@_) {
         print "body line: $b\n";
    }
}
