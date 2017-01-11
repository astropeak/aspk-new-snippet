use Marpa::R2;

my $dsl = <<'END_OF_DSL';
    :default ::= action => [name,values]
    lexeme default = latm => 1
    
    Statement ::=
    IfStatement action => ::first
    | WhileStatement action => ::first
    | VarStatement action =>::first
    IfStatement ::= '@if' '(' LogicalExpression ')' Content StatementEnd action =>do_if_statement
    StatementEnd ::= '@end' | ''

    LogicalExpression ::= EqulityExpression
    | EqulityExpression '&&' LogicalExpression
    | EqulityExpression '||' LogicalExpression
    | '!' '(' LogicalExpression ')'

    EqulityExpression ::=
    Name EqulityOperator Value
    | '(' EqulityOperator ')'

    EqulityOperator ::=
    '=='|'!='|'>'|'<'|'>='|'<='

    Content ::=
    

    Calculator ::= Expression action => ::first
    
    Factor ::= Number action => ::first
    Term ::=
    Term '*' Factor action => do_multiply
    | Factor action => ::first
    Expression ::=
    Expression '+' Term action => do_add
    | Term action => ::first
    Number ~ digits
    digits ~ [\d]+
    :discard ~ whitespace
    whitespace ~ [\s]+
    END_OF_DSL
    
    my $grammar = Marpa::R2::Scanless::G->new( { source => \$dsl } );
my $input = '42 * 1 + 7';
my $value_ref = $grammar->parse( \$input, 'My_Actions' );

sub My_Actions::do_add {
    my ( undef, $t1, undef, $t2 ) = @_;
return $t1 + $t2;
}

sub My_Actions::do_multiply {
    my ( undef, $t1, undef, $t2 ) = @_;
return $t1 * $t2;
}
