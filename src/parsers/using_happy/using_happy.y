{
module Parser (happyParser) where
}

%name happyParser
%tokentype { Token }

%token
	int             { TokenInt $$}
	'+'             { TokenPlus }
	'-'             { TokenMinus }
	'*'             { TokenMult }
	'/'             { TokenDiv }
	'('             { TokenLP }
	')'             { TokenRP }

%left '+' '-'
%left '*' '/'
%%

Exp
	: Exp '+' Exp		{ Plus $1 $3 }
	| Exp '-' Exp		{ Minus $1 $3 }
	| Exp '*' Exp		{ Mult $1 $3}
	| Exp '/' Exp		{ Div $1 $3}
	| '(' Exp ')'		{ Exp $2 }
	| int				{ Int $1 }


{}
