# client

[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com//fp-homework/blob/master/hw3/LICENSE)

Reads program code from file (path supplied via script arguments) \
Prints output to console \
Program grammar:
* Program = (Statement*) ReturnStatement
* Statement = (Assignment | Loop | Invocation | DBComand) '\n'
* Assignment = 'var' Identifier '=' Expression
* Loop = 'while' BracesExpression '{' (Statement*) '}' 
* Expression = Invocation | Constant | OperatorExpression | IfThenElseExpression | LambdaDefExpression | BracesExpression
* Invocation = Identifier '(' Expression (',' Expression)* ')'
* Identifier = [a-z]+ (note : names like "function", "return", "if" or other keywords are not allowed. If you use such names nothing can be guaranteed)
* Constant = IntConstant | StringConstant | BoolConstant
* IntConstant = [0-9]+ (note: 00091 <=> 91)
* StringConstant = \"[a-z0-9A-Z_]*\"
* BoolConstant = 'true' | 'false' 
* OperatorExpression : UnaryOpExpr | BinaryOpExpr
* UnaryOpExpr = UnaryOp BracesExpression
* BinaryOpExpr = BracesExpression BinaryOp BracesExpression
* UnaryOp : '+' | '-' | '!'
* BinaryOp : '+' | '-' | '*' | '/' | '==' | '!=' | '&&' | '||'
* IfThenElseExpression = 'if' BracesExpression 'then' BracesExpression 'else' BracesExpression
* LambdaDefExpression = 'function' '(' (Identifier (',' Identifier)*)? ')' '{' Program '}'
* BracesExpression = '(' Expression ')'
* DBComand = PublishComand | LoadComand
* PublishComand = 'PUBLISH' Key Value ReplicationFactor 
* Key = BracesExpression of type String
* Value = BracesExpression of type String
* ReplicationFactor = IntConstant
* LoadComand = 'LOAD' Key Identifier 
* Key = BracesExpression of type String
* ReturnStatement = 'return' Expression

Note: 
* No empty lines allowed
* Only spaces are allowed as whitespace symbols
* Exactly ONE (1) space is expected between EACH PAIR OF consecutive tokens, regardless of their purpose