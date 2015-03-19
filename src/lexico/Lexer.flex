package lexico;
import static lexico.Token.*;
%%
%class Lexer
%type Token
LETRA=[a-zA-Z_][a-zA-Z0-9_]*
DIGITO=[+-]?[0-9]+
CADENA=[\"][a-zA-Z0-9 ]*[\"]
FLOAT=[+-]?[0-9]+[.][0-9]+
MF=[+-]?[0-9]+[.]+[a-zA-Z_0-9]*
COMENT_BLOQUE = [/][*].*[*][/]
CARACTER=[\'][a-zA-Z0-9][\']
MD=[+-]?[0-9]+[a-zA-Z_][0-9]*
VARIABLE=[\$][a-zA-Z_]+[0-9]*
CONJUNCION=[{}[]()]
SEPARADOR=[;,.]

ESPACIO=[ \t\r\n]
%{
public String lexema;
%}
%%
{ESPACIO} {/*Ignore*/}
"//".* {/*Ignore*/}
"=" {return new Symbol(sym.ASIGNACION, yychar, yyline, yytext());}
"==" {return new Symbol(sym.IGUAL, yychar, yyline, yytext());}
"!=" {return new Symbol(sym.DIFERENTE, yychar, yyline, yytext());}
"+" {return new Symbol(sym.MAS, yychar, yyline, yytext());}
"-" {return new Symbol(sym.MENOS, yychar, yyline, yytext());}
"*" {return new Symbol(sym.MULTIPLICACION, yychar, yyline, yytext());}
"/" {return new Symbol(sym.DIVESION, yychar, yyline, yytext());}
"echo" {return new Symbol(sym.ECHO, yychar, yyline, yytext());}
"abstract" {return new Symbol(sym.T_ABSTRACT, yychar, yyline, yytext());}
"&=" {return new Symbol(sym.T_AND_EQUAL, yychar, yyline, yytext());}
"array" {return new Symbol(sym.T_ARRAY, yychar, yyline, yytext());}
"(array)" {return new Symbol(sym.T_ARRAY_CAST, yychar, yyline, yytext());}
"as" {return new Symbol(sym.T_AS, yychar, yyline, yytext());}
"&&" {return new Symbol(sym.T_BOOLEAN_AND, yychar, yyline, yytext());}
"||" {return new Symbol(sym.T_BOOLEAN_OR, yychar, yyline, yytext());}
"(bool)" {return new Symbol(sym.T_BOOL_CAST, yychar, yyline, yytext());}
"(boolean)" {return new Symbol(sym.T_BOOL_CAST, yychar, yyline, yytext());}
"break" {return new Symbol(sym.T_BREAK, yychar, yyline, yytext());}
"callable" {return new Symbol(sym.T_CALLABLE, yychar, yyline, yytext());}
"case" {return new Symbol(sym.T_CASE, yychar, yyline, yytext());}
"catch" {return new Symbol(sym.T_CATCH, yychar, yyline, yytext());}
"class" {return new Symbol(sym.T_CLASS, yychar, yyline, yytext());}
"__CLASS__" {return new Symbol(sym.T_CLASS_C, yychar, yyline, yytext());}
"clone" {return new Symbol(sym.T_CLONE, yychar, yyline, yytext());}
"?>" {return new Symbol(sym.T_CLOSE_TAG, yychar, yyline, yytext());}
".=" {return new Symbol(sym.T_CONCAT_EQUAL, yychar, yyline, yytext());}
"const" {return new Symbol(sym.T_CONST, yychar, yyline, yytext());}
"continue" {return new Symbol(sym.T_CONTINUE, yychar, yyline, yytext());}
"{?" {return new Symbol(sym.T_CURLY_OPEN, yychar, yyline, yytext());}
"--" {return new Symbol(sym.T_DEC, yychar, yyline, yytext());}
"declare" {return new Symbol(sym.T_DECLARE, yychar, yyline, yytext());}
"default" {return new Symbol(sym.T_DEFAULT, yychar, yyline, yytext());}
"__DIR__" {return new Symbol(sym.T_DIR, yychar, yyline, yytext());}
"/=" {return new Symbol(sym.T_DIV_EQUAL, yychar, yyline, yytext());}
"do" {return new Symbol(sym.T_DO, yychar, yyline, yytext());}
"${" {return new Symbol(sym.T_DOLLAR_OPEN_CURLY_BRACES, yychar, yyline, yytext());}
"=>" {return new Symbol(sym.T_DOUBLE_ARROW, yychar, yyline, yytext());}
"(real)" {return new Symbol(sym.T_DOUBLE_CAST, yychar, yyline, yytext());}
"(double)" {return new Symbol(sym.T_DOUBLE_CAST, yychar, yyline, yytext());}
"(float)" {return new Symbol(sym.T_DOUBLE_CAST, yychar, yyline, yytext());}
"::" {return new Symbol(sym.T_DOUBLE_COLON, yychar, yyline, yytext());}
"else" {return new Symbol(sym.T_ELSE, yychar, yyline, yytext());}
"elseif" {return new Symbol(sym.T_ELSEIF, yychar, yyline, yytext());}
"empty" {return new Symbol(sym.T_EMPTY, yychar, yyline, yytext());}
"enddeclare" {return new Symbol(sym.T_ENDDECLARE, yychar, yyline, yytext());}
"endfor" {return new Symbol(sym.T_ENDFOR, yychar, yyline, yytext());}
"endforeach"{return new Symbol(sym.T_ENDFOREACH, yychar, yyline, yytext());}
"endif" {return new Symbol(sym.T_ENDIF, yychar, yyline, yytext());}
"endswitch" {return new Symbol(sym.T_ENDSWITCH, yychar, yyline, yytext());}
"endwhile" {return new Symbol(sym.T_ENDWHILE, yychar, yyline, yytext());}
"eval" {return new Symbol(sym.T_EVAL, yychar, yyline, yytext());}
"exit" {return new Symbol(sym.T_EXIT, yychar, yyline, yytext());}
"die" {return new Symbol(sym.T_EXIT, yychar, yyline, yytext());}
"extends" {return new Symbol(sym.T_EXTENDS, yychar, yyline, yytext());}
"__FILE__" {return new Symbol(sym.T_FILE, yychar, yyline, yytext());}
"final" {return new Symbol(sym.T_FINAL, yychar, yyline, yytext());}
"finally" {return new Symbol(sym.T_FINALLY, yychar, yyline, yytext());}
"for" {return new Symbol(sym.T_FOR, yychar, yyline, yytext());}
"foreach" {return new Symbol(sym.T_FOREACH, yychar, yyline, yytext());}
"function" {return new Symbol(sym.T_FUNCTION, yychar, yyline, yytext());}
"cfunction" {return new Symbol(sym.T_FUNC_C, yychar, yyline, yytext());}
"global" {return new Symbol(sym.T_GLOBAL, yychar, yyline, yytext());}
"goto" {return new Symbol(sym.T_GOTO, yychar, yyline, yytext());}
"if" {return new Symbol(sym.T_IF, yychar, yyline, yytext());}
"implements" {return new Symbol(sym.T_IMPLEMENTS, yychar, yyline, yytext());}
"++" {return new Symbol(sym.T_INC, yychar, yyline, yytext());}
"include" {return new Symbol(sym.T_INCLUDE, yychar, yyline, yytext());}
"include_once" {return new Symbol(sym.T_INCLUDE_ONCE, yychar, yyline, yytext());}
"instanceof" {return new Symbol(sym.T_INSTANCEOF, yychar, yyline, yytext());}
"insteadof" {return new Symbol(sym.T_INS, yychar, yyline, yytext());}{lexema=yytext(); return T_INSTEADOF;}
"(int)" {return new Symbol(sym.T_INT_CAST, yychar, yyline, yytext());}
"(integer)" {return new Symbol(sym.T_INT_CAST, yychar, yyline, yytext());}
"interface" {return new Symbol(sym.T_INTERFACE, yychar, yyline, yytext());}
"isset" {return new Symbol(sym.T_ISSET, yychar, yyline, yytext());}
">=" {return new Symbol(sym.T_IS_GREATER_OR_EQUAL, yychar, yyline, yytext());}
"===" {return new Symbol(sym.T_IS_IDENTICAL, yychar, yyline, yytext());}
"!=" {return new Symbol(sym.T_IS_NOT_EQUAL, yychar, yyline, yytext());}
"<>" {return new Symbol(sym.T_IS_NOT_EQUAL, yychar, yyline, yytext());}
"!==" {return new Symbol(sym.T_IS_NOT_IDENTICAL, yychar, yyline, yytext());}
"<=" {return new Symbol(sym.T_IS_SMALLER_OR_EQUAL, yychar, yyline, yytext());}
"__LINE__" {return new Symbol(sym.T_LINE, yychar, yyline, yytext());}
"list" {return new Symbol(sym.T_LIST, yychar, yyline, yytext());}
"and" {return new Symbol(sym.T_LOGICAL_AND, yychar, yyline, yytext());}
"or" {return new Symbol(sym.T_LOGICAL_OR, yychar, yyline, yytext());}
"xor" {return new Symbol(sym.T_LOGICAL_XOR, yychar, yyline, yytext());}
"__METHOD__" {return new Symbol(sym.T_METHOD_C, yychar, yyline, yytext());}
"-=" {return new Symbol(sym.T_MINUS_EQUAL, yychar, yyline, yytext());}
"%=" {return new Symbol(sym.T_MOD_EQUAL, yychar, yyline, yytext());}
"*=" {return new Symbol(sym.T_MUL_EQUAL, yychar, yyline, yytext());}
"namespace" {return new Symbol(sym.T_NAMESPACE, yychar, yyline, yytext());}
"__NAMESPACE__" {return new Symbol(sym.T_NS_C, yychar, yyline, yytext());}
"\\" {return new Symbol(sym.T_NS_SEPARATOR, yychar, yyline, yytext());}
"new" {return new Symbol(sym.T_NEW, yychar, yyline, yytext());}
"(object)" {return new Symbol(sym.T_OBJECT_CAST, yychar, yyline, yytext());}
"->" {return new Symbol(sym.T_OBJECT_OPERATOR, yychar, yyline, yytext());}
"<?php" {return new Symbol(sym.T_OPEN_TAG, yychar, yyline, yytext());}
"<?" {return new Symbol(sym.T_OPEN_TAG, yychar, yyline, yytext());}
"<%" {return new Symbol(sym.T_OPEN_TAG, yychar, yyline, yytext());}
"<?=" {return new Symbol(sym.T_OPEN_TAG_WITH_ECHO, yychar, yyline, yytext());}
"<%=" {return new Symbol(sym.T_OPEN_TAG_WITH_ECHO, yychar, yyline, yytext());}
"|=" {return new Symbol(sym.T_OR_EQUAL, yychar, yyline, yytext());}
"+=" {return new Symbol(sym.T_PLUS_EQUAL, yychar, yyline, yytext());}
"," {return new Symbol(sym.COMA, yychar, yyline, yytext());}
"**" {return new Symbol(sym.T_POW, yychar, yyline, yytext());}
"**=" {return new Symbol(sym.T_POW_EQUAL, yychar, yyline, yytext());}
"print" {return new Symbol(sym.T_PRINT, yychar, yyline, yytext());}
"private" {return new Symbol(sym.T_PRIVATE, yychar, yyline, yytext());}
"public" {return new Symbol(sym.T_PUBLIC, yychar, yyline, yytext());}
"protected" {return new Symbol(sym.T_PROTECTED, yychar, yyline, yytext());}
"require" {return new Symbol(sym.T_REQUIRE, yychar, yyline, yytext());}
"require_once" {return new Symbol(sym.T_REQUIRE_ONCE, yychar, yyline, yytext());}
"return" {return new Symbol(sym.T_RETURN, yychar, yyline, yytext());}
"<<" {return new Symbol(sym.T_SL, yychar, yyline, yytext());}
"<<=" {return new Symbol(sym.T_ZL_EQUAL, yychar, yyline, yytext());}
">>" {return new Symbol(sym.T_SR, yychar, yyline, yytext());}
">>=" {return new Symbol(sym.T_SR_EQUAL, yychar, yyline, yytext());}
"<<<" {return new Symbol(sym.T_START_HEREDOC, yychar, yyline, yytext());}
"static" {return new Symbol(sym.T_STATIC, yychar, yyline, yytext());}
{LETRA} {return new Symbol(sym.ID, yychar, yyline, yytext());}
{DIGITO} {return new Symbol(sym.ENTERO, yychar, yyline, yytext());}
{CADENA} {return new Symbol(sym.CADENA, yychar, yyline, yytext());}
{FLOAT} {return new Symbol(sym.FLOAT, yychar, yyline, yytext());}
{VARIABLE} {return new Symbol(sym.VARIABLE, yychar, yyline, yytext());}
{MF} {return new Symbol(sym.MF, yychar, yyline, yytext());}
{CARACTER} {return new Symbol(sym.CARACTER, yychar, yyline, yytext());}
{MD} {return new Symbol(sym.MD, yychar, yyline, yytext());}
{COMENT_BLOQUE} {return new Symbol(sym.COMENT_BLOQUE, yychar, yyline, yytext());}
{CONJUNCION} {return new Symbol(sym.CONJUNCION, yychar, yyline, yytext());}
{SEPARADOR} {return new Symbol(sym.SEPARADOR, yychar, yyline, yytext());}
. {return ERROR;}