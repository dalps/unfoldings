%{
  open String_product.StringPTNet
%}

%token PLACES "places" TRANS "trans" TOKENS "tokens"
%token COMMA "," SEMICOLON ";" COLON ":" LBRACE "{" RBRACE "}" DIRECTED_ARC "-->"
%token PRODUCT "product" IDLE "idle"
%token <string> ID "x"
%token EOF

%start <t> main

%%

let main := ~ = petrinet_declaration; EOF; <>

let petrinet_declaration :=
  "places"; ":"; places = identifier_list; ";";
  "trans"; ":"; transitions = identifier_list; ";";
  arcs = arc_list;
  "tokens"; ":"; tokens = identifier_list; {
    of_lists places transitions arcs tokens
  }

let arc_list := ~ = list(arc); <>

let arc :=
  transition = identifier; ":"; preset = identifier_list; "-->"; postset = identifier_list; ";"; {
    ((preset --> postset) transition)
  }

let identifier_list := ~ = separated_list(",", identifier); <>

let identifier := ~ = "x"; <>