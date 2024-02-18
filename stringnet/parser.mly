%{
  open String_product
%}

%token PLACES "places" TRANS "trans" TOKENS "tokens"
%token COMMA "," SEMICOLON ";" COLON ":" 
%token LBRACE "{" RBRACE "}" DIRECTED_ARC "-->"
%token PRODUCT "product" IDLE "idle"
%token <string> ID "x"
%token EOF

%start <semantics list> main

%%

let main := ~ = list(declaration); EOF; <>

let declaration :=
| ~ = petrinet_declaration; <N> 
| ~ = product_declaration; <P>

let product_declaration :=
  "product"; "{";
    nets = list(petrinet_declaration); 
  "}"; "{";
    sync = separated_list(";", global_transition);
  "}"; {
    StringPTNetProduct.product nets sync
  }

let global_transition :=
  ~ = separated_nonempty_list(",", idle_or_transition); <>

let idle_or_transition :=
| "idle"; { `Idle }
| ~ = identifier; <`T>

let petrinet_declaration :=
| "{"; ~ = petrinet_declaration; "}"; <>
| "places"; ":"; places = identifier_list; ";";
  "trans"; ":"; transitions = identifier_list; ";";
  arcs = arc_list;
  "tokens"; ":"; tokens = identifier_list; {
    StringPTNet.of_lists places transitions arcs tokens
  }

let arc_list := ~ = list(arc); <>

let arc :=
  transition = identifier; ":"; preset = identifier_list; "-->"; postset = identifier_list; ";"; {
    (StringPTNet.( --> ) preset postset transition)
  }

let identifier_list := ~ = separated_list(",", identifier); <>

let identifier := ~ = "x"; <>