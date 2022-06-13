functor PlcParserLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : PlcParser_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\032\000\002\000\031\000\004\000\030\000\007\000\029\000\
\\009\000\028\000\010\000\027\000\011\000\026\000\012\000\025\000\
\\013\000\024\000\014\000\023\000\032\000\022\000\034\000\021\000\
\\035\000\020\000\037\000\019\000\038\000\018\000\046\000\017\000\
\\048\000\016\000\000\000\
\\001\000\003\000\071\000\046\000\070\000\000\000\
\\001\000\004\000\030\000\007\000\029\000\009\000\028\000\010\000\027\000\
\\011\000\026\000\012\000\025\000\013\000\024\000\014\000\023\000\
\\019\000\059\000\030\000\058\000\032\000\022\000\033\000\057\000\
\\034\000\056\000\037\000\019\000\038\000\018\000\043\000\055\000\
\\044\000\054\000\045\000\053\000\046\000\017\000\048\000\016\000\000\000\
\\001\000\004\000\030\000\007\000\029\000\009\000\028\000\010\000\027\000\
\\011\000\026\000\012\000\025\000\013\000\024\000\014\000\023\000\
\\030\000\058\000\032\000\022\000\033\000\057\000\034\000\056\000\
\\037\000\019\000\038\000\018\000\043\000\055\000\044\000\054\000\
\\045\000\053\000\046\000\017\000\048\000\016\000\000\000\
\\001\000\004\000\030\000\007\000\029\000\009\000\028\000\010\000\027\000\
\\011\000\026\000\012\000\025\000\013\000\024\000\014\000\023\000\
\\032\000\022\000\034\000\062\000\037\000\019\000\038\000\018\000\
\\042\000\141\000\046\000\017\000\048\000\016\000\000\000\
\\001\000\004\000\030\000\007\000\029\000\009\000\028\000\010\000\027\000\
\\011\000\026\000\012\000\025\000\013\000\024\000\014\000\023\000\
\\032\000\022\000\034\000\062\000\037\000\019\000\038\000\018\000\
\\046\000\017\000\048\000\016\000\000\000\
\\001\000\005\000\103\000\010\000\047\000\017\000\046\000\018\000\045\000\
\\019\000\044\000\020\000\043\000\021\000\042\000\022\000\041\000\
\\023\000\040\000\024\000\039\000\025\000\038\000\026\000\037\000\
\\030\000\036\000\000\000\
\\001\000\006\000\142\000\010\000\047\000\017\000\046\000\018\000\045\000\
\\019\000\044\000\020\000\043\000\021\000\042\000\022\000\041\000\
\\023\000\040\000\024\000\039\000\025\000\038\000\026\000\037\000\
\\030\000\036\000\000\000\
\\001\000\008\000\102\000\010\000\047\000\017\000\046\000\018\000\045\000\
\\019\000\044\000\020\000\043\000\021\000\042\000\022\000\041\000\
\\023\000\040\000\024\000\039\000\025\000\038\000\026\000\037\000\
\\030\000\036\000\000\000\
\\001\000\010\000\047\000\017\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\021\000\042\000\022\000\041\000\023\000\040\000\
\\024\000\039\000\025\000\038\000\026\000\037\000\027\000\095\000\
\\030\000\036\000\033\000\094\000\000\000\
\\001\000\010\000\047\000\017\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\021\000\042\000\022\000\041\000\023\000\040\000\
\\024\000\039\000\025\000\038\000\026\000\037\000\030\000\036\000\
\\031\000\161\000\049\000\161\000\000\000\
\\001\000\010\000\047\000\017\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\021\000\042\000\022\000\041\000\023\000\040\000\
\\024\000\039\000\025\000\038\000\026\000\037\000\030\000\036\000\
\\036\000\128\000\039\000\127\000\000\000\
\\001\000\010\000\047\000\017\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\021\000\042\000\022\000\041\000\023\000\040\000\
\\024\000\039\000\025\000\038\000\026\000\037\000\030\000\036\000\
\\036\000\133\000\000\000\
\\001\000\010\000\047\000\017\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\021\000\042\000\022\000\041\000\023\000\040\000\
\\024\000\039\000\025\000\038\000\026\000\145\000\030\000\036\000\000\000\
\\001\000\010\000\047\000\017\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\021\000\042\000\022\000\041\000\023\000\040\000\
\\024\000\039\000\025\000\038\000\026\000\149\000\030\000\036\000\000\000\
\\001\000\010\000\047\000\017\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\021\000\042\000\022\000\041\000\023\000\040\000\
\\024\000\039\000\025\000\038\000\026\000\158\000\030\000\036\000\000\000\
\\001\000\019\000\125\000\028\000\124\000\033\000\123\000\034\000\122\000\
\\046\000\121\000\000\000\
\\001\000\021\000\106\000\000\000\
\\001\000\021\000\130\000\000\000\
\\001\000\021\000\155\000\040\000\091\000\000\000\
\\001\000\027\000\119\000\030\000\092\000\033\000\118\000\040\000\091\000\000\000\
\\001\000\027\000\119\000\033\000\118\000\040\000\091\000\000\000\
\\001\000\028\000\144\000\000\000\
\\001\000\029\000\107\000\000\000\
\\001\000\029\000\114\000\000\000\
\\001\000\029\000\120\000\040\000\091\000\000\000\
\\001\000\030\000\058\000\033\000\090\000\034\000\089\000\043\000\055\000\
\\044\000\054\000\045\000\053\000\000\000\
\\001\000\030\000\058\000\034\000\089\000\043\000\055\000\044\000\054\000\
\\045\000\053\000\000\000\
\\001\000\030\000\092\000\040\000\091\000\000\000\
\\001\000\031\000\101\000\000\000\
\\001\000\033\000\093\000\000\000\
\\001\000\033\000\111\000\000\000\
\\001\000\033\000\117\000\000\000\
\\001\000\033\000\135\000\000\000\
\\001\000\033\000\138\000\000\000\
\\001\000\034\000\049\000\000\000\
\\001\000\036\000\128\000\039\000\127\000\000\000\
\\001\000\040\000\091\000\046\000\109\000\000\000\
\\001\000\040\000\147\000\000\000\
\\001\000\041\000\085\000\000\000\
\\001\000\046\000\072\000\000\000\
\\001\000\046\000\100\000\000\000\
\\001\000\046\000\105\000\000\000\
\\001\000\048\000\073\000\000\000\
\\001\000\049\000\000\000\000\000\
\\161\000\010\000\047\000\017\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\021\000\042\000\022\000\041\000\023\000\040\000\
\\024\000\039\000\025\000\038\000\026\000\037\000\030\000\036\000\000\000\
\\162\000\000\000\
\\163\000\000\000\
\\164\000\010\000\047\000\017\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\021\000\042\000\022\000\041\000\023\000\040\000\
\\024\000\039\000\025\000\038\000\026\000\037\000\030\000\036\000\000\000\
\\165\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\172\000\000\000\
\\173\000\000\000\
\\174\000\000\000\
\\175\000\000\000\
\\176\000\032\000\022\000\034\000\034\000\046\000\017\000\000\000\
\\177\000\000\000\
\\178\000\032\000\022\000\034\000\034\000\046\000\017\000\000\000\
\\179\000\000\000\
\\180\000\000\000\
\\181\000\000\000\
\\182\000\000\000\
\\183\000\000\000\
\\184\000\000\000\
\\185\000\010\000\047\000\017\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\021\000\042\000\022\000\041\000\023\000\040\000\
\\024\000\039\000\025\000\038\000\030\000\036\000\000\000\
\\186\000\010\000\047\000\017\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\021\000\042\000\022\000\041\000\023\000\040\000\
\\024\000\039\000\025\000\038\000\030\000\036\000\000\000\
\\187\000\010\000\047\000\017\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\021\000\042\000\022\000\041\000\023\000\040\000\
\\024\000\039\000\025\000\038\000\030\000\036\000\000\000\
\\188\000\000\000\
\\189\000\000\000\
\\190\000\010\000\047\000\017\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\021\000\042\000\022\000\041\000\023\000\040\000\
\\024\000\039\000\025\000\038\000\030\000\036\000\000\000\
\\191\000\000\000\
\\192\000\000\000\
\\193\000\000\000\
\\194\000\025\000\038\000\000\000\
\\195\000\017\000\046\000\021\000\042\000\022\000\041\000\023\000\040\000\
\\024\000\039\000\025\000\038\000\030\000\036\000\000\000\
\\196\000\017\000\046\000\021\000\042\000\022\000\041\000\023\000\040\000\
\\024\000\039\000\025\000\038\000\030\000\036\000\000\000\
\\197\000\017\000\046\000\021\000\042\000\022\000\041\000\023\000\040\000\
\\024\000\039\000\025\000\038\000\030\000\036\000\000\000\
\\198\000\017\000\046\000\021\000\042\000\022\000\041\000\023\000\040\000\
\\024\000\039\000\025\000\038\000\030\000\036\000\000\000\
\\199\000\025\000\038\000\000\000\
\\200\000\025\000\038\000\000\000\
\\201\000\025\000\038\000\000\000\
\\202\000\025\000\038\000\000\000\
\\203\000\025\000\038\000\000\000\
\\204\000\010\000\047\000\017\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\021\000\042\000\022\000\041\000\023\000\040\000\
\\024\000\039\000\025\000\038\000\026\000\037\000\030\000\036\000\000\000\
\\205\000\000\000\
\\206\000\010\000\047\000\017\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\021\000\042\000\022\000\041\000\023\000\040\000\
\\024\000\039\000\025\000\038\000\030\000\036\000\000\000\
\\207\000\010\000\047\000\017\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\021\000\042\000\022\000\041\000\023\000\040\000\
\\024\000\039\000\025\000\038\000\030\000\036\000\000\000\
\\208\000\017\000\046\000\021\000\042\000\022\000\041\000\023\000\040\000\
\\024\000\039\000\025\000\038\000\030\000\036\000\000\000\
\\209\000\000\000\
\\210\000\000\000\
\\211\000\000\000\
\\212\000\000\000\
\\213\000\000\000\
\\214\000\000\000\
\\215\000\000\000\
\\216\000\000\000\
\\217\000\010\000\047\000\017\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\021\000\042\000\022\000\041\000\023\000\040\000\
\\024\000\039\000\025\000\038\000\026\000\037\000\027\000\095\000\
\\030\000\036\000\000\000\
\\218\000\000\000\
\\219\000\010\000\047\000\017\000\046\000\018\000\045\000\019\000\044\000\
\\020\000\043\000\021\000\042\000\022\000\041\000\023\000\040\000\
\\024\000\039\000\025\000\038\000\026\000\037\000\030\000\036\000\000\000\
\\220\000\000\000\
\\221\000\000\000\
\\222\000\000\000\
\\223\000\027\000\110\000\000\000\
\\224\000\000\000\
\\225\000\000\000\
\\226\000\000\000\
\\227\000\000\000\
\\228\000\000\000\
\\229\000\000\000\
\\230\000\000\000\
\\231\000\000\000\
\\232\000\040\000\091\000\000\000\
\\233\000\027\000\119\000\040\000\091\000\000\000\
\\234\000\000\000\
\"
val actionRowNumbers =
"\000\000\066\000\047\000\062\000\
\\065\000\064\000\063\000\067\000\
\\055\000\060\000\061\000\045\000\
\\058\000\046\000\075\000\095\000\
\\077\000\076\000\035\000\002\000\
\\000\000\005\000\005\000\005\000\
\\005\000\005\000\005\000\005\000\
\\005\000\001\000\040\000\100\000\
\\005\000\099\000\043\000\005\000\
\\005\000\005\000\005\000\005\000\
\\005\000\005\000\005\000\005\000\
\\005\000\005\000\039\000\026\000\
\\028\000\030\000\009\000\112\000\
\\111\000\110\000\003\000\072\000\
\\027\000\041\000\029\000\071\000\
\\003\000\090\000\070\000\069\000\
\\092\000\091\000\008\000\006\000\
\\035\000\042\000\017\000\023\000\
\\088\000\087\000\086\000\085\000\
\\084\000\083\000\082\000\081\000\
\\079\000\078\000\080\000\005\000\
\\037\000\107\000\031\000\027\000\
\\105\000\027\000\024\000\098\000\
\\097\000\005\000\032\000\020\000\
\\025\000\016\000\049\000\096\000\
\\036\000\005\000\018\000\035\000\
\\005\000\089\000\012\000\109\000\
\\027\000\106\000\021\000\116\000\
\\033\000\102\000\101\000\114\000\
\\113\000\027\000\115\000\051\000\
\\052\000\053\000\050\000\034\000\
\\068\000\004\000\093\000\007\000\
\\005\000\022\000\013\000\059\000\
\\108\000\073\000\118\000\117\000\
\\005\000\038\000\103\000\104\000\
\\005\000\014\000\027\000\000\000\
\\048\000\005\000\074\000\000\000\
\\019\000\010\000\054\000\011\000\
\\056\000\005\000\094\000\015\000\
\\000\000\057\000\044\000"
val gotoT =
"\
\\001\000\158\000\002\000\013\000\003\000\012\000\004\000\011\000\
\\005\000\010\000\006\000\009\000\007\000\008\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\014\000\002\000\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\006\000\031\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\033\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\019\000\046\000\000\000\
\\004\000\050\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\016\000\049\000\022\000\048\000\000\000\
\\001\000\058\000\002\000\013\000\003\000\012\000\004\000\011\000\
\\005\000\010\000\006\000\009\000\007\000\008\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\014\000\002\000\015\000\001\000\000\000\
\\004\000\059\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\004\000\061\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\004\000\062\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\004\000\063\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\004\000\064\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\004\000\065\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\004\000\066\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\004\000\067\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\050\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\016\000\049\000\000\000\
\\000\000\
\\000\000\
\\004\000\072\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\004\000\073\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\004\000\074\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\004\000\075\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\004\000\076\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\004\000\077\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\004\000\078\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\004\000\079\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\004\000\080\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\004\000\081\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\004\000\082\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\000\000\
\\020\000\086\000\021\000\085\000\022\000\084\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\050\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\016\000\049\000\022\000\095\000\023\000\094\000\000\000\
\\000\000\
\\022\000\096\000\000\000\
\\009\000\097\000\000\000\
\\000\000\
\\000\000\
\\004\000\050\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\016\000\049\000\022\000\048\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\019\000\102\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\106\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\022\000\110\000\023\000\094\000\000\000\
\\000\000\
\\022\000\111\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\114\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\016\000\113\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\124\000\000\000\
\\004\000\127\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\000\000\
\\019\000\129\000\000\000\
\\004\000\130\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\020\000\132\000\021\000\085\000\022\000\084\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\022\000\135\000\023\000\134\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\138\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\018\000\137\000\000\000\
\\000\000\
\\000\000\
\\004\000\141\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\144\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\146\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\000\000\
\\022\000\148\000\000\000\
\\001\000\150\000\002\000\013\000\003\000\012\000\004\000\149\000\
\\005\000\010\000\006\000\009\000\007\000\008\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\014\000\002\000\015\000\001\000\000\000\
\\000\000\
\\004\000\151\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\000\000\
\\001\000\152\000\002\000\013\000\003\000\012\000\004\000\149\000\
\\005\000\010\000\006\000\009\000\007\000\008\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\014\000\002\000\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\154\000\000\000\
\\000\000\
\\004\000\155\000\005\000\010\000\006\000\009\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\157\000\002\000\013\000\003\000\012\000\004\000\149\000\
\\005\000\010\000\006\000\009\000\007\000\008\000\008\000\007\000\
\\010\000\006\000\011\000\005\000\012\000\004\000\013\000\003\000\
\\014\000\002\000\015\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 159
val numrules = 74
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | Number of unit ->  (int) | CINT of unit ->  (int)
 | ID of unit ->  (string) | Types of unit ->  (plcType list)
 | Type of unit ->  (plcType) | TypedVar of unit ->  (plcType*string)
 | Params of unit ->  ( ( plcType * string )  list)
 | Args of unit ->  ( ( plcType * string )  list)
 | KeyExpr of unit ->  (expr option)
 | MatchExpr of unit ->  ( ( expr option * expr )  list)
 | Comps of unit ->  (expr list) | NotExpr of unit ->  (expr)
 | Comen of unit ->  (expr) | AppExpr of unit ->  (expr)
 | ComparativExpr of unit ->  (expr) | AlgebricExpr of unit ->  (expr)
 | LogicalExpr of unit ->  (expr) | Text of unit ->  (expr)
 | Const of unit ->  (expr) | DeclFun of unit ->  (expr)
 | UExpr of unit ->  (expr) | Cond of unit ->  (expr)
 | Expr of unit ->  (expr) | AnonFun of unit ->  (expr)
 | Decl of unit ->  (expr) | Prog of unit ->  (expr)
end
type svalue = MlyValue.svalue
type result = expr
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn _ => false
val showTerminal =
fn (T 0) => "VAR"
  | (T 1) => "FUN"
  | (T 2) => "REC"
  | (T 3) => "IF"
  | (T 4) => "THEN"
  | (T 5) => "ELSE"
  | (T 6) => "MATCH"
  | (T 7) => "WITH"
  | (T 8) => "NOT"
  | (T 9) => "MINUS"
  | (T 10) => "HD"
  | (T 11) => "TL"
  | (T 12) => "ISEQUAL"
  | (T 13) => "PRINT"
  | (T 14) => "OCOMEN"
  | (T 15) => "CCOMEN"
  | (T 16) => "AND"
  | (T 17) => "PLUS"
  | (T 18) => "MULTI"
  | (T 19) => "DIV"
  | (T 20) => "EQUAL"
  | (T 21) => "DIFF"
  | (T 22) => "LSTHAN"
  | (T 23) => "LSEQTHAN"
  | (T 24) => "ADDTL"
  | (T 25) => "SEMICOLON"
  | (T 26) => "COMMA"
  | (T 27) => "COLON"
  | (T 28) => "RSBRAC"
  | (T 29) => "LSBRAC"
  | (T 30) => "RBRA"
  | (T 31) => "LBRA"
  | (T 32) => "RPAR"
  | (T 33) => "LPAR"
  | (T 34) => "FN"
  | (T 35) => "END"
  | (T 36) => "TRUE"
  | (T 37) => "FALSE"
  | (T 38) => "PIPE"
  | (T 39) => "ARROW"
  | (T 40) => "ARROWFUNCTION"
  | (T 41) => "UNDERSCORE"
  | (T 42) => "NIL"
  | (T 43) => "BOOL"
  | (T 44) => "INT"
  | (T 45) => "ID"
  | (T 46) => "CINT"
  | (T 47) => "Number"
  | (T 48) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 48) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39)
 $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32)
 $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25)
 $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18)
 $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11)
 $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ 
(T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Expr Expr1, Expr1left, Expr1right)) :: 
rest671)) => let val  result = MlyValue.Prog (fn _ => let val  (Expr
 as Expr1) = Expr1 ()
 in (Expr)
end)
 in ( LrTable.NT 0, ( result, Expr1left, Expr1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.Decl Decl1, Decl1left, Decl1right)) :: 
rest671)) => let val  result = MlyValue.Prog (fn _ => let val  (Decl
 as Decl1) = Decl1 ()
 in (Decl)
end)
 in ( LrTable.NT 0, ( result, Decl1left, Decl1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.Comen Comen1, Comen1left, Comen1right)) :: 
rest671)) => let val  result = MlyValue.Prog (fn _ => let val  (Comen
 as Comen1) = Comen1 ()
 in (Comen)
end)
 in ( LrTable.NT 0, ( result, Comen1left, Comen1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: _ :: _ :: ( _
, ( MlyValue.Text Text1, _, _)) :: _ :: ( _, ( _, LPAR1left, _)) :: 
rest671)) => let val  result = MlyValue.Comen (fn _ => let val  Text1
 = Text1 ()
 val  (Expr as Expr1) = Expr1 ()
 in (Expr)
end)
 in ( LrTable.NT 13, ( result, LPAR1left, Expr1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.Text (fn _ => let val  (ID as ID1) = ID1
 ()
 in (Var(ID))
end)
 in ( LrTable.NT 8, ( result, ID1left, ID1right), rest671)
end
|  ( 5, ( ( _, ( _, _, COLON1right)) :: ( _, ( MlyValue.Text Text1, 
Text1left, _)) :: rest671)) => let val  result = MlyValue.Text (fn _
 => let val  (Text as Text1) = Text1 ()
 in (Text)
end)
 in ( LrTable.NT 8, ( result, Text1left, COLON1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: ( _, ( 
MlyValue.Text Text1, Text1left, _)) :: rest671)) => let val  result = 
MlyValue.Text (fn _ => let val  (Text as Text1) = Text1 ()
 val  ID1 = ID1 ()
 in (Text)
end)
 in ( LrTable.NT 8, ( result, Text1left, ID1right), rest671)
end
|  ( 7, ( ( _, ( _, _, LPAR1right)) :: ( _, ( MlyValue.Text Text1, 
Text1left, _)) :: rest671)) => let val  result = MlyValue.Text (fn _
 => let val  (Text as Text1) = Text1 ()
 in (Text)
end)
 in ( LrTable.NT 8, ( result, Text1left, LPAR1right), rest671)
end
|  ( 8, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Text Text1, 
Text1left, _)) :: rest671)) => let val  result = MlyValue.Text (fn _
 => let val  (Text as Text1) = Text1 ()
 in (Text)
end)
 in ( LrTable.NT 8, ( result, Text1left, RPAR1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.Prog Prog1, _, Prog1right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: 
( _, ( _, VAR1left, _)) :: rest671)) => let val  result = 
MlyValue.Decl (fn _ => let val  (ID as ID1) = ID1 ()
 val  (Expr as Expr1) = Expr1 ()
 val  (Prog as Prog1) = Prog1 ()
 in (Let(ID, Expr, Prog))
end)
 in ( LrTable.NT 1, ( result, VAR1left, Prog1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.DeclFun DeclFun1, DeclFun1left, 
DeclFun1right)) :: rest671)) => let val  result = MlyValue.Decl (fn _
 => let val  (DeclFun as DeclFun1) = DeclFun1 ()
 in (DeclFun)
end)
 in ( LrTable.NT 1, ( result, DeclFun1left, DeclFun1right), rest671)

end
|  ( 11, ( ( _, ( MlyValue.Prog Prog1, _, Prog1right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, _, _)) :: _ :: ( _, ( MlyValue.Args Args1, _, _))
 :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, FUN1left, _)) :: 
rest671)) => let val  result = MlyValue.DeclFun (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (Args as Args1) = Args1 ()
 val  (Expr as Expr1) = Expr1 ()
 val  (Prog as Prog1) = Prog1 ()
 in (Let(ID, makeAnon(Args, Expr), Prog))
end)
 in ( LrTable.NT 6, ( result, FUN1left, Prog1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.Prog Prog1, _, Prog1right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, _, _)) :: _ :: ( _, ( MlyValue.Type Type1, _, _))
 :: _ :: ( _, ( MlyValue.Args Args1, _, _)) :: ( _, ( MlyValue.ID ID1,
 _, _)) :: _ :: ( _, ( _, FUN1left, _)) :: rest671)) => let val  
result = MlyValue.DeclFun (fn _ => let val  (ID as ID1) = ID1 ()
 val  (Args as Args1) = Args1 ()
 val  (Type as Type1) = Type1 ()
 val  (Expr as Expr1) = Expr1 ()
 val  (Prog as Prog1) = Prog1 ()
 in (makeFun(ID, Args, Type, Expr, Prog))
end)
 in ( LrTable.NT 6, ( result, FUN1left, Prog1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.AnonFun AnonFun1, AnonFun1left, 
AnonFun1right)) :: rest671)) => let val  result = MlyValue.DeclFun (fn
 _ => let val  (AnonFun as AnonFun1) = AnonFun1 ()
 in (AnonFun)
end)
 in ( LrTable.NT 6, ( result, AnonFun1left, AnonFun1right), rest671)

end
|  ( 14, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.Expr Expr1, _,
 _)) :: _ :: ( _, ( MlyValue.Args Args1, _, _)) :: ( _, ( _, FN1left,
 _)) :: rest671)) => let val  result = MlyValue.AnonFun (fn _ => let
 val  (Args as Args1) = Args1 ()
 val  (Expr as Expr1) = Expr1 ()
 in (makeAnon(Args, Expr))
end)
 in ( LrTable.NT 2, ( result, FN1left, END1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.UExpr UExpr1, UExpr1left, UExpr1right)) :: 
rest671)) => let val  result = MlyValue.Expr (fn _ => let val  (UExpr
 as UExpr1) = UExpr1 ()
 in (UExpr)
end)
 in ( LrTable.NT 3, ( result, UExpr1left, UExpr1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.Cond Cond1, Cond1left, Cond1right)) :: 
rest671)) => let val  result = MlyValue.Expr (fn _ => let val  (Cond
 as Cond1) = Cond1 ()
 in (Cond)
end)
 in ( LrTable.NT 3, ( result, Cond1left, Cond1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.AppExpr AppExpr1, AppExpr1left, 
AppExpr1right)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (AppExpr as AppExpr1) = AppExpr1 ()
 in (AppExpr)
end)
 in ( LrTable.NT 3, ( result, AppExpr1left, AppExpr1right), rest671)

end
|  ( 18, ( ( _, ( MlyValue.LogicalExpr LogicalExpr1, LogicalExpr1left,
 LogicalExpr1right)) :: rest671)) => let val  result = MlyValue.Expr
 (fn _ => let val  (LogicalExpr as LogicalExpr1) = LogicalExpr1 ()
 in (LogicalExpr)
end)
 in ( LrTable.NT 3, ( result, LogicalExpr1left, LogicalExpr1right), 
rest671)
end
|  ( 19, ( ( _, ( MlyValue.AlgebricExpr AlgebricExpr1, 
AlgebricExpr1left, AlgebricExpr1right)) :: rest671)) => let val  
result = MlyValue.Expr (fn _ => let val  (AlgebricExpr as 
AlgebricExpr1) = AlgebricExpr1 ()
 in (AlgebricExpr)
end)
 in ( LrTable.NT 3, ( result, AlgebricExpr1left, AlgebricExpr1right), 
rest671)
end
|  ( 20, ( ( _, ( MlyValue.ComparativExpr ComparativExpr1, 
ComparativExpr1left, ComparativExpr1right)) :: rest671)) => let val  
result = MlyValue.Expr (fn _ => let val  (ComparativExpr as 
ComparativExpr1) = ComparativExpr1 ()
 in (ComparativExpr)
end)
 in ( LrTable.NT 3, ( result, ComparativExpr1left, 
ComparativExpr1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.NotExpr NotExpr1, NotExpr1left, 
NotExpr1right)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  (NotExpr as NotExpr1) = NotExpr1 ()
 in (NotExpr)
end)
 in ( LrTable.NT 3, ( result, NotExpr1left, NotExpr1right), rest671)

end
|  ( 22, ( ( _, ( MlyValue.Const Const1, Const1left, Const1right)) :: 
rest671)) => let val  result = MlyValue.Expr (fn _ => let val  (Const
 as Const1) = Const1 ()
 in (Const)
end)
 in ( LrTable.NT 3, ( result, Const1left, Const1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.MatchExpr MatchExpr1, _, MatchExpr1right))
 :: _ :: ( _, ( MlyValue.Expr Expr1, _, _)) :: ( _, ( _, MATCH1left, _
)) :: rest671)) => let val  result = MlyValue.Expr (fn _ => let val  (
Expr as Expr1) = Expr1 ()
 val  (MatchExpr as MatchExpr1) = MatchExpr1 ()
 in (Match(Expr, MatchExpr))
end)
 in ( LrTable.NT 3, ( result, MATCH1left, MatchExpr1right), rest671)

end
|  ( 24, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
HD1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _ =>
 let val  Expr1 = Expr1 ()
 in (Prim1("hd", Expr1))
end)
 in ( LrTable.NT 3, ( result, HD1left, Expr1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
TL1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _ =>
 let val  Expr1 = Expr1 ()
 in (Prim1("tl", Expr1))
end)
 in ( LrTable.NT 3, ( result, TL1left, Expr1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
PRINT1left, _)) :: rest671)) => let val  result = MlyValue.Expr (fn _
 => let val  Expr1 = Expr1 ()
 in (Prim1("print", Expr1))
end)
 in ( LrTable.NT 3, ( result, PRINT1left, Expr1right), rest671)
end
|  ( 27, ( ( _, ( _, _, RPAR1right)) :: ( _, ( _, LPAR1left, _)) :: 
rest671)) => let val  result = MlyValue.Expr (fn _ => (List []))
 in ( LrTable.NT 3, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 28, ( ( _, ( _, _, RPAR1right)) :: _ :: _ :: ( _, ( MlyValue.Type
 Type1, _, _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  
result = MlyValue.Expr (fn _ => let val  (Type as Type1) = Type1 ()
 in (ESeq(Type))
end)
 in ( LrTable.NT 3, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.Expr Expr3, _, Expr3right)) :: _ :: ( _, ( 
MlyValue.Expr Expr2, _, _)) :: _ :: ( _, ( MlyValue.Expr Expr1, _, _))
 :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.Cond (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 val  Expr3 = Expr3 ()
 in (If(Expr1, Expr2, Expr3))
end)
 in ( LrTable.NT 4, ( result, IF1left, Expr3right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.Number Number1, Number1left, Number1right))
 :: rest671)) => let val  result = MlyValue.Const (fn _ => let val  (
Number as Number1) = Number1 ()
 in (ConI(Number))
end)
 in ( LrTable.NT 7, ( result, Number1left, Number1right), rest671)
end
|  ( 31, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.Const (fn _ => (ConB(true)))
 in ( LrTable.NT 7, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 32, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.Const (fn _ => (ConB(false)))
 in ( LrTable.NT 7, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.LogicalExpr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("&&", Expr1, Expr2))
end)
 in ( LrTable.NT 9, ( result, Expr1left, Expr2right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.AlgebricExpr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("+", Expr1, Expr2))
end)
 in ( LrTable.NT 10, ( result, Expr1left, Expr2right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.AlgebricExpr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("-", Expr1, Expr2))
end)
 in ( LrTable.NT 10, ( result, Expr1left, Expr2right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.AlgebricExpr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("*", Expr1, Expr2))
end)
 in ( LrTable.NT 10, ( result, Expr1left, Expr2right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.AlgebricExpr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("/", Expr1, Expr2))
end)
 in ( LrTable.NT 10, ( result, Expr1left, Expr2right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.ComparativExpr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("=", Expr1, Expr2))
end)
 in ( LrTable.NT 11, ( result, Expr1left, Expr2right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.ComparativExpr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("!=", Expr1, Expr2))
end)
 in ( LrTable.NT 11, ( result, Expr1left, Expr2right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.ComparativExpr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("<", Expr1, Expr2))
end)
 in ( LrTable.NT 11, ( result, Expr1left, Expr2right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.ComparativExpr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("<=", Expr1, Expr2))
end)
 in ( LrTable.NT 11, ( result, Expr1left, Expr2right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.ComparativExpr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2("::", Expr1, Expr2))
end)
 in ( LrTable.NT 11, ( result, Expr1left, Expr2right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.ComparativExpr (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Prim2(";", Expr1, Expr2))
end)
 in ( LrTable.NT 11, ( result, Expr1left, Expr2right), rest671)
end
|  ( 44, ( ( _, ( _, _, RSBRAC1right)) :: ( _, ( MlyValue.Number 
Number1, _, _)) :: _ :: ( _, ( MlyValue.Expr Expr1, Expr1left, _)) :: 
rest671)) => let val  result = MlyValue.ComparativExpr (fn _ => let
 val  Expr1 = Expr1 ()
 val  (Number as Number1) = Number1 ()
 in (Item(Number, Expr1))
end)
 in ( LrTable.NT 11, ( result, Expr1left, RSBRAC1right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
ISEQUAL1left, _)) :: rest671)) => let val  result = 
MlyValue.ComparativExpr (fn _ => let val  Expr1 = Expr1 ()
 in (Prim1("ise", Expr1))
end)
 in ( LrTable.NT 11, ( result, ISEQUAL1left, Expr1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
NOT1left, _)) :: rest671)) => let val  result = MlyValue.NotExpr (fn _
 => let val  Expr1 = Expr1 ()
 in (Prim1("!", Expr1))
end)
 in ( LrTable.NT 14, ( result, NOT1left, Expr1right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.Expr Expr1, _, Expr1right)) :: ( _, ( _, 
MINUS1left, _)) :: rest671)) => let val  result = MlyValue.NotExpr (fn
 _ => let val  Expr1 = Expr1 ()
 in (Prim1("-", Expr1))
end)
 in ( LrTable.NT 14, ( result, MINUS1left, Expr1right), rest671)
end
|  ( 48, ( ( _, ( _, END1left, END1right)) :: rest671)) => let val  
result = MlyValue.MatchExpr (fn _ => ([]))
 in ( LrTable.NT 16, ( result, END1left, END1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.MatchExpr MatchExpr1, _, MatchExpr1right))
 :: ( _, ( MlyValue.Expr Expr1, _, _)) :: _ :: ( _, ( MlyValue.KeyExpr
 KeyExpr1, _, _)) :: ( _, ( _, PIPE1left, _)) :: rest671)) => let val 
 result = MlyValue.MatchExpr (fn _ => let val  (KeyExpr as KeyExpr1) =
 KeyExpr1 ()
 val  (Expr as Expr1) = Expr1 ()
 val  (MatchExpr as MatchExpr1) = MatchExpr1 ()
 in ((KeyExpr, Expr)::MatchExpr)
end)
 in ( LrTable.NT 16, ( result, PIPE1left, MatchExpr1right), rest671)

end
|  ( 50, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.UExpr (fn _ => let val  (ID as ID1) = ID1
 ()
 in (Var(ID))
end)
 in ( LrTable.NT 5, ( result, ID1left, ID1right), rest671)
end
|  ( 51, ( ( _, ( _, _, RBRA1right)) :: ( _, ( MlyValue.Prog Prog1, _,
 _)) :: ( _, ( _, LBRA1left, _)) :: rest671)) => let val  result = 
MlyValue.UExpr (fn _ => let val  (Prog as Prog1) = Prog1 ()
 in (Prog)
end)
 in ( LrTable.NT 5, ( result, LBRA1left, RBRA1right), rest671)
end
|  ( 52, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Expr Expr1, _,
 _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = 
MlyValue.UExpr (fn _ => let val  (Expr as Expr1) = Expr1 ()
 in (Expr)
end)
 in ( LrTable.NT 5, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 53, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Comps Comps1,
 _, _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = 
MlyValue.UExpr (fn _ => let val  (Comps as Comps1) = Comps1 ()
 in (List Comps)
end)
 in ( LrTable.NT 5, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.UExpr UExpr2, _, UExpr2right)) :: ( _, ( 
MlyValue.UExpr UExpr1, UExpr1left, _)) :: rest671)) => let val  result
 = MlyValue.AppExpr (fn _ => let val  UExpr1 = UExpr1 ()
 val  UExpr2 = UExpr2 ()
 in (Call(UExpr1, UExpr2))
end)
 in ( LrTable.NT 12, ( result, UExpr1left, UExpr2right), rest671)
end
|  ( 55, ( ( _, ( MlyValue.UExpr UExpr1, _, UExpr1right)) :: ( _, ( 
MlyValue.AppExpr AppExpr1, AppExpr1left, _)) :: rest671)) => let val  
result = MlyValue.AppExpr (fn _ => let val  (AppExpr as AppExpr1) = 
AppExpr1 ()
 val  (UExpr as UExpr1) = UExpr1 ()
 in (Call(AppExpr, UExpr))
end)
 in ( LrTable.NT 12, ( result, AppExpr1left, UExpr1right), rest671)

end
|  ( 56, ( ( _, ( MlyValue.Expr Expr2, _, Expr2right)) :: _ :: ( _, ( 
MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result = 
MlyValue.Comps (fn _ => let val  Expr1 = Expr1 ()
 val  Expr2 = Expr2 ()
 in (Expr1::Expr2::[])
end)
 in ( LrTable.NT 15, ( result, Expr1left, Expr2right), rest671)
end
|  ( 57, ( ( _, ( MlyValue.Comps Comps1, _, Comps1right)) :: _ :: ( _,
 ( MlyValue.Expr Expr1, Expr1left, _)) :: rest671)) => let val  result
 = MlyValue.Comps (fn _ => let val  (Expr as Expr1) = Expr1 ()
 val  (Comps as Comps1) = Comps1 ()
 in (Expr::Comps)
end)
 in ( LrTable.NT 15, ( result, Expr1left, Comps1right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.Expr Expr1, Expr1left, Expr1right)) :: 
rest671)) => let val  result = MlyValue.KeyExpr (fn _ => let val  (
Expr as Expr1) = Expr1 ()
 in (SOME(Expr))
end)
 in ( LrTable.NT 17, ( result, Expr1left, Expr1right), rest671)
end
|  ( 59, ( ( _, ( _, UNDERSCORE1left, UNDERSCORE1right)) :: rest671))
 => let val  result = MlyValue.KeyExpr (fn _ => (NONE))
 in ( LrTable.NT 17, ( result, UNDERSCORE1left, UNDERSCORE1right), 
rest671)
end
|  ( 60, ( ( _, ( _, _, RPAR1right)) :: ( _, ( _, LPAR1left, _)) :: 
rest671)) => let val  result = MlyValue.Args (fn _ => ([]))
 in ( LrTable.NT 18, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 61, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Params Params1
, _, _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result =
 MlyValue.Args (fn _ => let val  (Params as Params1) = Params1 ()
 in (Params)
end)
 in ( LrTable.NT 18, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 62, ( ( _, ( MlyValue.TypedVar TypedVar1, TypedVar1left, 
TypedVar1right)) :: rest671)) => let val  result = MlyValue.Params (fn
 _ => let val  (TypedVar as TypedVar1) = TypedVar1 ()
 in (TypedVar::[])
end)
 in ( LrTable.NT 19, ( result, TypedVar1left, TypedVar1right), rest671
)
end
|  ( 63, ( ( _, ( MlyValue.Params Params1, _, Params1right)) :: _ :: (
 _, ( MlyValue.TypedVar TypedVar1, TypedVar1left, _)) :: rest671)) =>
 let val  result = MlyValue.Params (fn _ => let val  (TypedVar as 
TypedVar1) = TypedVar1 ()
 val  (Params as Params1) = Params1 ()
 in (TypedVar::Params)
end)
 in ( LrTable.NT 19, ( result, TypedVar1left, Params1right), rest671)

end
|  ( 64, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: ( _, ( 
MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result = 
MlyValue.TypedVar (fn _ => let val  (Type as Type1) = Type1 ()
 val  (ID as ID1) = ID1 ()
 in (Type, ID)
end)
 in ( LrTable.NT 20, ( result, Type1left, ID1right), rest671)
end
|  ( 65, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.Type (fn _ => (ListT []))
 in ( LrTable.NT 21, ( result, NIL1left, NIL1right), rest671)
end
|  ( 66, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.Type (fn _ => (BoolT))
 in ( LrTable.NT 21, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 67, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.Type (fn _ => (IntT))
 in ( LrTable.NT 21, ( result, INT1left, INT1right), rest671)
end
|  ( 68, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Type Type1, _,
 _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = 
MlyValue.Type (fn _ => let val  (Type as Type1) = Type1 ()
 in (Type)
end)
 in ( LrTable.NT 21, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 69, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Types Types1,
 _, _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = 
MlyValue.Type (fn _ => let val  (Types as Types1) = Types1 ()
 in (ListT (Types))
end)
 in ( LrTable.NT 21, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 70, ( ( _, ( _, _, RSBRAC1right)) :: ( _, ( MlyValue.Type Type1,
 _, _)) :: ( _, ( _, LSBRAC1left, _)) :: rest671)) => let val  result
 = MlyValue.Type (fn _ => let val  (Type as Type1) = Type1 ()
 in (SeqT(Type))
end)
 in ( LrTable.NT 21, ( result, LSBRAC1left, RSBRAC1right), rest671)

end
|  ( 71, ( ( _, ( MlyValue.Type Type2, _, Type2right)) :: _ :: ( _, ( 
MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result = 
MlyValue.Type (fn _ => let val  Type1 = Type1 ()
 val  Type2 = Type2 ()
 in (FunT(Type1, Type2))
end)
 in ( LrTable.NT 21, ( result, Type1left, Type2right), rest671)
end
|  ( 72, ( ( _, ( MlyValue.Type Type2, _, Type2right)) :: _ :: ( _, ( 
MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result = 
MlyValue.Types (fn _ => let val  Type1 = Type1 ()
 val  Type2 = Type2 ()
 in (Type1::Type2::[])
end)
 in ( LrTable.NT 22, ( result, Type1left, Type2right), rest671)
end
|  ( 73, ( ( _, ( MlyValue.Types Types1, _, Types1right)) :: _ :: ( _,
 ( MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result
 = MlyValue.Types (fn _ => let val  (Type as Type1) = Type1 ()
 val  (Types as Types1) = Types1 ()
 in (Type::Types)
end)
 in ( LrTable.NT 22, ( result, Type1left, Types1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Prog x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : PlcParser_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun REC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun MATCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun WITH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun HD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun TL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun ISEQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun OCOMEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun CCOMEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun MULTI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun DIFF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun LSTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun LSEQTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun ADDTL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun RSBRAC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun LSBRAC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun PIPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROWFUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun UNDERSCORE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun CINT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(
ParserData.MlyValue.CINT (fn () => i),p1,p2))
fun Number (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(
ParserData.MlyValue.Number (fn () => i),p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 48,(
ParserData.MlyValue.VOID,p1,p2))
end
end
