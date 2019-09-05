/// The MIT License (MIT)
/// Copyright (c) 2015-Mar 2016 Kevin Thompson @kthompson
/// Copyright (c) 2016 Bazinga Technologies Inc

module FSharp.Data.GraphQL.Parser

open FSharp.Data.GraphQL.Ast
open System
open FParsec

[<AutoOpen>]
module internal Internal =

  // 2.1.7 Ignored tokens
  let ignored = 
    // 2.1.2 WhiteSpace 
    //    Horizontal Tab (U+0009) | Space (U+0020) 
    let whiteSpace = skipAnyOf [|'\u0009'; '\u000B'; '\u000C'; '\u0020'; '\u00A0'|]

    // 2.1.3 LineTerminator
    //   New Line (U+000A)
    //   Carriage Return (U+000D)New Line (U+000A) | (U+000D)New Line (U+000A)
    let lineTerminators = skipAnyOf [|'\u000A'; '\u000D'; '\u2028'; '\u2029'|]
    
    // 2.1.4 CommentChar
    //  SourceCharacter but not LineTerminator
    let comments  = pchar '#' >>. skipManyTill anyChar (lineTerminators <|>  eof) 

    // 2.1.5 Comma (Insignificant)
    let comma = skipChar ',' 
  
    whiteSpace <|> lineTerminators <|> comments <|> comma <?> "Ignored"

  // ignore zero or more whitespaces
  let whitespaces = many ignored |>> ignore
  

  // lexical token are indivisible terminal symbols 
  let token p = p .>> notFollowedBy (letter <|> digit <|> pchar '_')
  let stoken = pstring >> token
  let token_ws p = token p .>> whitespaces
  let stoken_ws = pstring >> token_ws
  let ctoken_ws = pchar >> token_ws

  // helpers to format parser results
  let someOrEmpty = function | Some lst -> lst | None -> []
  let charsToString = Array.ofList >> String
  
  // parse one items between two char punctuators
  let betweenChars left right p =
   (pchar left .>> whitespaces) >>. p .>> (whitespaces .>> pchar right)

  // parse one or more items ignoring inconsequential whitespace
  let betweenCharsMany1 left right p =
    between (pchar left .>> whitespaces) (pchar right) (sepEndBy1 p whitespaces)

  // parse zero or more items ignoring inconsequential whitespace
  let betweenCharsMany left right p =
    between (pchar left .>> whitespaces) (pchar right) (sepEndBy p whitespaces)

  // run the key, seperator, then value parsers while ignoring inconsequential whitespaces
  let pairBetween seperator key value =
    (key .>> whitespaces) .>>. ((pchar seperator .>> whitespaces) >>. value)


  // 2.9 Input Values
  // forward ref since value is recursive in the case of lists and objects
  let inputValue, inputValueRef = createParserForwardedToRef()


  // 2.1.9 Names (ASCII only)
  //   /[_A-Za-z][_0-9A-Za-z]*/
  let name = 
    let isIdentifierFirstChar c = isAsciiLetter c || c = '_'
    let isIdentifierChar c = isAsciiLetter c || isDigit c || c = '_'
    many1Satisfy2 isIdentifierFirstChar isIdentifierChar

  // 2.9.4 StringValue
  let stringValue =
    let escapedCharacter =
      let escaped = anyOf [| '"'; '\\'; '/'; 'b'; 'f'; 'n'; 'r'; 't' |]
                        |>> function | 'b' -> '\b' | 'f' -> '\u000C' | 'n' -> '\n' 
                                     | 'r' -> '\r' | 't' -> '\t' | c -> c 
      let unicode = 
        pchar 'u' >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
          let hex2int c = (int c &&& 15) + (int c >>> 6)*9 
          (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0 |> char)
      pchar '\\' >>. (escaped <|> unicode)
  
    let normalCharacter = noneOf [|'\u000A';'\u000D';'\u2028';'\u2029';'"';'\''|]
    let quote =  pchar '"'
    between quote quote (manyChars (normalCharacter <|> escapedCharacter))


  // 2.9.3 Boolean value
  //  one of true false
  let booleanValue =
    choice [ stoken "true" >>% true
             stoken "false" >>% false ]


  // Integer Part 
  //   (NegativeSign opt) 0
  //   (NegativeSign opt) NonZeroDigit (Digit list opt)
  let integerPart = 
    let negativeSign = pchar '-'
    let nonZeroDigit = anyOf [|'1';'2';'3';'4';'5';'6';'7';'8';'9'|]
    let zero = pchar '0'
    let zeroInteger =  opt negativeSign >>. zero >>% "0"
    let nonZeroInteger =  opt negativeSign .>>. (many1Chars2 nonZeroDigit digit) 
                          |>> function | (Some _, v) -> "-" + v
                                       | (None, v) -> v
    (attempt zeroInteger) <|> nonZeroInteger


  // 2.9.1 IntValue 
  //   IntegerPart
  let integerValue = integerPart |>> int64


  // 2.9.2 FloatValue
  //    IntegerPart FractionalPart
  //    IntegerPart ExponentPart
  //    IntegerPart FractionalPart ExponentPart
  let floatValue =
    let exponentPart = 
      let sign = pchar '+' <|> pchar '-'
      let exponentIndicator = pchar 'e' <|> pchar 'E'
      
      pipe3 exponentIndicator (opt sign) (many1 digit) 
        (fun exp sign digits ->
          charsToString (match sign with
                         | Some sign -> exp::sign::digits
                         | None -> exp::digits ))
  
    let fractionPart = 
      pchar '.' .>>. (many1 digit) 
      |>> (fun (dot, digits) -> charsToString(dot::digits))
    
    choice [
      integerPart .>>. (exponentPart <|> fractionPart) |>> fun(p1, p2)-> p1 + p2
      pipe3 integerPart fractionPart exponentPart 
        (fun integer fraction exponent -> integer + fraction + exponent ) ] |>> float

  // 2.9.5 Null Value
  let nullValue = stoken "null" >>% NullValue

  // 2.9.6 Enum Value
  //   Name but not true or false or null
  //   (boolean parser is run first)
  let enumValue = name


  // 2.10 Variable
  //   $ Name
  let variable = pchar '$' >>. name 


  // 2.9.8 Input Object Values
  let inputObject =
    betweenCharsMany '{' '}' (pairBetween ':' name inputValue <?> "ObjectField") 
    |>> Map.ofList 
  
  // 2.9.7 List Value
  let listValue =
    betweenCharsMany '[' ']' (token_ws inputValue <?> "Value") 
     

  // 2.9 Value 
  //   Variable|IntValue|FloatValue|StringValue|
  //   BooleanValue|NullValue|EnumValue|ListValue|ObjectValue
  inputValueRef :=
    choice [ variable |>> Variable <?> "Variable"
             (attempt floatValue) |>> FloatValue <?> "Float"
             integerValue |>> IntValue <?> "Integer"
             stringValue |>> StringValue <?> "String"
             (attempt booleanValue) |>> BooleanValue <?> "Boolean"
             nullValue
             enumValue |>> EnumValue  <?> "Enum"
             inputObject |>> ObjectValue  <?> "InputObject"
             listValue |>> ListValue <?> "ListValue" ]  


  // 2.6 Arguments
  //   Argument list
  let arguments = 
    // Argument
    //   Name:Value
    let argument = 
      pairBetween ':' name inputValue
      |>> fun (name, value) -> { Argument.Name = name; Value = value } 
      <?> "Argument"
    betweenCharsMany '(' ')' argument <?> "Arguments"


  // 2.1.2 Directives
  //   Directive list
  let directives = 
    // Directive 
    //    @ Name (Arguments opt)
    let directive =
      pchar '@' >>. (name .>> whitespaces) .>>. (opt arguments) 
      |>> fun (name, args) -> { Directive.Name = name; Arguments = someOrEmpty args}
      <?> "Directive"
    sepEndBy directive whitespaces <?> "Directives"


  // 2.11 Type 
  //   |NamedType|ListType|NonNullType
  let inputType, inputTypeRef = createParserForwardedToRef()
  let namedType = name |>> NamedType <?> "NamedType"
  let listType = 
    betweenChars '[' ']' inputType
    |>> ListType  <?> "ListType"
  let nonNullType = 
    (listType <|> namedType) .>> pchar '!' 
    |>> NonNullType <?> "NonNullType"
  inputTypeRef :=
    choice [ attempt nonNullType
             namedType
             listType ]


  // 2.4 Selection Sets
  //  Selection list
  let selection, selectionRef = createParserForwardedToRef()
  let selectionSet, selectionSetRef = createParserForwardedToRef()
 

  // 2.5 Fields
  //   (Alias opt) Name (Arguments opt) (Directives opt) (SelectionSet opt)
  let field = 
    let alias = token_ws name .>> pchar ':' .>> whitespaces
    pipe5 (opt(attempt alias)) (token_ws name) (opt(token_ws arguments)) (opt directives) (opt selectionSet)
      (fun oalias name oargs directives oselection ->
         (Field { Alias = oalias; Name = name; Arguments = someOrEmpty oargs;
                  Directives = someOrEmpty directives; SelectionSet = match oselection with None -> [] | Some s -> s }))
    <?> "Field"

  
  // 2.8 Fragments
  //  FragmentSpread|FragmentDefinition|FragmentName
  let selectionFragment =
    let inlineFragment =
      pipe3 (opt(stoken_ws "on" >>. token_ws name)) (opt(token_ws directives)) selectionSet
        (fun typeCondition directives selectionSet -> 
          { FragmentDefinition.Name = None
            Directives = someOrEmpty directives
            SelectionSet = selectionSet 
            TypeCondition = typeCondition })
      |>> InlineFragment <?> "InlineFragment"

    let fragmentSpread = 
      token_ws name .>>. opt directives
      |>> fun (name, directives) -> { FragmentSpread.Name = name; Directives = someOrEmpty directives }
      |>> FragmentSpread <?> "FragmentSpread"

    pstring "..." .>> whitespaces >>. (inlineFragment <|> fragmentSpread)  <?> "Fragment"

  selectionRef :=
    field <|> selectionFragment  <?> "Selection"

  selectionSetRef := 
    betweenCharsMany1 '{' '}' selection <?> "SelectionSet"


  // 2.3 Operations
  //    OperationDefinition List
  let definitions =
    let operationType = 
      (stoken_ws "query" >>% Query) <|> (stoken_ws "mutation" >>% Mutation) <|> (stoken_ws "subscription" >>% Subscription)
      
    let operationDefinition = 
      let variableDefinition =
        pipe2 (pairBetween ':' variable inputType) (whitespaces >>. opt((ctoken_ws '=') >>. inputValue))
          (fun (variableName, variableType) defaultVal ->
              { VariableName = variableName; Type = variableType
                DefaultValue = defaultVal })
      let variableDefinitions =
        betweenCharsMany '(' ')' variableDefinition
      
      pipe5 operationType (opt(token_ws name)) (opt(token_ws variableDefinitions)) (opt(token_ws directives)) (token_ws selectionSet)
        (fun  otype name ovars directives selection ->
          { OperationType = otype; Name = name; SelectionSet = selection
            VariableDefinitions = someOrEmpty ovars; Directives = someOrEmpty directives })
    
    // Short hand format where operation defaults to a Query
    let shortHandQueryDefinition =
      selectionSet |>> (fun selectionSet -> 
                         { OperationType = Query; SelectionSet = selectionSet; 
                           VariableDefinitions = []; Directives = []; Name = None })
    
    // SelectionSet |
    // (OperationType (Name opt) (VariableDefinitions opt) (Directives opt) SelectionSet) 
    let operationDefinition =
      shortHandQueryDefinition <|> operationDefinition |>> OperationDefinition  
    
    let fragmentDefinition =
      pipe4 ((stoken_ws "fragment") >>. token_ws name .>> (stoken_ws "on")) (token_ws name) directives selectionSet
        (fun name typeCond directives selSet ->
          FragmentDefinition 
            { Name = Some name;  Directives = directives; SelectionSet = selSet
              TypeCondition = if String.IsNullOrEmpty typeCond then None else Some typeCond })
      
    // GraphQL documents can contain zero definitions
    sepEndBy (operationDefinition <|> fragmentDefinition) whitespaces


  // 2.2 Document
  //   DefinitionList
  let documents = 
    whitespaces >>. definitions .>> (skipMany ignored <|> eof)
    |>> (fun definitions -> { Document.Definitions = definitions })


/// Parses a GraphQL Document. Throws exception on invalid formats.
let parse query =
  match run documents query with
  | Success(result, _, _) -> result
  | Failure(errorMsg, _, _) -> raise (System.FormatException(errorMsg))