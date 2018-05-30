import Data.Char
import Control.Applicative
import Data.List
import SmLispPrimativeFuncs
import SmDataTypes

-- author: Rafiq Dandoo
data Token = Comment [Char] |
             NumToken Int |
             AlphaNumToken [Char] |
             SpecialToken [Char] |
             Lparen | Rparen | Lbrak | Rbrak | Lbrace | Rbrace |
             Equal | Semicolon | Arrow | Quote | Colon deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize "" = []
tokenize (x:xs)
  | (length xs >= 2) && (x == ';' && head xs == ';' && second xs == ';') =
    let t = (span (\x -> x /= '\n') xs)
    in (Comment (x:(fst t))) : tokenize (snd t)
  | isSpace x = tokenize xs
  | x == '(' = Lparen : tokenize xs
  | x == ')' = Rparen : tokenize xs
  | x == '[' = Lbrak : tokenize xs
  | x == ']' = Rbrak : tokenize xs
  | x == '{' = Lbrace : tokenize xs
  | x == '}' = Rbrace : tokenize xs
  | x == '=' = Equal : tokenize xs
  | x == ';' = Semicolon : tokenize xs
  | x == '"' = Quote : tokenize xs
  | x == ':' = Colon : tokenize xs
  | length xs >= 2 && x == '-' &&
    (head xs == '-' && (second xs) == '>') = Arrow : tokenize (drop 2 xs)
  | isDigit x || (length xs >= 1 && x == '-' && (isDigit (head xs))) =
    let t = (span isDigit xs)
    in (NumToken (read (x: fst t) :: Int)) : tokenize (snd t)
  | isAlpha x =
    let t = (span (\x-> isAlphaNum x || x == '-') xs)
    in AlphaNumToken (x: fst t) : tokenize (snd t)
  | elem x "+-*/<>=&|!@#$%?:" =
    let t = (span (\x -> elem x "+-*/<>=&|!@#$%?:") xs)
    in SpecialToken (x: fst t) : tokenize (snd t)
  | otherwise = error ("Unknown token " ++ show x)

second :: [a] -> a
second x = head $ tail x
third :: [a] -> a
third x = head $ tail $ tail x

parseSExpression :: [Token] -> (Maybe SExpression, [Token])
parseSExpression [] = (Nothing, [])
parseSExpression ((NumToken a):xs) = (Just (NumAtom a), xs)
parseSExpression ((AlphaNumToken a):xs) = (Just (SymAtom a), xs)
parseSExpression ((SpecialToken a):xs) = (Just (SymAtom a), xs)
parseSExpression ((Rparen):xs) = (Nothing, xs)
parseSExpression ((Lparen):xs) =
  case parseSExpression xs of
    ((Just sExpr), tokens) -> extendExpression (sExpr, tokens)
    (Nothing, b) -> (Just (List []), b)
parseSExpression (_:xs) = (Nothing, xs)

extendExpression :: (SExpression, [Token]) -> (Maybe SExpression, [Token])
extendExpression (sExpr, tokens) =
    case head tokens of
      (Rparen) -> (Just (List [sExpr]), tail tokens)
      _ -> let (nextSExpr, moreTokens) = parseSExpression tokens in
        case nextSExpr of
          (Just nextSExpr) -> let (extendedSExpr, evenMoreTokens) = extendExpression (nextSExpr, moreTokens) in
               case extendedSExpr of
                 (Just (List sExprList)) -> (Just (List (sExpr:sExprList)), evenMoreTokens)
                 (Nothing) -> (Nothing, moreTokens)
          (Nothing) -> (Nothing, tokens)


parseValue :: [Token] -> (Maybe SmLispExpr, [Token])
parseValue ((Quote):(AlphaNumToken a):(Quote):after) = (Just (SExpr (SymAtom a)), after)
parseValue ((Quote):(SpecialToken a):(Quote):after) = (Just (SExpr (SymAtom a)), after)
parseValue ((AlphaNumToken a):after) = (Just (Variable a), after)
parseValue ((NumToken a):after) = (Just (SExpr (NumAtom a)), after)
parseValue ((Lparen):after) =
  case parseSExpression ((Lparen):after) of
    (Just a, b) -> (Just (SExpr a), b)
    (Nothing, b) -> (Nothing, b)
parseValue tokens = (Nothing, tokens)

parseLocalDef :: [Token] -> (Maybe LocalDef, [Token])
parseLocalDef ((AlphaNumToken a):(Equal):after) =
  case parseSmLispExpr after of
    (Just b, c) -> (Just (a, b), c)
    _ -> (Nothing, after)
parseLocalDef tokens = (Nothing, tokens)

parseLetExpr :: [Token] -> (Maybe SmLispExpr, [Token])
parseLetExpr a =
  let (justlocaldef, tokens) = parseLocalDef a in
    case justlocaldef of
      (Just localdef) ->
        case (lookAhead tokens) of
          -- Last Expression to parse
          (Just Colon, rest) ->
            let (justLetExpr, tokens') = parseSmLispExpr rest in
            case justLetExpr of
              (Just letExpr) ->
                case (lookAhead tokens') of
                  (Just Rbrace, rest') -> (Just (LetExpr [localdef] letExpr), rest')
                  (_, rest') -> (Nothing, rest')
              (Nothing) -> (Nothing, rest)
          -- More expressions to parse
          (Just Semicolon, rest) -> let (justLetExprList, tokensRest) = parseLetExpr rest in
            case justLetExprList of
              (Just (LetExpr deflist letExpr)) -> (Just (LetExpr (localdef:deflist) letExpr), tokensRest)
              (Nothing) -> (Nothing, tokensRest)
          (Nothing, rest) -> (Nothing, rest)
      (Nothing) -> (Nothing, tokens)

parseClause :: [Token] -> (Maybe CondClause, [Token])
parseClause a =
  case parseSmLispExpr a of
    (Just expr1, (Arrow:more)) -> case parseSmLispExpr more of
      (Just expr2, evenMore) -> (Just (expr1, expr2), evenMore)
      (Nothing, evenMore) -> (Nothing, evenMore)
    (Just expr1, more) -> (Nothing, more)
    (Nothing, more) -> (Nothing, more)

parseCondExpr :: [Token] -> (Maybe SmLispExpr, [Token])
parseCondExpr a =
  case parseClause a of
    (Just clause, (Semicolon):more) -> let (clause2, evenMore) = parseCondExpr more in
      case clause2 of
        (Just (CondExpr clauseList)) -> (Just (CondExpr (clause:clauseList)), evenMore)
        (Nothing) -> (Nothing, evenMore)
    (Just clause, (Rbrak):more) -> (Just (CondExpr [clause]), more)
    (Nothing, more) -> (Nothing, more)

parseFuncCall :: [Token] -> (Maybe [SmLispExpr], [Token])
parseFuncCall a =
  case parseSmLispExpr a of
    (Just expr1, (Semicolon):more) -> let (expr2, evenMore) = parseFuncCall more in
      case expr2 of
        (Just exprList) -> (Just (expr1:exprList), evenMore)
        (Nothing) -> (Nothing, evenMore)
    (Just expr1, (Rbrak):more) -> (Just [expr1], more)
    (Just expr, more) -> (Nothing, more)
    (Nothing, more) -> (Nothing, more)

lookAhead :: [Token] -> (Maybe Token, [Token])
lookAhead [] = (Nothing, [])
lookAhead (c:cs) = (Just c, cs)

parseSmLispExpr :: [Token] -> (Maybe SmLispExpr, [Token])
parseSmLispExpr (first:rest) =
  case first of
    (Lbrace) -> parseLetExpr rest
    (Lbrak) -> parseCondExpr rest
    (Lparen) -> parseValue (first:rest)
    (Quote) -> parseValue (first:rest)
    (AlphaNumToken a) ->
      case lookAhead rest of
        (Just (Lbrak), more) -> let (funcCall, evenMore) = parseFuncCall more in
          case funcCall of
            (Just args) -> (Just (FnCall a args), evenMore)
            (Nothing) -> (Nothing, evenMore)
        (Just a, d) -> parseValue (first:rest)
        (Nothing, d) -> (Just (Variable a), d)
    (NumToken a) -> parseValue (first:rest)
    _ -> (Nothing, (first:rest))

isComment :: Token -> Bool
isComment (Comment a) = True
isComment b = False

getComment :: Token -> [Char]
getComment (Comment a) = a

parseParamList :: [Token] -> (Maybe [Identifier], [Token])
parseParamList ((AlphaNumToken a):more) = let (nextToken, evenMore) = lookAhead more in
  case nextToken of
    (Just Rbrak) -> (Just [a], evenMore)
    (Just Semicolon) -> let (justParamList, evenMoreMore) = parseParamList evenMore in
      case justParamList of
        (Just paramList) -> (Just (a:paramList), evenMoreMore)
        (Nothing) -> (Nothing, evenMore)
    _ -> (Nothing, more)
parseParamList tokens = (Nothing, tokens)

parseDef :: [Token] -> (Maybe Definition, [Token])
parseDef tokens = let (comments, (first:rest)) = span isComment tokens in
  case first of
    (AlphaNumToken alphaTok) -> let (nextToken, more) = lookAhead rest in
      case nextToken of
        -- constant defition
        (Just Equal) -> let (expr, evenMore) = parseSmLispExpr more in
          case expr of
            (Just a) -> (Just (ConstantDef (map getComment comments) alphaTok a), evenMore)
            _ -> (Nothing, more)
        -- function Definition
        (Just Lbrak) -> let (justParamList, evenMore) = parseParamList more in
          case justParamList of
            (Just paramList) -> let (nextnextToken, evenMoreMore) = lookAhead evenMore in
              case nextnextToken of
                (Just Equal) -> let (justExpr, lastMore) = parseSmLispExpr evenMoreMore in
                  case justExpr of
                    (Just expr) -> (Just (FunctionDef (map getComment comments) alphaTok paramList expr), lastMore)
                    (Nothing) -> (Nothing, evenMoreMore)
                (Nothing) -> (Nothing, evenMore)
            (Nothing) -> (Nothing, rest)
        _ -> (Nothing, rest)
    _ -> (Nothing, rest)

parseSmLispProgram :: [Token] -> (SmLispProgram, [Token])
parseSmLispProgram [] = ([], [])
parseSmLispProgram tokens = let (justDef, moreTokens) = parseDef tokens in
  case (justDef, moreTokens) of
    (Just def, []) -> ([def], [])
    (Just def, a) -> let (nextDef, evenMoreTokens) = parseSmLispProgram moreTokens in ((def:nextDef), evenMoreTokens)
    (Nothing, a) -> ([], a)


-- Evaluation functions --
sl_eval :: SmLispExpr -> [(Identifier,Maybe Definition)] -> [(Identifier, Maybe SExpression)] -> (Maybe SExpression)
sl_eval slexpr fnEnv valEnv =
  case slexpr of
    (SExpr expr) -> (Just expr)
    (Variable ident) -> let maybeVal = lookup ident valEnv in
      case maybeVal of
        (Just val) -> val
        Nothing -> Nothing
    (FnCall ident slExprList) -> let maybeExpr = sl_apply ident slExprList fnEnv valEnv in
      case maybeExpr of
        (Just applyExpr) -> applyExpr
        (Nothing) -> Nothing
    (CondExpr condClauseList) -> sl_evcond condClauseList fnEnv valEnv
    (LetExpr localDefList expr) -> sl_eval expr fnEnv (extend_local_env fnEnv valEnv localDefList)

extend_local_env :: [(Identifier,Maybe Definition)] -> [(Identifier, Maybe SExpression)] -> [LocalDef] -> [(Identifier, Maybe SExpression)]
extend_local_env fnEnv valEnv [] = valEnv
extend_local_env fnEnv valEnv (firstDef:localDefList) =
  let ident = fst firstDef;
      expr = sl_eval (snd firstDef) fnEnv valEnv;
      newValEnv = extend_env valEnv ident expr
      in extend_local_env fnEnv valEnv localDefList

sl_evalClause :: CondClause -> [(Identifier, Maybe Definition)] -> [(Identifier, Maybe SExpression)] -> Bool
sl_evalClause ((pre, expr)) fnEnv valEnv = let sExpr = sl_eval pre fnEnv valEnv in
  case sExpr of
    (Just (SymAtom "T")) -> True
    _ -> False

sl_evcond :: [CondClause] -> [(Identifier, Maybe Definition)] -> [(Identifier, Maybe SExpression)] -> Maybe SExpression
sl_evcond cList fnEnv valEnv = let condClause = find (\x -> (sl_evalClause x fnEnv valEnv)) cList in
  case condClause of
    (Just (p, expr)) -> sl_eval expr fnEnv valEnv
    _ -> Nothing

sl_evlis :: [SmLispExpr] -> [(Identifier, Maybe Definition)] -> [(Identifier, Maybe SExpression)] -> [Maybe SExpression]
sl_evlis exprList fnEnv valEnv = map (\x -> sl_eval x fnEnv valEnv) exprList

sl_apply :: Identifier -> [SmLispExpr] -> [(Identifier, Maybe Definition)] -> [(Identifier, Maybe SExpression)]-> (Maybe (Maybe SExpression))
sl_apply funName args fnEnv valEnv = let exprList = sl_evlis args fnEnv valEnv in
  case (funName, length exprList) of
    ("first", 1) -> fmap apply_first (head exprList)
    ("rest", 1) -> fmap apply_rest (head exprList)
    ("endp", 1) -> fmap apply_endp (head exprList)
    ("symbolp", 1) -> fmap apply_symbolp (head exprList)
    ("numberp", 1) -> fmap apply_numberp (head exprList)
    ("listp", 1) -> fmap apply_listp (head exprList)
    ("eq", 2) -> liftA2 apply_eq (head exprList) (head $ tail exprList)
    ("cons", 2) -> liftA2 apply_cons (head exprList) (head $ tail exprList)
    ("plus", 2) -> liftA2 apply_plus (head exprList) (head $ tail exprList)
    ("minus", 2) -> liftA2 apply_minus (head exprList) (head $ tail exprList)
    ("times", 2) -> liftA2 apply_times (head exprList) (head $ tail exprList)
    ("divide", 2) -> liftA2 apply_divide (head exprList) (head $ tail exprList)
    ("rem", 2) -> liftA2 apply_rem (head exprList) (head $ tail exprList)
    ("eqp", 2) -> liftA2 apply_eqp (head exprList) (head $ tail exprList)
    ("lessp", 2) -> liftA2 apply_lessp (head exprList) (head $ tail exprList)
    ("greaterp", 2) -> liftA2 apply_greaterp (head exprList) (head $ tail exprList)
    ("sym-lessp", 2) -> liftA2 apply_sym_lessp (head exprList) (head $ tail exprList)
    ("explode", 1) -> fmap apply_explode (head exprList)
    ("implode", 1) -> fmap apply_implode (head exprList)
    ("error", 1) -> fmap apply_error (head exprList)
    otherwise -> let function = lookup funName fnEnv in
      case function of
        (Just (Just (FunctionDef comments funcId paramList funcBody))) -> let sameArgsNum = (length paramList) == (length exprList) in
          case sameArgsNum of
            True -> Just (sl_eval funcBody fnEnv (add_associations valEnv paramList exprList))
            False -> Nothing
        (Just (Nothing)) -> Nothing --error ("Function " ++ funName ++ " defined, but could not parse it properly")
        (Nothing) -> Nothing -- error ("function " ++ funName ++ " not defined")

interpret :: SmLispProgram -> SmLispExpr -> Maybe SExpression
interpret program expr = setup_envs_then_eval [] [("T", (Just (SymAtom "T"))), ("F", (Just (SymAtom "F"))), ("otherwise", (Just (SymAtom "T")))] program expr

setup_envs_then_eval :: [(Identifier, Maybe Definition)] -> [(Identifier, Maybe SExpression)] -> SmLispProgram -> SmLispExpr -> Maybe SExpression
setup_envs_then_eval fnEnv valEnv definitonList expression = let (fullFnEnv, fullValEnv) = foldl extendDefs (fnEnv, valEnv) definitonList in sl_eval expression fullFnEnv fullValEnv

extendDefs :: ([(Identifier, Maybe Definition)], [(Identifier, Maybe SExpression)]) -> Definition -> ([(Identifier, Maybe Definition)], [(Identifier, Maybe SExpression)])
extendDefs (fnEnv, valEnv) def =
  case def of
    (ConstantDef comments identifier constExpr) -> (fnEnv, (extend_env valEnv identifier (sl_eval constExpr fnEnv valEnv)))
    (FunctionDef comments identifier paramlist funcExpr)  -> ((extend_fn fnEnv identifier (Just def)), valEnv)

add_associations :: [(Identifier, Maybe SExpression)] -> [Identifier] -> [Maybe SExpression] -> [(Identifier, Maybe SExpression)]
add_associations valEnv [] [] = valEnv
add_associations valEnv (firstParam:paramList) (firstArg:argList) = add_associations (extend_env valEnv firstParam firstArg) paramList argList

extend_fn :: [(Identifier, Maybe Definition)] -> Identifier -> (Maybe Definition) -> [(Identifier, Maybe Definition)]
extend_fn [] ident expr = [(ident, expr)]
extend_fn envs ident expr = (ident, expr) : envs

extend_env :: [(Identifier, Maybe SExpression)] -> Identifier -> (Maybe SExpression) -> [(Identifier, Maybe SExpression)]
extend_env [] ident expr = [(ident, expr)]
extend_env envs ident expr = (ident, expr) : envs
