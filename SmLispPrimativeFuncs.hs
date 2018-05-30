module SmLispPrimativeFuncs where
import SmDataTypes

apply_symbolp :: SExpression -> (Maybe SExpression)
apply_symbolp (SymAtom a) = Just (SymAtom "T")
apply_symbolp _ = Just (SymAtom "F")


apply_numberp :: SExpression -> (Maybe SExpression)
apply_numberp (NumAtom a) = Just (SymAtom "T")
apply_numberp _ = Just (SymAtom "F")

apply_listp :: SExpression -> (Maybe SExpression)
apply_listp (List a) = Just (SymAtom "T")
apply_listp _ = Just (SymAtom "F")

apply_endp :: SExpression -> (Maybe SExpression)
apply_endp (List []) = Just (SymAtom "T")
apply_endp (List _) = Just (SymAtom "F")
apply_endp _ = Nothing

apply_first :: SExpression -> (Maybe SExpression)
apply_first (List (sexpr:more)) = Just sexpr
apply_first _ = Nothing

apply_rest :: SExpression -> (Maybe SExpression)
apply_rest (List (sexpr:more)) = Just (List more)
apply_rest _ = Nothing

apply_cons :: SExpression -> SExpression -> (Maybe SExpression)
apply_cons a (List b) = Just (List (a:b))
apply_cons a _ = Nothing

apply_eq :: SExpression -> SExpression -> (Maybe SExpression)
apply_eq (SymAtom a) (SymAtom b)
  | a == b = Just (SymAtom "T")
  | otherwise = Just (SymAtom "F")
apply_eq _ _ = Nothing

apply_plus :: SExpression -> SExpression -> (Maybe SExpression)
apply_plus (NumAtom a) (NumAtom b) = (Just (NumAtom (a+b)))
apply_plus _ _ = Nothing

apply_minus :: SExpression -> SExpression -> (Maybe SExpression)
apply_minus (NumAtom a) (NumAtom b) = (Just (NumAtom (a-b)))
apply_minus _ _ = Nothing

apply_times :: SExpression -> SExpression -> (Maybe SExpression)
apply_times (NumAtom a) (NumAtom b) = (Just (NumAtom (a*b)))
apply_times _ _ = Nothing

apply_divide :: SExpression -> SExpression -> (Maybe SExpression)
apply_divide a (NumAtom 0) = Nothing
apply_divide (NumAtom a) (NumAtom b) = (Just (NumAtom (quot a b)))
apply_divide _ _ = Nothing

apply_rem :: SExpression -> SExpression -> (Maybe SExpression)
apply_rem a (NumAtom 0) = Nothing
apply_rem (NumAtom a) (NumAtom b) = (Just (NumAtom (a `mod` b)))
apply_rem _ _ = Nothing

apply_eqp :: SExpression -> SExpression -> (Maybe SExpression)
apply_eqp (NumAtom a) (NumAtom b)
  | a == b = Just (SymAtom "T")
  | otherwise = Just (SymAtom "F")
apply_eqp _ _ = Nothing

apply_lessp :: SExpression -> SExpression -> (Maybe SExpression)
apply_lessp (NumAtom a) (NumAtom b)
  | a < b = Just (SymAtom "T")
  | otherwise = Just (SymAtom "F")
apply_lessp _ _ = Nothing

apply_greaterp :: SExpression -> SExpression -> (Maybe SExpression)
apply_greaterp (NumAtom a) (NumAtom b)
  | a > b = Just (SymAtom "T")
  | otherwise = Just (SymAtom "F")
apply_greaterp _ _ = Nothing

apply_sym_lessp :: SExpression -> SExpression -> (Maybe SExpression)
apply_sym_lessp (SymAtom a) (SymAtom b)
  | a < b = Just (SymAtom "T")
  | otherwise = Just (SymAtom "F")
apply_sym_lessp _ _ = Nothing

apply_explode :: SExpression -> Maybe SExpression
apply_explode (SymAtom word) = Just (List ( [(SymAtom [c]) | c <- word]))
apply_explode _ = Nothing

atom_printer :: SExpression -> String
atom_printer (NumAtom a) = show a
atom_printer (SymAtom a) = a
atom_printer (List []) = ""
atom_printer (List (first:rest)) =  (atom_printer first) ++ (atom_printer (List rest))

apply_implode :: SExpression -> Maybe SExpression
apply_implode (List atoms) = (Just (SymAtom (atom_printer (List atoms))))
apply_implode _ = Nothing

apply_error :: SExpression -> Maybe SExpression
apply_error a = error (atom_printer a)
