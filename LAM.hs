module LAM where
import ParserLambda

remove_char :: Char -> [Char] -> [Char]
remove_char c [] = []
remove_char c s = 
	if c == head(s)
		then tail(s)
	else head s : remove_char c (tail s)

find_rename :: TLam -> TLam
find_rename (App (Abs x t) (Var y)) =
	if (x /= y) && (elem y (linked_vars t))
		then (App (Abs x (rename_link y t (name_new_var t))) (Var y))
	else (App (Abs x t) (Var y))

rename_link :: Char -> TLam -> Char -> TLam
rename_link x (Var y) z = if (x == y)
	then (Var z)
	else (Var y)
rename_link x (Abs y t) z = if (x == y)
	then (Abs z (rename_link x t z))
	else (Abs y (rename_link x t z))
rename_link x (App t1 t2) z = (App (rename_link x t1 z) (rename_link x t2 z))

name_new_var :: TLam -> Char
name_new_var t = head("abcdefghijklmnopqrstuvwxyz" \\ freeVars(t))

linked_vars :: TLam -> [Char]
linked_vars (Var x) = []
linked_vars (Abs x t) = [x] ++ linked_vars(t)
linked_vars (App t1 t2) = linked_vars(t1) ++ linked_vars(t2)

freeVars :: TLam -> [Char]
freeVars (Var x)     = [x]
freeVars (Abs x t)   = remove_char x (freeVars t)
freeVars (App t1 t2) = freeVars t1 ++ freeVars t2

--semantica
beta_redex :: TLam -> TLam
beta_redex (App (Abs x t1) t2) = subs x (beta_redex(t2)) t1
beta_redex (App t1 t2) = (App (beta_redex(t1)) (beta_redex(t2)))
beta_redex (Var x) = (Var x)
beta_redex (Abs x t) = (Abs x (beta_redex(t)))

subs :: Char -> TLam -> TLam -> TLam
subs x s (Var y) = if (x == y) then s else (Var y)
subs x s (Abs y t1) =
	if (x /= y)
		then (Abs y (subs x s t1))
	else (Abs y t1)
subs x s (App t1 t2) = App (subs x s t1) (subs x s t2)

to_string :: TLam -> [Char]
to_string (Var x) = [x]
to_string (Abs x t) = "( lam " ++ [x] ++ " . " ++ to_string(t) ++ " )"
to_srting (App t1 t2) = "( " ++ to_string(t1) ++ " " ++ to_string(t2) ++ " )"