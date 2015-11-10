--Parses Pablo Language into a list of PabloS statements

module ParsePablo(parsePablo, Op(..), PabloE(..), PabloS(..), Token(..)) where

import Data.Char

data Op = Add | And | Or | Xor | Shl
   deriving Show

data PabloE = All(Int) | Variable(String) | E Op(PabloE, PabloE) | Not(PabloE) 
			| Advance(PabloE, Int) | MatchStar(PabloE, PabloE)
   deriving Show

data PabloS = Assign(String, PabloE) |  If (PabloE, [PabloS], [PabloS])| While (PabloE, [PabloS])
   deriving Show

data Token = Equal | Colon | ParenLeft | ParenRight | Period | AndOp | OrOp | XorOp | NotOp 
				| IfKw | WhileKw | ElseKw | AdvanceKw | MatchstarKw | IntToken Int | VarToken [Char] 
				| AllZeroes | AllOnes | BadToken [Char]
				deriving Show

lexer :: [Char] -> [Token]

lexer [] = []
lexer (':' : more) = Colon : (lexer more) 
lexer ('=' : more) = Equal : (lexer more) 
lexer ('(' : more) = ParenLeft : (lexer more) 
lexer (')' : more) = ParenRight : (lexer more) 
lexer ('.' : more) = Period : (lexer more) 
lexer ('&' : more) = AndOp : (lexer more) 
lexer ('|' : more) = OrOp : (lexer more) 
lexer ('^' : more) = XorOp : (lexer more) 
lexer ('~' : more) = NotOp : (lexer more) 
lexer (' ' : more) = lexer more
lexer ('\n' : more) = lexer more
lexer ('\t' : more) = lexer more
lexer ('0':'0':'0':'.':'.':'.':more) = AllZeroes: (lexer more)
lexer ('1':'1':'1':'.':'.':'.':more) = AllOnes: (lexer more)
lexer (ch1:more)
   | isDigit ch1  = lexInt (more, ord(ch1) - 48)
   | isLetter ch1 = lexVarOrKW (span (\ch-> isDigit(ch) || isLetter(ch) || ch == '_') (ch1:more))
   | otherwise = (BadToken [ch1]) : (lexer more)

lexVarOrKW("if", afterIf) = IfKw:(lexer afterIf)
lexVarOrKW("else", ':':afterColon) = ElseKw:(lexer afterColon)
lexVarOrKW("while", afterWhile) = WhileKw:(lexer afterWhile)
lexVarOrKW("MatchStar", more) = MatchstarKw:(lexer more)
lexVarOrKW("Advance", more) = AdvanceKw:(lexer more)
lexVarOrKW(s, more) = VarToken s:(lexer more)

lexInt :: ([Char], Int) -> [Token]
lexInt([],numSoFar) = [IntToken numSoFar]
lexInt(d:more,numSoFar) 
  | isDigit(d)    = lexInt(more, numSoFar * 10 + (ord(d) - 48))
  | otherwise     = (IntToken numSoFar) : (lexer (d:more))

parseStmts :: [Token] -> ([PabloS], [Token])

parseStmts [] = ([], [])
parseStmts tokens =
	case parseStmt(tokens) of
		Nothing -> ([], tokens)
		Just (t, tokens1) -> 
		    let (stmts, tokens2) = parseStmts tokens1
		    in (t:stmts, tokens2)

parseStmt :: [Token] -> Maybe (PabloS, [Token])

parseStmt [] = Nothing
parseStmt (VarToken v:Equal:afterEq) =
	case parseExpr(afterEq) of
		Just (e, tokens) -> Just (Assign (v, e), tokens)
		_ -> Nothing

parseStmt (IfKw : afterIf) =
	case parseExpr(afterIf) of 
		Just (e, Colon:afterColon) ->
			case parseStmts(afterColon) of
				(thenStmts, ElseKw:afterElse) ->
					case parseStmts(afterElse) of
						(elseStmts, Period:afterPeriod) -> Just(If(e, thenStmts, elseStmts), afterPeriod)
						_ -> Nothing
				(thenStmts, Period:afterPeriod) -> Just (If(e, thenStmts, []), afterPeriod)
				_ -> Nothing
		_ -> Nothing

parseStmt (WhileKw : afterWhile) =
	case parseExpr(afterWhile) of 
		Just (e, Colon:afterColon) ->
			case parseStmts(afterColon) of
				(repeatStmts, Period:afterPeriod) -> Just(While(e, repeatStmts), afterPeriod)
				_ -> Nothing
		_ -> Nothing

parseStmt _ = Nothing

parseExpr :: [Token] -> Maybe (PabloE, [Token])

parseExpr s =
	case parseTerm(s) of
		Just (term1, afterTerm) -> extendExpr(term1, afterTerm)
		_ -> Nothing

-- extendExpr(e1, []) = Just (e1, [])
extendExpr(e1, (OrOp:afterOr)) =
	case parseTerm(afterOr) of
		Just (t2, afterTerm) -> extendExpr(E Or(e1, t2), afterTerm)
		_ -> Nothing
extendExpr(e1, (XorOp:afterXor)) =
	case parseTerm(afterXor) of
		Just (t2, afterTerm) -> extendExpr(E Xor(e1, t2), afterTerm)
		_ -> Nothing
extendExpr(e1, ts) = Just (e1, ts)

parseTerm :: [Token] -> Maybe (PabloE, [Token])

parseTerm s =
	case parseFactor(s) of
		Just (factor1, afterFactor) -> extendTerm(factor1, afterFactor)
		_ -> Nothing

extendTerm(e1, (AndOp:afterAnd)) =
	case parseFactor(afterAnd) of
		Just (f2, afterFactor) -> extendTerm(E And(e1, f2), afterFactor)
		_ -> Nothing
extendTerm(e1, ts) = Just (e1, ts)



parseFactor :: [Token] -> Maybe (PabloE, [Token])

parseFactor (AllZeroes:more) = Just (All 0, more) 
parseFactor (AllOnes:more) = Just (All 1, more)
parseFactor (VarToken v:more) = Just (Variable v, more)
parseFactor (ParenLeft:afterParenL) =
        case parseExpr(afterParenL) of 
                 Just (e, ParenRight:afterParenR) -> Just (e, afterParenR)
                 _ -> Nothing
parseFactor (MatchstarKw:afterMatchstar) =
        case parseExpr(afterMatchstar) of 
			Just (e, afterExpr) ->       
				case parseExpr(afterExpr) of 
					Just (e2, afterExpr2) -> Just (MatchStar(e, e2), afterExpr2)
					_ -> Nothing
			_ -> Nothing
parseFactor (AdvanceKw:afterAdvance) =
		case parseExpr(afterAdvance) of 
			Just (e, IntToken n:afterInt) -> Just (Advance(e, n), afterInt)
			_ -> Nothing
parseFactor (NotOp:afterNot) = 
		case parseExpr(afterNot) of 
			Just (e, afterExpr) -> Just (Not(e), afterExpr)
			_ -> Nothing
parseFactor _ = Nothing        

parsePablo :: [Char] -> Maybe [PabloS]
parsePablo input = 
        case parseStmts (lexer input) of 
                 (stmts, []) -> Just stmts
                 _ -> Nothing

