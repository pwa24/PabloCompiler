--Patrick Wang
--
--Compiles Pablo programming language to PabloIR - a simplified intermediate representation based on LLVM IR

import System.Environment
import Data.List
import ParsePablo(parsePablo, Op(..), PabloE(..), PabloS(..), Token(..))

data PVar = V(String)
data PVal = Var(PVar) | C(Int)
data PhiI = Phi(PVar, PVal, String, PVal, String)
data BOp = BAdd | BAnd | BOr | BXor | BShl
data BinI = I BOp(PVar, PVal, PVal) | AssignVal(PVar, PVal)
data CtrlI = UBranch(String) | CBranch(PVar, String, String) | Return
data PabloIR = BasicBlock(String, [PhiI], [BinI], CtrlI)

--VarUpdateTable = (variable, (counter, [(phiBlock, phiVar)]))
type VarEntry = (String, (Int, [(String, String)]))
--Global label count, Block label count, VarUpdateTable
data G = G Int Int [VarEntry]
	deriving Show

instance Show PVar where
	show (V s) = "%" ++ s

instance Show PVal where
	show (Var s) = show s
	show (C int) = show int

instance Show BOp where
	show BOr = "or"
	show BXor = "xor"
	show BAdd = "add"
	show BAnd = "and"
	show BShl = "shl"

instance Show PhiI where
	show (Phi(r, a1, b1, a2, b2)) = 
		(show r) ++ " = " ++ "phi [" ++ (show a1) ++ ", " ++ b1 ++ "], [" ++ (show a2) ++ ", " ++ b2 ++ "]"

instance Show BinI where
	show (I op(r,a,b)) = (show r) ++ " = " ++ (show op) ++ " " ++ (show a) ++ ", " ++ (show b)
	show (AssignVal(r,a)) = (show r) ++ " = " ++ (show a)

instance Show CtrlI where
	show (UBranch(t)) = "br " ++ t
	show (CBranch(a,nz,z)) = "cb " ++ (show a) ++ ", " ++ nz ++ ", " ++ z
	show Return = "ret"
	
instance Show PabloIR where
	show (BasicBlock(label, phiI, binI, ctrlI)) =
		label ++ ":\n" ++
		unlines (map (\x -> "\t" ++ show x) phiI) ++
		unlines (map (\x -> "\t" ++ show x) binI) ++
		"\t" ++ show ctrlI

--Increment gL of G
incGL :: G -> G
incGL (G gL bL vT) = (G (gL+1) bL vT)

--Increment bL of G
incBL :: G -> G
incBL (G gL bL vT) = (G gL (bL+1) vT)

showGL :: G -> String
showGL (G gL bL vT) = show gL

showBL :: G -> String
showBL (G gL bL vT) = show bL

--Use the current global label count as a temp variable
tempVar :: G -> PVar
tempVar (G gL bL vT) = V (show gL)

--Converts Pablo Op (Add, And, Or, Xor) to PabloIR Bop (BAdd, BAnd, BOr, BXor)
getPabloIROp :: Op -> BOp
getPabloIROp Add = BAdd
getPabloIROp And = BAnd
getPabloIROp Or = BOr
getPabloIROp Xor = BXor
getPabloIROp Shl = BShl

pabloEToPVal :: PabloE -> Maybe PVal
pabloEToPVal (All int) = Just (C int)
pabloEToPVal (Variable s) = Just (Var (V s))
pabloEToPVal _ = Nothing

latestVal :: G -> PVal -> PVal
latestVal (G gL bL vT) (Var v) = Var (latestName vT v)
latestVal (G gL bL vT) (C int) = C int

latestNameG :: G -> PVar -> PVar
latestNameG (G gL bL vT) v = latestName vT v

--Update key a from list of (key,value) pairs with value b. If no entry found, create new entry for (a, b)
updateEntry :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
updateEntry key value [] = [(key, value)]
updateEntry key value ((k,v):more)
	| (key == k) = ((k,value):more)
	| otherwise = (k,v):(updateEntry key value more)

--Returns latest variable name from [VarEntry]
latestName :: [VarEntry] -> PVar -> PVar
latestName vT (V v) = 
	case (lookup v vT) of
		Just (ct, _) -> V (show ct ++ v)
		Nothing -> (V v)

--Increments var's counter. If var doesn't exist in [VarEntry], create an entry
incName :: [VarEntry] -> PVar -> [VarEntry]
incName vT (V v) =
	let (key, value) =
		case (lookup v vT) of
			Just (ct, phiMap) -> (ct+1, phiMap)
			Nothing -> (0, [])
	in (updateEntry v (key, value) vT)

--Updates the phiMap for corresponding PVar entry in [VarEntry]
updatePhiMap :: [VarEntry] -> String -> PVar -> [VarEntry]
updatePhiMap vT bName (V v) =
	let (key, value) =
		case (lookup v vT) of
			Just (ct, phiMap) -> (ct, (updateEntry bName (show ct ++ v) phiMap))
			Nothing -> (0, (updateEntry bName ("0" ++ v) []))
	in (updateEntry v (key, value) vT)

--Return invalid block name "?" if last block doesn't exist
getLastBlockName :: [PabloIR] -> String
getLastBlockName [] = "?"
getLastBlockName x = 
	case last x of
		(BasicBlock(bName, _, _, _)) -> bName

generatePabloIR :: [PabloS] -> [PabloIR]
generatePabloIR input =
	case generateBlocks (G 0 0 []) "entry" [] [] Return input of
		(_, [], blocks) -> blocks

--Global vars -> prev block name-> previous binary insts -> [input stmts]
--		-> (Global vars, [output stmts], [output IR])
generateBlocks :: G -> String -> [PhiI] -> [BinI] -> CtrlI-> [PabloS] -> (G, [PabloS], [PabloIR])
--Generate Assignment Blocks
generateBlocks (G gL bL vT) bName prevPhiIs prevBinIs prevCtrlI (Assign(s,ex):more) =
	generateBlocks g' bName prevPhiIs (prevBinIs ++ binIs) prevCtrlI more
	where
		((G gL'' bL'' vT_), binIs) = assignToBinIs (G gL bL vT) var ex
		vT' = (incName vT (V s))
		var = (latestName vT' (V s))
		vT'' = updatePhiMap vT' bName (V s)
		g' = G gL'' bL'' vT''
--Generate While Blocks
generateBlocks g bName prevPhiIs prevBinIs prevCtrlI (While(condEx,whileS) : more) =
	(g5, afterWhileS, [prevBlock] ++ bodyBlocks ++ [condBlock] ++ moreBlocks)
	where
		--set up local vars
		bL = (showBL g)
		brToWCond = UBranch("wCond" ++ bL)

		--finish previous block / entry block
		prevBlock = BasicBlock(bName, prevPhiIs, prevBinIs, brToWCond)

		--generate wBody blocks
		(g2, _, bodyBlocks) = generateBlocks (incBL g) ("wBody" ++ bL) [] [] brToWCond whileS

		--generate wCond block
		condBlock = BasicBlock("wCond" ++ bL, phiIs, condBinIs,
								CBranch(condVar, "wBody" ++ bL, "wEnd" ++ bL))
		(g3, phiIs) = generatePhiIs g2 "wCond" (bName, getLastBlockName bodyBlocks)
		condVar = (tempVar g3)
		(g4, condBinIs) = assignToBinIs (incGL g3) condVar condEx

		--start end block
		(g5, afterWhileS, moreBlocks) = generateBlocks g4 ("wEnd" ++ bL) [] [] prevCtrlI more
--Generate If Blocks
generateBlocks g bName prevPhiIs prevBinIs prevCtrlI (If(condEx, thenS, elseS) : more) = 
	(g6, afterIfS, [prevBlock] ++ thenBlocks ++ elseBlocks ++ moreBlocks)
	where
		--finish previous block / entry block
		bL = (showBL g)
		entryCondVar = (tempVar g)
		prevBlock = BasicBlock(bName, prevPhiIs, prevBinIs ++ entryCondBinIs, 
								CBranch(entryCondVar, "ifThen" ++ bL, "ifElse" ++ bL))
		(g2, entryCondBinIs) = assignToBinIs (incBL (incGL g)) entryCondVar condEx

		--generate ifThen blocks
		brToEnd = UBranch("ifEnd" ++ bL)
		(g3, _, thenBlocks) = generateBlocks g2 ("ifThen" ++ bL) [] [] brToEnd thenS

		--generate ifElse blocks
		(g4, _, elseBlocks) = generateBlocks g3 ("ifElse" ++ bL) [] [] brToEnd elseS
	
		--start end block
		(g5, phiIs) = generatePhiIs g4 "ifEnd" (getLastBlockName thenBlocks, getLastBlockName elseBlocks)
		(g6, afterIfS, moreBlocks) = generateBlocks g5 ("ifEnd" ++ bL) phiIs [] prevCtrlI more
--End of statements. Return Basic Block consisting of prev Instructions
generateBlocks g bName prevPhiIs prevBinIs prevCtrlI [] =
	(g, [], [BasicBlock(bName, prevPhiIs, prevBinIs, prevCtrlI)])


--Generates list of Phi instructions for variables that come from 2 blocks
generatePhiIs :: G -> String -> (String, String) -> (G, [PhiI])
generatePhiIs (G gL bL []) _ _ = (G gL bL [], [])
generatePhiIs (G gL bL vT@(currVT@(v, (ct, phiMap)):moreVT)) cBlock (b1, b2) =
	(G gL bL (currVTToUse ++ nextVT''),
	 phiI ++ nextPhiIs)
	where
		(G _ _ nextVT'', nextPhiIs) = generatePhiIs (G gL bL moreVT) cBlock (b1, b2)

		currVT' = incName [currVT] (V v)	--increment ct		
		var' = latestName currVT' (V v)	--no changes to vT
		currVT'' = updatePhiMap currVT' cBlock (V v)	--changed phiMap
		currVTToUse =
			case (lookup b1 phiMap, lookup b2 phiMap) of
				(Nothing, Nothing) -> [currVT]
				_ -> currVT''

		--Note: changes to vT are on current var, and do not affect phiMap of next var
		phiI = 
			case (lookup b1 phiMap, lookup b2 phiMap) of
				(Just p1, Just p2) -> [Phi(var', Var(V(p1)), b1, Var(V(p2)), b2)]
				(Just p1, Nothing) -> [Phi(var', Var(V(p1)), b1, phiLatestName [currVT] (V v), b2)]
				(Nothing, Just p2) -> [Phi(var', phiLatestName [currVT] (V v), b1, Var(V(p2)), b2)]
				_ -> []

--Returns latest variable name from [VarEntry]. Special case: return 0 if ct is 0
phiLatestName :: [VarEntry] -> PVar -> PVal
phiLatestName vT (V v) = 
	case (lookup v vT) of
		Just (0, _) -> C(0)
		Just (ct, _) -> Var(V (show (ct-1) ++ v))
		Nothing -> Var(V v)

assignToBinIs :: G -> PVar -> PabloE -> (G, [BinI])
assignToBinIs g var (E op(ex1, ex2)) =
	case (pabloEToPVal ex1, pabloEToPVal ex2) of
		(Just val1, Just val2) -> (g, [I bOp(var, (latestVal g val1), (latestVal g val2))])
		(Just val1, Nothing) -> (incGL g', rightBinI ++ [I bOp(var, (latestVal g val1), Var gLVar)])
			where
				gLVar = V (showGL g)
				(g', rightBinI) = assignToBinIs (incGL g) gLVar ex2
		(Nothing, Just val2) -> (incGL g', leftBinI ++ [I bOp(var, Var gLVar, (latestVal g val2))])
			where
				gLVar = V (showGL g)
				(g', leftBinI) = assignToBinIs (incGL g) gLVar ex1
		(Nothing, Nothing) -> (incGL g'', leftBinI ++ rightBinI ++ [I bOp(var, Var gLVar, Var gLVar')])
			where
				gLVar = V (showGL g)
				(g', leftBinI) = assignToBinIs (incGL g) gLVar ex1
				gLVar' = V (showGL g')
				(g'', rightBinI) = assignToBinIs (incGL g') gLVar' ex2
		where bOp = getPabloIROp op
assignToBinIs g var (Advance(ex, int)) = 
	case (pabloEToPVal ex) of
		Just (Var (V x)) -> assignToBinIs g var (E Shl(Variable(x), All(int)))
		Just (C x) -> assignToBinIs g var (E Shl(All(x), All(int)))
		Nothing -> (incGL g'', binI ++ advanceI)
			where
				gLVar = V (showGL g)
				(g', binI) = assignToBinIs (incGL g) gLVar ex
				(g'', advanceI) = assignToBinIs (g') var (Advance(Variable(showGL g), int))
assignToBinIs g var (MatchStar(ex_m, ex_c)) =
	(g_5, binI_m ++ binI_c ++ binI_1 ++ binI_2 ++ binI_3 ++ binI_4)
	where
		(g_c, binI_m) = assignToBinIs (incGL g_m) (V (showGL g_m)) ex_m
		(g_1, binI_c) = assignToBinIs (incGL g_c) (V (showGL g_c)) ex_c
		(g_2, binI_1) = assignToBinIs (incGL g_1) (V (showGL g_1)) (E And(v_m,v_c))
		(g_3, binI_2) = assignToBinIs (incGL g_2) (V (showGL g_2)) (E Add(v_1,v_c))
		(g_4, binI_3) = assignToBinIs (incGL g_3) (V (showGL g_3)) (E Xor(v_2,v_c))
		(g_5, binI_4) = assignToBinIs (incGL g_4) (var) (E Or(v_3,v_m))
		g_m = g
		v_m = Variable(showGL g_m)
		v_c = Variable(showGL g_c)
		v_1 = Variable(showGL g_1)
		v_2 = Variable(showGL g_2)
		v_3 = Variable(showGL g_3)
assignToBinIs g var (Not(ex)) = assignToBinIs g var (E Xor(ex, All(1)))
assignToBinIs g var ex =
	case (pabloEToPVal ex) of
		Just val -> (g, [AssignVal(var, (latestVal g val))])

compilePablo inputPath outputPath = do
	code <- readFile inputPath
	case parsePablo code of
		Just pabloS -> do
			print pabloS
			writeFile outputPath $ unlines (map show (generatePabloIR pabloS))
		_ -> do
			print "Error: Syntax error in pablo code"