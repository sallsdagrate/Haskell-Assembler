> module Ex3FunctionsCodeGenerator where
> import Ex3FunctionsTypes
> import Data.List

-----------------------------------------------------------
Solution for Compilers exercise 3

Paul Kelly  Imperial College London  2009
-----------------------------------------------------------

Fill in the gaps...

Part (1): translate function declaration

> transFunction :: Function -> [Instr]
> transFunction (Defun fname paramname body) = [Define fname] ++ (transExp body (freeRegs(allRegs))) ++ [Ret]

> freeRegs :: [Register] -> [Register]
> freeRegs (D0 : D1 : rest) = (D0 : rest)

Part (2): saving registers

> saveRegs :: [Register] -> [Instr]
> saveRegs regsNotInUse
>  = saveRegsHelp (allRegs \\ regsNotInUse)

> saveRegsHelp ::  [Register] -> [Instr]
> saveRegsHelp [] = []
> saveRegsHelp (r:rs) = (saveRegsHelp rs) ++ [Mov (Reg r) Push] 
        

> restoreRegs :: [Register] -> [Instr]
> restoreRegs regsNotInUse
>  = restoreRegsHelp (allRegs \\ regsNotInUse)

> restoreRegsHelp ::  [Register] -> [Instr]
> restoreRegsHelp [] = []
> restoreRegsHelp (r:rs) = (Mov Pop (Reg r)) : restoreRegsHelp rs

Part (3): translate expression (ie function body, perhaps including
function calls)

> transExp :: Exp -> [Register] -> [Instr]
> 
> transExp (Const x) (dst:rest) = [Mov (ImmNum x)(Reg dst)]
> transExp (Var _) (dst:rest) = [Mov (Reg paramReg) (Reg dst)]
> transExp (Minus e1 e2) regs@(dst:d:rest)
>   | (weight e1) >= (weight e2) = (transExp e1 regs) ++ (transExp e2 (d:rest)) ++ [Sub (Reg d) (Reg dst)]
>   | otherwise = (transExp e2 (d:dst:regs)) ++ (transExp e1 (dst:rest)) ++ [Sub (Reg d) (Reg dst)]
> transExp (Apply s e) regs@(dst:rest)= (saveRegs regs) ++ (transExp e regs) ++ (moveRegs (dst, paramReg)) ++ [Jsr s] ++ (moveRegs (resultReg, dst)) ++ (restoreRegs regs)

> moveRegs :: (Register, Register) -> [Instr]
> moveRegs (x, y)
>   | (x == y) = []
>   | otherwise = [Mov (Reg x) (Reg y)]    

> weight :: Exp -> Int
> weight (Const _) = 1
> weight (Var _) = 1
> weight (Apply _ e) = weight e
> weight (Plus e1 e2) = min cost1 cost2
>    where
>        cost1 = max (weight e1) ((weight e2) + 1)
>        cost2 = max (weight e1 + 1) ((weight e2))
 
> weight (Minus e1 e2) = min cost1 cost2
>    where
>        cost1 = max (weight e1) ((weight e2) + 1)
>        cost2 = max (weight e1 + 1) ((weight e2))
