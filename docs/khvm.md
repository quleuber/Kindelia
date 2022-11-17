# KHVM

```txt
Term :=
  // A variable
  Var (name: Name)

  // A cloning operation
  Dup (var0: Name) (var1: Name) (value: Term) (body: Term)

  // A lambda function
  Lam (var: Name) (body: Term)

  // A lambda application
  App (f: Term) (arg: Term)

  // A constructor
  Ctr (name: Name) (args: List Term)

  // A function call
  Func (name: Name) (args: List Term)

  // A native integer
  Num (value: U120)

  // An native integer operation
  Op2 (op: Op) (arg0: Term) (arg1: Term)
```

VS

```txt
VarNode := VarNode (bind: TermNode*)

DupNode := DupNode (left: Option VarNode*) (right: Option VarNode*) (body: TermNode*)

SupNode := SupNode (left: Option TermNode*) (right: Option TermNode*)

LamNode := LamNode (var: Option Var*) (body: TermNode*)

TermNode :=
  Var (lam: LamNode*)
  Dp0 (label: Label) (dup: DupNode*)
  Dp1 (label: Label) (dup: DupNode*)
  Sup (label: Label) (sup: SupNode*)

  Lam (lam: LamNode*)
  App (f: TermNode*) (arg: TermNode*)
  Ctr (name: Name) (args: List TermNode*)
  Fun (name: Name) (args: List TermNode*)

  Num (value: U120)
  Op2 (op: Op) (arg0: TermNode*) (arg1: TermNode*)


where * denotes pointer.
```
