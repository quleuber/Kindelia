use kindelia_core::common::{Name, U120};
use kindelia_core::hvm::{Oper, Term, init_name_map, RuntimeError};
use kindelia_core::parser;
use kindelia_core::util::NameMap;

/*
VarNode = VarNode (lam: LamNode*)
DpxNode = DpxNode (label: Label) (side: bool) (dup: DupNode*)

LamNode := LamNode (var: Option VarNode*) (body: TermNode)

DupNode := DupNode (left: Option DpxNode*) (right: Option DpxNode*) (body: TermNode)

SupNode := SupNode (left: Option TermNode) (right: Option TermNode)

TermNode :=
  Var (var: VarNode*)
  Dpx (dup: DpxNode*)

  Sup (label: Label) (sup: SupNode*)

  Lam (lam: LamNode*)
  App (f: TermNode) (arg: TermNode)
  Ctr (name: Name) (args: List TermNode)
  Fun (name: Name) (args: List TermNode)

  Num (value: U120)
  Op2 (op: Op) (arg0: TermNode) (arg1: TermNode)
*/

use std::cell::RefCell;
use std::rc::Rc;

type Label = u64;
type RCell<T> = Rc<RefCell<T>>;

struct VarNode {
  lam: RCell<LamNode>,
}

struct DpxNode {
  label: Label,
  side: bool,
  dup: RCell<DupNode>,
}

struct LamNode {
  var: Option<Rc<VarNode>>,
  body: TermNode,
}

struct DupNode {
  left: Option<Rc<DpxNode>>,
  right: Option<Rc<DpxNode>>,
  expr: TermNode,
}

struct SupNode {
  left: Option<TermNode>,
  right: Option<TermNode>,
}

enum TermNode {
  Var { var: Rc<VarNode> },
  Dpx { dpx: Rc<DpxNode> },

  Sup { label: Label, sup: Rc<SupNode> },

  Lam { lam: RCell<LamNode> },
  App { f: Box<TermNode>, arg: Box<TermNode> },

  Ctr { name: Name, args: Vec<TermNode> },
  Fun { name: Name, args: Vec<TermNode> },

  Num { value: U120 },
  Op2 { op: Oper, arg0: Box<TermNode>, arg1: Box<TermNode> },
}

fn rc<T>(x: T) -> Rc<T> {
  Rc::new(x)
}

fn rcell<T>(x: T) -> Rc<RefCell<T>> {
  Rc::new(RefCell::new(x))
}

fn placeholder() -> TermNode {
  TermNode::Num { value: U120::MAX }
}

fn create_term(term: &Term) -> Result<TermNode, RuntimeError> {
  let mut vars_map: NameMap<Vec<TermNode>> = init_name_map();
  create_term_go(&mut vars_map, term)
}

fn create_term_go(vars_map: &mut NameMap<Vec<TermNode>>, term: &Term) -> Result<TermNode, RuntimeError> {
  let mut labels = 1;
  let mut fresh_label = move || { labels += 1; labels - 1 };

  fn consume(vars_map: &mut NameMap<Vec<TermNode>>, name: &Name) -> Option<TermNode> {
    let stack = vars_map.get_mut(name)?;
    stack.pop()
  }

  fn bind_var(vars_map: &mut NameMap<Vec<TermNode>>, name: Name, lam_node: &RCell<LamNode>) {
    if name != Name::NONE {
      // The Var node
      let var_node = rc(VarNode { lam: lam_node.clone() });

      // Link Var node on Lam node
      let mut lam_node = lam_node.borrow_mut();
      lam_node.var = Some(var_node.clone());

      // Build the Var term itself and bind to the variable name on a new scope
      let term = TermNode::Var { var: var_node };
      let stack = vars_map.entry(name).or_default();
      stack.push(term);
    }
  }

  fn bind_dp(vars_map: &mut NameMap<Vec<TermNode>>, name: Name, label: Label, side: bool, dup_node: &RCell<DupNode>) {
    if name != Name::NONE {
      // The Dpx node
      let dpx_node = rc(DpxNode { label, side, dup: dup_node.clone() });

      // Link Dpx node on corresponding side of DupNode
      let mut dup_node = dup_node.borrow_mut();
      let dup_side = if !side { &mut dup_node.left } else { &mut dup_node.right };
      *dup_side = Some(dpx_node.clone());

      // Build the Dpx term itself and bind to the variable name on a new scope
      let term = TermNode::Dpx { dpx: dpx_node };
      let stack = vars_map.entry(name).or_default();
      stack.push(term);
    }
  }

  match term {
    Term::Var { name } => {
      consume(vars_map, name).ok_or(RuntimeError::UnboundVar { name: *name })
    }
    Term::Dup { nam0, nam1, expr, body } => {
      let label = fresh_label();
      let expr = create_term_go(vars_map, expr)?;
      let dup_node = rcell(DupNode { left: None, right: None, expr });
      bind_dp(vars_map, *nam0, label, false, &dup_node);
      bind_dp(vars_map, *nam1, label, true, &dup_node);
      create_term_go(vars_map, body)
    },
    Term::Lam { name, body } => {
      let lam_node = rcell(LamNode { var: None, body: placeholder() });
      bind_var(vars_map, *name, &lam_node);
      let body = create_term_go(vars_map, body)?;
      lam_node.borrow_mut().body = body;
      Ok(TermNode::Lam { lam: lam_node })
    },
    Term::App { func, argm } => {
      let f = Box::new(create_term_go(vars_map, func)?);
      let arg =  Box::new(create_term_go(vars_map, argm)?);
      Ok(TermNode::App { f, arg })
    },
    Term::Ctr { name, args } => {
      let args: Result<Vec<_>, RuntimeError> = args.iter().map(|arg| {
        create_term_go(vars_map, arg)
      }).collect();
      let args = args?;
      Ok(TermNode::Ctr { name: *name, args })
    },
    Term::Fun { name, args } => {
      let args: Result<Vec<_>, RuntimeError> = args.iter().map(|arg| {
        create_term_go(vars_map, arg)
      }).collect();
      let args = args?;
      Ok(TermNode::Fun { name: *name, args })
    }
    Term::Num { numb } => {
      Ok(TermNode::Num { value: *numb })
    },
    Term::Op2 { oper, val0, val1 } => {
      let arg0 = Box::new(create_term_go(vars_map, val0)?);
      let arg1 = Box::new(create_term_go(vars_map, val1)?);
      Ok(TermNode::Op2 { op: *oper, arg0, arg1})
    },
  }
}

fn readback(_node: &TermNode) -> Term {
  todo!()
}

fn reduce(_node: TermNode) -> TermNode {
  todo!()
}

fn main() -> Result<(), String> {
  let code = "
    dup a b = λx(λy(Pair x y))
    (Pair a b)
  ";
  let term = parser::parse_term(code).map_err(|e| format!("{:?}", e))?;
  let (rest, term) = term;
  assert_eq!(rest, "");
  let node = create_term(&term).map_err(|e| format!("{:?}", e))?;

  Ok(())
}
